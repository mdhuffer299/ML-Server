setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

data_dir <- "C:/Users/mhuffer/Documents/GitHub/ML-Server/data"

file_list <- list.files(data_dir, pattern = "*.csv")
files <- paste0(data_dir,"/",file_list)

output_dir <- "C:/Users/mhuffer/Documents/GitHub/ML-Server"
hourlyEnergyXdf <- file.path(output_dir, "hourlyEnergy.xdf")


# Not the optimal way to create multiple data frames.  They should be represented in a single list using lapply
#for (i in file_list) {
#    print(i)
#    assign(i, read.csv(paste0(data_dir, "/", i), header = TRUE, sep = ","))
#}

energy_files <- lapply(files, read.csv)

names(energy_files) <- c("AEP_hourly", "COMED_hourly", "DAYTON_hourly", "DEOK_hourly", "DOM_hourly", "DUQ_hourly", "EKPC_hourly", "FE_hourly", "NI_hourly", "PJM_hourly_est", "PJM_Load_hourly", "PJME_hourly", "PJMW_hourly")

df_list <- list()
df_list <- list(energy_files$AEP_hourly, energy_files$COMED_hourly, energy_files$DAYTON_hourly, energy_files$DEOK_hourly, energy_files$DOM_hourly, energy_files$DUQ_hourly, energy_files$EKPC_hourly, energy_files$FE_hourly, energy_files$NI_hourly, energy_files$PJME_hourly, energy_files$PJMW_hourly)

change_names <- function(dataframe) {
    names(dataframe) <- c("date", "MW")
    return(dataframe)
}

df_list <- lapply(df_list, change_names)
names(df_list) <- c("AEP_hourly", "COMED_hourly", "DAYTON_hourly", "DEOK_hourly", "DOM_hourly", "DUQ_hourly", "EKPC_hourly", "FE_hourly", "NI_hourly", "PJME_hourly", "PJMW_hourly")

# this will be replaced with a function that dynamically applies the src_system_cd using lapply
df_list$AEP_hourly$src_system_cd <- "AEP_hourly"
df_list$COMED_hourly$src_system_cd <- "COMED_hourly"
df_list$DAYTON_hourly$src_system_cd <- "DAYTON_hourly"
df_list$DEOK_hourly$src_system_cd <- "DEOK_hourly"
df_list$DOM_hourly$src_system_cd <- "DOM_hourly"
df_list$DUQ_hourly$src_system_cd <- "DUQ_hourly"
df_list$EKPC_hourly$src_system_cd <- "EKPC_hourly"
df_list$FE_hourly$src_system_cd <- "FE_hourly"
df_list$NI_hourly$src_system_cd <- "NI_hourly"
df_list$PJME_hourly$src_system_cd <- "PJME_hourly"
df_list$PJMW_hourly$src_system_cd <- "PJMW_hourly"


# Create complete data frame
energy_df <- do.call("rbind",df_list)

hourlyEnergyXdf <- rxImport(inData = energy_df, outFile = hourlyEnergyXdf, overwrite = TRUE, stringsAsFactors = TRUE)
rxGetInfo(hourlyEnergyXdf, getVarInfo = TRUE)

start_time <- Sys.time()
hourlyEnergyXdf <- rxDataStep(inData = hourlyEnergyXdf, hourlyEnergyXdf
                                , transforms = list(date_time = as.Date(date, format = "%Y-%m-%d %H:%M:%S")
                                , year = as.integer(format(as.Date(date, format = "%Y-%m-%d %H:%M:%S"), "%Y"))
                                , month = format(as.Date(date, format = "%Y-%m-%d %H:%M:%S"), "%m")
                                , day = format(as.Date(date, format = "%Y-%m-%d %H:%M:%S"), "%d"))
                                , overwrite = TRUE)
(elapsed_time <- Sys.time() - start_time)

hourlyEnergyNewXdf <- file.path(output_dir, "hourlyEnergyNewXdf")
hourlyEnergyNewXdf <- rxFactors(inData = hourlyEnergyXdf, factorInfo = c("month", "day"), outFile = hourlyEnergyNewXdf, overwrite = TRUE)
rxGetInfo(hourlyEnergyNewXdf, getVarInfo = TRUE)

rxHistogram(~F(year) | src_system_cd, data = hourlyEnergyNewXdf)
rxLinePlot(MW ~ year | src_system_cd, data = hourlyEnergyNewXdf)

# Create the Test and Train Datasets
hourlyEnergyTrainXdf <- file.path(output_dir, "hourlyEnergyTrain.xdf")
hourlyEnergyTestXdf <- file.path(output_dir, "hourlyEnergyTest.xdf")

hourlyEnergyTrainXdf <- rxDataStep(hourlyEnergyNewXdf, hourlyEnergyTrainXdf, rowSelection = year <= 2015, overwrite = TRUE)
hourlyEnergyTestXdf <- rxDataStep(hourlyEnergyNewXdf, hourlyEnergyTestXdf, rowSelection = year > 2015, overwrite = TRUE)

# Create a linear model predicting the MegaWatts used
hourlyConsumptionPredLinModXdf <- file.path(output_dir, "hourlyConsumptionPredLinMod.xdf")
consumption_pred_lin_mod <- rxLinMod(MW ~ src_system_cd + year + month + day , data = hourlyEnergyTrainXdf)
hourlyConsumptionPredLinModXdf <- rxPredict(consumption_pred_lin_mod, data = hourlyEnergyTestXdf, outData = hourlyConsumptionPredLinModXdf, writeModelVars = TRUE, extraVarsToWrite = "Label", overwrite = TRUE)
rxGetInfo(hourlyConsumptionPredLinModXdf, getVarInfo = TRUE)
head(hourlyConsumptionPredLinModXdf, n = 100)

# Create a Neural Network model predicting the MegaWatts used
hourlyConsumptionNeuralNetXdf <- file.path(output_dir, "hourlyConsumptionNeuralNet.xdf")
consumption_neural_net <- rxNeuralNet(MW ~ src_system_cd + year + month + day, data = hourlyEnergyTrainXdf ,type = c("regression"), numHiddenNodes = 100, numIterations = 50)
hourlyConsumptionNeuralNetXdf <- rxPredict(consumption_neural_net, data = hourlyEnergyTestXdf, outData = hourlyConsumptionNeuralNetXdf, writeModelVars = TRUE, overwrite = TRUE)
rxGetInfo(hourlyConsumptionNeuralNetXdf, getVarInfo = TRUE)
head(hourlyConsumptionNeuralNetXdf, n = 20)
summary(consumption_neural_net)


# Compute contexts
parallelContext <- RxLocalParallel()
rxSetComputeContext(parallelContext)

newXdf <- file.path(output_dir, "new.xdf")

start_time <- Sys.time()
newXdf <- rxExec(rxDataStep, inData = hourlyEnergyXdf, transforms = list(date_time = as.Date(date, format = "%Y-%m-%d %H:%M:%S"), year = as.integer(format(as.Date(date, format = "%Y-%m-%d %H:%M:%S"), "%Y")), month = format(as.Date(date, format = "%Y-%m-%d %H:%M:%S"), "%m"), day = format(as.Date(date, format = "%Y-%m-%d %H:%M:%S"), "%d")))
(elapsed_time <- Sys.time() - start_time)

###################################################
############### WIP ###############################
###################################################


# WIP
aep_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$AEP) != TRUE, c(1, 2)]
comed_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$COMED) != TRUE, c(1, 3)]
dayton_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$DAYTON) != TRUE, c(1, 4)]
deok_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$DEOK) != TRUE, c(1, 5)]
dom_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$DOM) != TRUE, c(1, 6)]
duq_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$DUQ) != TRUE, c(1, 7)]
ekpc_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$EKPC) != TRUE, c(1, 8)]
fe_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$FE) != TRUE, c(1, 8)]
ni_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$NI) != TRUE, c(1, 9)]
pjme_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$PJME) != TRUE, c(1, 10)]
pjmw_est <- energy_files$PJM_hourly_est[is.na(energy_files$PJM_hourly_est$PJMW) != TRUE, c(1, 11)]

# WIP
add_source_cd <- function(dataframe) {
    df_name_list <- c("AEP_hourly", "COMED_hourly", "DAYTON_hourly", "DEOK_hourly", "DOM_hourly", "DUQ_hourly", "EKPC_hourly", "FE_hourly", "NI_hourly", "PJME_hourly", "PJMW_hourly")
    for (i in 1:length(df_name_list)) {
        df <- noquote(df_name_list[i])
        dataframe$src_system_cd <- df
        return(dataframe)
    }
}

df_list <- lapply(df_list, add_source_cd)
head(df_list$AEP_hourly)
head(df_list$COMED_hourly)