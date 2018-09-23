churn_dir <- 'C:/Users/mhuffer/Desktop/data/telco-cust-churn/'

output_dir <- 'C:/Users/mhuffer/Documents/GitHub/ML-Server'
churnXdf <- file.path(output_dir, 'custChurn.xdf')

churn_file <- list.files(churn_dir, pattern = '*.csv')
file <- paste0(churn_dir,'/',churn_file)

churn_file <- lapply(file, read.csv)
names(churn_file) <- c("cust_churn")

churnXdf <- rxImport(churn_file$cust_churn, churnXdf, overwrite = TRUE)
rxGetInfo(churnXdf, getVarInfo = TRUE)

rxSummary(~gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = churnXdf)

rxCrossTabs(~gender:Partner, data = churnXdf)
rxCube(~gender:Partner, data = churnXdf)

print_CrossTabs <- rxCrossTabs(tenure~gender:Partner, data = churnXdf)
print(print_CrossTabs)
print(print_CrossTabs, output = "means")

print_Cube <- rxCube(tenure ~ gender:Partner, data = churnXdf)
print(print_Cube)
print(print_Cube, output = "means")

churnXdf <- rxFactors(inData = churnXdf, outFile = churnXdf, factorInfo = list(gender = list(newLevels = c(M = "Male", F = "Female"), varName = "gender")), overwrite = TRUE)
rxGetVarInfo(churnXdf)
churnXdf <- rxDataStep(inData = churnXdf, outFile = churnXdf, transforms = list(gender = factor(gender, levels = c("Male", "Female"), labels = c("Male", "Female"))), overwrite = TRUE)
rxGetVarInfo(churnXdf)

# Create a logical 1 or 0 for customer churn
churnXdf <- rxDataStep(inData = churnXdf , outFile = churnXdf, transforms = list(churnLabel = ifelse(Churn == "Yes", 1, 0)), overwrite = TRUE)

# Add Binomial indicator for train/test split
binomialInd <- function(data) {
    set.seed(10)
    data$TrainInd <- rbinom(length(data[[1]]), 1, .70)
    return(data)
}

churnTrainXdf <- file.path(output_dir, "churnTrain.xdf")
churnTestXdf <- file.path(output_dir, "churnTest.xdf")

churnTrainXdf <- rxDataStep(inData = churnXdf, outFile = churnTrainXdf, varsToDrop = c("customerID"), rowSelection = TrainInd == 1, transformFunc = binomialInd, overwrite = TRUE)
churnTestXdf <- rxDataStep(inData = churnXdf, outFile = churnTestXdf, varsToDrop = c("churnLabel","customerID"), rowSelection = TrainInd == 0, transformFunc = binomialInd, overwrite = TRUE)

# K-Means clustering example
churnClusterXdf <- file.path(output_dir, "churnCluster.xdf")

churnClusterXdf <- rxKmeans(~tenure + MonthlyCharges + TotalCharges, data = churnTrainXdf, seed = 5, numClusters = 5, outFile = churnClusterXdf, overwrite = TRUE)
churnClusterXdf

# Logistic Regression Model
churnLogitModel <- rxLogit(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = churnTrainXdf)
churnLogitModel

churnLogitPredXdf <- file.path(output_dir, "churnLogitPred.xdf")
churnLogitPredXdf <- rxPredict(churnLogitModel, data = churnTestXdf, outData = churnLogitPredXdf, type = "response", writeModelVars = TRUE, overwrite = TRUE)
rxGetInfo(churnLogitPredXdf, getVarInfo = TRUE, getBlockSizes = TRUE, getValueLabels = TRUE)
churnLogitPredXdf <- rxDataStep(inData = churnLogitPredXdf, outFile = churnLogitPredXdf, transforms = list(ChurnPrediction = ifelse(Churn_Pred >= 0.5, "Yes", "No")), overwrite = TRUE)
head(churnLogitPredXdf)

# GLM Model
churnGenLinModel <- rxGlm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = churnTrainXdf, family = binomial())

churnGenLinPredXdf <- file.path(output_dir, "churnGenLinPred.xdf")
churnGenLinPredXdf <- rxPredict(churnGenLinModel, data = churnTestXdf, outData = churnGenLinPredXdf, writeModelVars = TRUE, overwrite = TRUE)
churnLogitPredXdf <- rxDataStep(inData = churnGenLinPredXdf, outFile = churnGenLinPredXdf, transforms = list(ChurnPrediction = ifelse(Churn_Pred >= 0.5, "Yes", "No")), varsToKeep = c("ChurnPrediction","Churn","Churn_Pred"), overwrite = TRUE)
rxGetInfo(churnGenLinPredXdf, getVarInfo = TRUE)
head(churnGenLinPredXdf)

# Linear Model with L1 and L2 Regularlization
# rxLogisticRegression model

churnLogRegModel <- rxLogisticRegression(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = churnTrainXdf, type = c("binary"), l1Weight = 100, l2Weight = 0)
summary(churnLogRegModel)

churnLogRegPredXdf <- file.path(output_dir, "churnLogRegPred.xdf")
churnLogRegPredXdf <- rxPredict(churnLogRegModel, data = churnTestXdf, outData = churnLogRegPredXdf, overwrite = TRUE)
rxGetInfo(churnLogRegPredXdf, getVarInfo = TRUE)