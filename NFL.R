library("RODBC")
library("RevoScaleR")
library("dplyrXdf")

source("C:/Users/mhuffer/Desktop/AzureSQLDBConnection.R")

setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

outputDir <- "C:/Users/mhuffer/Documents/GitHub/ML-Server"

outputFile <- file.path(outputDir, "nflFullXdf.xdf")

# Connection string is stored in a separate file for security.
#connectionString <- "Driver={ODBC Driver 13 for SQL Server};Server=Your Server.database.windows.net,1433;Database=Your Database;Uid=Your ID;Pwd={Your Password};Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;"
sqlQuery <- "SELECT * FROM dbo.play_by_play WHERE PLAY_TYPE = 'Pass' OR PLAY_TYPE = 'Run'"
conn <- odbcDriverConnect(connectionString)

# Data can be loaded into a data frame prior to loading into an XDF data file.
#nfl_df <- data.frame(sqlQuery(conn, sqlQuery))
#nflFullXdf <- rxImport(nfl_df, outputFile)

# Data is loaded directly into an XDF data file
nflFullXdf <- rxImport(data.frame(sqlQuery(conn,sqlQuery)), outputFile, stringsAsFactors = TRUE ,overwrite = TRUE)
close(conn)

rxGetInfo(nflFullXdf, getVarInfo = TRUE, getBlockSizes = TRUE, getValueLabels = TRUE)

formula_1 <- ~ .

summary <- rxSummary(formula_1, nflFullXdf)

# Create a subset of data based on variables that would be present during play execution
var_list <- c("GAME_DATE", "GAME_ID", "PLAY_ID", "DRIVE", "QTR", "DOWN", "GAME_TIME", "TIME_SECS", "SIDE_OF_FIELD", "YARD_LINE", "YARD_LINE_100"
            , "YARDS_TO_GO", "POS_TEAM", "DEF_TEAM", "PLAY_TYPE", "POS_TEAM_SCORE", "DEF_TEAM_SCORE", "AWAY_TEAM", "HOME_TEAM", "SEASON")

nflSmallXdf <- file.path(outputDir, "nflSmallXdf.xdf")

nflSmallXdf <- rxDataStep(nflFullXdf, nflSmallXdf, varsToKeep = var_list ,overwrite = TRUE)

rxGetInfo(nflSmallXdf, getVarInfo = TRUE, numRows = 10)
nflSmallSum <- rxSummary(formula_1, nflSmallXdf)
nflSmallSumStats <- nflSmallSum$sDataFrame
nflSmallSumStats

# Clean the data and missing observations
# All missing factor observations have been left
nflSmallXdf <- rxDataStep(nflSmallXdf, nflSmallXdf
                            , transforms = list(TIME_SECS = ifelse(is.na(TIME_SECS), as.integer(0), TIME_SECS)
                                                , TIME_SECS = ifelse(TIME_SECS < 0, as.integer(0), TIME_SECS))
                            , overwrite = TRUE)

nflSmallSum <- rxSummary(formula_1, nflSmallXdf)
nflSmallSumStats <- nflSmallSum$sDataFrame
nflSmallSumStats

# For rxCrossTabs, the dependent variables must be continuous
# All of the independent variables must be factors "discrete"
# Creates cross tabulation counts on the variables defined in the formula
playTypeCounts <- rxCrossTabs( ~ POS_TEAM:DEF_TEAM:PLAY_TYPE, nflSmallXdf)
playTypeCounts

# Create a histogram showning play tendencies (Pass/Run) by team
rxHistogram(~POS_TEAM | PLAY_TYPE, histType = "Percent", nflSmallXdf, scales = list(x = list(rot = 90)))

# Train and Test Split
nflTrainXdf <- file.path(outputDir, "nflTrainXdf.xdf")
nflTestXdf <- file.path(outputDir, "nflTestXdf.xdf")

#convert_toFactor <- function(data) {
#    data$SEASON <- as.string(data$SEASON)
#    return(data)
#}

nflTrainXdf <- rxDataStep(inData = nflSmallXdf, outFile = nflTrainXdf, rowSelection = SEASON <= 2015, overwrite = TRUE)
nflTestXdf <- rxDataStep(inData = nflSmallXdf, outFile = nflTestXdf, rowSelection = SEASON > 2015, varsToDrop = c("PLAY_TYPE", "GAME_TIME", "SEASON"), overwrite = TRUE)
#nflTestXdf <- rxDataStep(inData = nflTestXdf, outFile = nflTestXdf, transformFunc = convert_toFactor, overwrite = TRUE)

# Linear Classification Model
reg_form <- PLAY_TYPE ~ GAME_DATE + GAME_ID + PLAY_ID + DRIVE + QTR + DOWN + TIME_SECS + SIDE_OF_FIELD + YARD_LINE + YARD_LINE_100 + YARDS_TO_GO + POS_TEAM + DEF_TEAM + POS_TEAM_SCORE + DEF_TEAM_SCORE + AWAY_TEAM + HOME_TEAM

play_type_classification <- rxGlm(formula = reg_form, data = nflTrainXdf, family = binomial())

nflPlayPredGlmXdf <- rxPredict(play_type_classification, data = nflTrainXdf, outData  = nflPlayPredGlmXdf, writeModelVars = TRUE, extraVarsToWrite = "Label", overwrite = TRUE)
rxGetInfo(nflPlayPredGlmXdf, getVarInfo = TRUE)
head(nflPlayPredGlmXdf)

# Logistic Regression 
play_type_rxlogit <- rxLogit(formula = reg_form, data = nflTrainXdf)
nflPlayPredLogitXdf <- rxPredict(play_type_rxlogit, data = nflTestXdf, outData = nflPlayPredLogitXdf, writeModelVars = TRUE, extraVarsToWrite = "Label", overwrite = TRUE)
rxGetInfo(nflPlayPredLogitXdf, getVarInfo = TRUE)
head(nflPlayPredLogitXdf)

playTypePredXdf <- file.path(outputDir, "playTypePredXdf.xdf")
playTypePredXdf <- rxDataStep(inData = nflPlayPredGlmXdf, outFile = playTypePredXdf, varsToKeep = c("GAME_ID", "PLAY_ID", "PLAY_TYPE_Pred", "PLAY_TYPE"), overwrite = TRUE)
head(playTypePredXdf)

# Neural Network
playTypeNeuralNetXdf <- file.path(outputDir, "playTypeNeuralNetXdf.xdf")
play_type_neural_net <- rxNeuralNet(formula = reg_form, data = nflTrainXdf, c("binary"), numHiddenNodes = 100, numIterations = 100)
playTypeNeuralNetXdf <- rxPredict(play_type_neural_net, data = nflTestXdf, outData = playTypeNeuralNetXdf, writeModelVars = TRUE, extraVarsToWrite = "Label", overwrite = TRUE)

# Local parallel compute context
