setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

library(RevoScaleR)

list.files(rxGetOption("sampleDataDir"))

data_source <- file.path(rxGetOption("sampleDataDir"), "AirlineDemoSmall.xdf")
airXdf <- rxImport(data_source, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/airXdf.xdf")

rxGetInfo(airXdf, getVarInfo = TRUE)
rxSummary(ArrDelay ~ DayOfWeek + CRSDepTime, data = airXdf)

# rxCube examples
airCube <- rxCube(ArrDelay ~ DayOfWeek, data = airXdf)
airCube

airCube <- rxCube(ArrDelay ~ F(DayOfWeek), data = airXdf, blocksPerRead = 3, returnDataFrame = TRUE)
airCube

rowSelectList <- c("Monday","Tuesday")
airCube.CRS_10 <- rxCube(ArrDelay ~ F(DayOfWeek), data = airXdf, rowSelection = CRSDepTime >= 10)
airCube.CRS_10

# rxQuantile Examples
airGLM <- rxGlm(ArrDelay ~ DayOfWeek + CRSDepTime, data = airXdf)

airPred <- tempfile(pattern = "airPred", fileext = ".xdf")
rxPredict(airGLM, data = airXdf, outData = airPred, writeModelVars = TRUE, overwrite = TRUE)

predBreaks <- rxQuantile(data = airPred, varName = "ArrDelay_Pred", probs = seq(from = 0, to = 1, by = .1))
predBreaks

# compare with quantile function
airPredDF <- rxDataStep(inData = airPred)
quantile(airPredDF$ArrDelay_Pred, probs = seq(0, 1, by = .1), type = 4)

file.remove(airPred)