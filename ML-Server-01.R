setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

library(RevoScaleR)

list.files(rxGetOption("sampleDataDir"))

dataSource <- file.path(rxGetOption("sampleDataDir"), "AirlineDemoSmall.csv")
airXdfData <- rxImport(inData = dataSource)

airXdfData <- rxImport(inData = dataSource, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/airXdfData.xdf")

rxGetInfo(airXdfData, getVarInfo = TRUE)

airXdfData <- rxImport(inData = dataSource, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/airXdfData.xdf", stringsAsFactors = TRUE, missingValueString = "M", rowsPerRead = 200000, overwrite = TRUE)
rxGetInfo(airXdfData, getVarInfo = TRUE)

colInfo <- list(DayOfWeek = list(type = "factor", levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

airXdfData <- rxImport(inData = dataSource, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/airXdfData.xdf", missingValueString = "M", rowsPerRead = 200000, colInfo = colInfo, colClasses = c(ArrDelay = "integer"), overwrite = TRUE)

rxHistogram(~ArrDelay | DayOfWeek, data = airXdfData)

rxSummary(~ArrDelay, data = airXdfData)

airXdfData <- rxDataStep(inData = airXdfData, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/airXdfData.xdf", transforms = list(VeryLate = (ArrDelay > 120 | is.na(ArrDelay))), overwrite = TRUE)

rxGetInfo(airXdfData, getVarInfo = TRUE)

rxSummary(~ArrDelay + CRSDepTime + DayOfWeek, data = airXdfData)

summary(airXdfData)

rxSummary(~ArrDelay:DayOfWeek, data = airXdfData)

options("device.ask.default" = T)
rxHistogram(~ArrDelay, data = airXdfData)
rxHistogram(~CRSDepTime, data = airXdfData)
rxHistogram(~DayOfWeek, data = airXdfData)

mySampleData <- rxDataStep(inData = airXdfData, rowSelection = ArrDelay > 240 & ArrDelay <= 300, varsToKeep = c("ArrDelay", "DayOfWeek"))
rxHistogram(~ArrDelay, data = mySampleData)

nrow(airXdfData)
ncol(airXdfData)
head(airXdfData)

airXdfDataSmall <- rxDataStep(inData = airXdfData, numRows = 10, startRow = 100000)
airXdfDataSmall

airExtraDS <- rxDataStep(inData = airXdfData, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/xdf_test2.xdf", transforms = list(late = ArrDelay > 15, DepHour = as.integer(CRSDepTime), Night = DepHour >= 20 | DepHour <= 5))
rxGetInfo(airExtraDS, getVarInfo = TRUE, numRows = 5)