setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

library(RevoScaleR)

list.files(rxGetOption("sampleDataDir"))

data_source <- file.path(rxGetOption("sampleDataDir"), "AirlineDemoSmall.xdf")
airXdf <- rxImport(data_source, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/airXdf.xdf", overwrite = TRUE)

rxGetInfo(airXdf, getVarInfo = TRUE)

# Use F() to quickly compute bins for each integer level
rxHistogram(~F(CRSDepTime), data = airXdf)
# Specify the number of breaks
rxHistogram(~F(CRSDepTime), numBreaks = 11, data = airXdf)

# Create panels for each of the days of the week
rxHistogram(~F(CRSDepTime) | DayOfWeek, data = airXdf)
# print the x axis labels at an angle and all panels in a row
rxHistogram(~F(CRSDepTime) | DayOfWeek, scales = list(x = list(rot = 30)), data = airXdf, layout = c(7, 1))

# Show panels for each day on a separate page
numCols <- 1
numRows <- 1

# Set ask to pause between each plot
par(ask = TRUE)

rxHistogram(~F(CRSDepTime) | DayOfWeek, data = airXdf, layout = c(numCols, numRows))

# Create a jpeg file file for each page, named myplot001.jpeg, etc
# jpeg(file="myplot",
#    rxHistogram(~F(CRSDepTime) | DayOfWeek, data = airXdf,
#    blocksPerRead=6, layout=c(numCols, numRows)))
# dev.off()

# rxLinePlot
# Simple scatter plot
rxLinePlot(ArrDelay ~ CRSDepTime, data = airXdf, type = "p")

airCube <- rxCube(~DayOfWeek:F(CRSDepTime), data = airXdf)
airResults <- rxResultsDF(airCube)

rxLinePlot(Counts ~ DayOfWeek , groups = DayOfWeek, data = airResults)