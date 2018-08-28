setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")
list.files(rxGetOption("sampleDataDir"))

library(RevoScaleR)

form <- Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species
irisLinMod <- rxLinMod(form, data = iris)
irisLinMod
summary(irisLinMod)

irisLM <- lm(form, data = iris, contrasts = list(Species = contr.SAS))
summary(irisLM)

irisCubeLinMod <- rxLinMod(Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width, data = iris, cube = TRUE)
summary(irisCubeLinMod)

# Create a model on a subset of data using rowSelection
censusWorker <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers")
censusLinMod <- rxLinMod(wkswork1 ~ age:sex, data = censusWorker, pweights = "perwt")
censusLinMod

censusSubsetLinMod <- rxLinMod(wkswork1 ~ age:sex, data = censusWorker, pweights = "perwt", rowSelection = age > 39)
censusSubsetLinMod

# Use the Sample Airline data and report progress during computations
sampleDataDir <- rxGetOption("sampleDataDir")
airlineDemoSmall <- file.path(sampleDataDir, "AirlineDemoSmall.xdf")

airlineLinMod <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoSmall, reportProgress = 1)
airlineLinMod <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoSmall, reportProgress = 2)
airlineLinMod <- rxLinMod(ArrDelay ~ CRSDepTime, data = airlineDemoSmall, reportProgress = 2, blocksPerRead = 3)

summary(airlineLinMod)