setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

library(RevoScaleR)

list.files(rxGetOption("sampleDataDir"))

# An example decision forest
kyphForest <- rxDForest(Kyphosis ~ Age + Start + Number, seed = 10, data = kyphosis, cp = 0.01, nTree = 500, mTry = 3)
kyphForest

dfPreds <- rxPredict(kyphForest, data = kyphosis)
dfPreds

sum(as.character(dfPreds[, 1]) == as.character(kyphosis$Kyphosis)) / 81

# A simple Regression Forest
stackForest <- rxDForest(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss, maxDepth = 3, nTree = 200, mTry = 2)
stackForest

# A larger regression forest model
censusWorker <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
rxGetInfo(censusWorker, getVarInfo = TRUE)
incForest <- rxDForest(incwage ~ age + sex + wkswork1, data = censusWorker, pweights = "perwt", maxDepth = 3, minBucket = 30000, mTry = 2, nTree = 200)

# Stochastic gradient boosting
# A simple Classification Forest

kyphBTrees <- rxBTrees(Kyphosis ~ Age + Start + Number, seed = 10, data = kyphosis, cp = 0.01, nTree = 500, mTry = 3, lossFunction = "bernoulli")
kyphBTrees

# A simple regression forest
stackBTrees <- rxDForest(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss, nTree = 200, mTry = 2, lossFunction = "gaussian")
stackBTrees

# A multinomial forest model
irisBTrees <- rxBTrees(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, maxDepth = 3, nTree = 50, seed = 0, lossFunction = "multinomial")
irisBTrees