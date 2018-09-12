setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")
list.files(rxGetOption("sampleDataDir"))

library(RevoTreeView)

# A Simple Classification Tree
kyphTree <- rxDTree(Kyphosis ~ Age + Start + Number, data = kyphosis, cp = 0.01)
kyphTree

# A Simple Regression Tree
mtCarTree <- rxDTree(mpg ~ disp, data = mtcars)
mtCarTree

# A Larger Regression Tree Model
censusWorkers <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
rxGetInfo(censusWorkers, getVarInfo = TRUE, getBlockSizes = TRUE, getValueLabels = TRUE)
incomeTree <- rxDTree(incwage ~ age + sex + wkswork1 + state, data = censusWorkers, pweights = "perwt", minBucket = 30000, maxDepth = 3)
incomeTree

plot(createTreeView(kyphTree))

plot(rxAddInheritance(kyphTree))
text(rxAddInheritance(kyphTree))