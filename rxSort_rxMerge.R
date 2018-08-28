setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

# rxSort
list.files(rxGetOption("sampleDataDir"))
censusWorkers <- file.path(rxGetOption("sampleDataDir"), "CensusWorkers.xdf")
rxGetInfo(censusWorkers, getVarInfo = TRUE)

outXDF <- "censusWorkersSorted.xdf"
rxSort(inData = censusWorkers, outFile = outXDF, sortByVars = c("age", "incwage"), decreasing = c(FALSE, TRUE))
rxGetInfo(outXDF, numRows = 10)

# Remove Duplicates
set.seed(10)
users <- sample(c("Aiden", "Ella", "Jayden", "Ava", "Max", "Grace", "Riley", "Lolita", "Liam", "Emma", "Ethan", "Elizabeth", "Jack", "Genevieve", "Avery", "Aurora", "Dylan", "Isabella", "Caleb", "Bella"), 100, replace = TRUE)
state <- sample(c("Washington", "California", "Texas", "North Carolina", "New York", "Massachusetts"), 100, replace = TRUE)
transAmt <- round(runif(100) * 100, digits = 3)
df <- data.frame(users = users, state = state, transAmt = transAmt)

rxSort(df, sortByVars = c("users", "state"), removeDupKeys = TRUE, dupFreqVar = "DUP_COUNT")

# rxMerge
claimsXdf <- file.path(rxGetOption("sampleDataDir"), "claims.xdf")
claims_union <- rxMerge(inData1 = claimsXdf, inData2 = claimsXdf, type = "union")

rxGetInfo(claims_union, numRows = 10)

# Left Join
# Outer joins (type = "left", "right", "full")
claims_left <- rxMerge(inData1 = claimsXdf, inData2 = claimsXdf, type = "left", matchVars = c("RowNum"))
rxGetInfo(claims_left, numRows = 10)

# Inner Join
claims_inner <- rxMerge(inData1 = claimsXdf, inData2 = claimsXdf, type = "inner", matchVars = c("RowNum"))
rxGetInfo(claims_inner, numRows = 10)

# one-to-one join
claims_1_1 <- rxMerge(inData1 = claimsXdf, inData2 = claimsXdf, type = "oneToOne")
rxGetInfo(claims_1_1, numRows = 10)