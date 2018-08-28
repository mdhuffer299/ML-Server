setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

# Create a data frame
set.seed(100)
df <- data.frame(x = 1:1000, y = rep(c("a", "b", "c", "d"), 25), z = rnorm(1000), w = runif(1000))

# Get a subset rows and columns from the data frame
df_sub_1 <- rxDataStep(inData = df, varsToKeep = c("x", "y", "z"), rowSelection = z > 0)
rxGetInfo(df_sub_1, getVarInfo = TRUE)

# Create a simple .xdf file from the data frame
inputFile <- file.path(tempdir(), "textInput.xdf")
rxDataStep(inData = df, outFile = inputFile, overwrite = TRUE)

# Create a multi-block .xdf file
rxDataStep(inData = df, outFile = inputFile, rowsPerRead = 50, overwrite = TRUE)
rxGetInfo(inputFile)

# Subset rows and columns, creating a new .xdf file
outputFile <- file.path(tempdir(), "testOutput.xdf")
rxDataStep(inData = inputFile, outFile = outputFile, varsToKeep = c("x", "w", "z"), rowSelection = z > 0, overwrite = TRUE)
rxGetInfo(outputFile)

# Use transforms list with data frame input and output data
# Add new columns
df_2 <- rxDataStep(inData = df, transforms = list(a = w > 0, b = 100 * z))
names(df_2)

myXFormFunc <- function(data) {
    data$b <- 100 * data$z
    return(data)
}

df_3 <- rxDataStep(inData = df, transformFunc = myXFormFunc)
names(df_3)

# Use transformFunc to remove columns
xForm <- function(data) {
    data$w <- NULL
    return(data)
}

df_3 <- rxDataStep(inData = df, transformFunc = xForm, overwrite = TRUE)

# Use "transform" and "transformEnvir" to add new columns
env <- new.env()
env$constant <- 10
env$myTransform <- function(x) {
    x * constant
}

environment(env$myTransform) <- env
data <- rxDataStep(inData = inputFile, outFile = outputFile, transforms = list(b = myTransform(x)), transformEnvir = env, overwrite = TRUE)
rxGetVarInfo(outputFile)

# Create a selection variable to take a roughly 25% random sample
# of each block of data read
rxDataStep(inData = inputFile, outFile = outputFile, rowSelection = selVar, transforms = list(selVar = as.logical(rbinom(.rxNumRows, 1, .25))), overwrite = TRUE)
rxGetInfo(outputFile)