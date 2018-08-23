setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

library(RevoScaleR)

list.files(rxGetOption("sampleDataDir"))

data_source <- file.path(rxGetOption("sampleDataDir"), "claims.xdf")
claimsXdf <- rxImport(data_source, outFile = "C:/Users/mhuffer/Documents/GitHub/ML-Server/claimsXdf.xdf")

rxGetInfo(claimsXdf, getVarInfo = TRUE)

claimsCTabs <- rxCrossTabs(cost ~ type:car.age, data = claimsXdf)
print(claimsCTabs, output = "sums")
print(claimsCTabs, output = "counts")
print(claimsCTabs, output = "means")
summary(claimsCTabs, type = "%")
summary(claimsCTabs, output = "means", type = "%")
summary(claimsCTabs, type = "chisquare")

claimsCTabsDep <- rxCrossTabs(cbind(cost, number) ~ type:car.age:age, data = claimsXdf)
claimsCTabsDep

claimsMeans <- mean(claimsCTabsDep, marginals = TRUE)
claimsMeans

barplot(claimsCTabs$sums$cost, xlab = "Car Age (Years)", ylab = "Cost (USD)", legend.text = c("Type A", "Type B", "Type C", "Type D"))

# Create a data frame
myDF <- data.frame(sex = c("Female", "Male", "Female", "Male"), age = c(20, 20, 12, 15), score = 1.1:4.1, sport = c(1:3, 2))

# Use the transforms argument to dynamically transform the variables
# of the data source.  Here, we form a named list of transformation
# expressions.  To avoid evaluation when assigning to a local variable
# we wrap the transformation list with expression().
transforms <- expression(list(ageHalved = age / 2, sport = factor(sport, labels = c("tennis", "golf", "football"))))
rxCrossTabs(score ~ sport:sex, data = myDF, transforms = transforms)
rxCrossTabs(~sport:F(ageHalved, low = 7, high = 10), data = myDF, transforms = transforms)

# No transformFunc or formula arithmetic expressions
rxCrossTabs(score ~ F(age):sex, data = myDF)

# Transform a categorical variable to a continuous one and use it as 
# a response variable in the formula for cross-tabulation.
myDF_2 <- data.frame(sex = c("Male", "Male", "Female", "Male"), age = factor(c(20, 20, 12, 15)), score = factor(1.1:4.1))
rxCrossTabs(N(age) ~ sex:score, data = myDF_2)

# To transform a categorical variable (like age) that has numerica levels
# (as opposed to codes), use the following construction:
myDF_3 <- data.frame(sex = c("Male", "Male", "Female", "Male"), age = factor(c(20, 20, 12, 15)), score = factor(1.1:4.1))
rxCrossTabs(as.numeric(levels(age))[age] ~ sex:score, data = myDF_3)

# frequency weighting
fwts <- 1:4
sex <- c("Male", "Male", "Female", "Male")
age <- c(20, 20, 12, 15)
score <- 1.1:4.1

myDF1 <- data.frame(sex = sex, age = age, score = factor(score), fwts = fwts)
myDF2 <- data.frame(sex = rep(sex, fwts), age = rep(age, fwts), score = factor(rep(score, fwts)))

mySums1 <- rxCrossTabs(age ~ sex:score, data = myDF1, fweights = "fwts")$sums$age[c("Male", "Female"),]
mySums2 <- rxCrossTabs(age ~ sex:score, data = myDF2)$sums$age[c("Male", "Female"),]
all.equal(mySums1,mySums2)