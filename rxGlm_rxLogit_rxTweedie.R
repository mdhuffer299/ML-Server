setwd("C:/Users/mhuffer/Documents/GitHub/ML-Server")

list.files(rxGetOption("sampleDataDir"))

# Compare rxGlm and glm, which both can be used on small data sets
infertRxGlm <- rxGlm(case ~ age + parity + education + spontaneous + induced, family = binomial(), dropFirst = TRUE, data = infert)
summary(infertRxGlm)

infertGlm <- glm(case ~ age + parity + education + spontaneous + induced, data = infert, family = binomial())
summary(infertGlm)

# For the case of the binomial family with the 'logit' link family,
# the optimized rxLogit function can be used
infertRxLogit <- rxLogit(case ~ age + parity + education + spontaneous + induced, data = infert, dropFirst = TRUE)
summary(infertRxLogit)

# Instead of using the equivalent of contr.SAS, estimate the parameters
# for the categorical levels without contrasting against an intercept form.
infertCubeLogit <- rxLogit(case ~ education + age + parity + spontaneous + induced, data = infert, cube = TRUE)
summary(infertCubeLogit)

# Define an ageGroup variable through a variable transforamtion list.
# Partition the age groups between 1 and 100 by 20 years.
rxLogit(case ~ education + ageGroup + parity + spontaneous + induced, transforms = list(ageGroup = cut(age, seq(1, 100, 20))), data = infert, cube = TRUE)

# Estimate a Gamma family model using sample data
claimsXdf <- file.path(rxGetOption("sampleDataDir"), "claims.xdf")
claimsGlm <- rxGlm(cost ~ age + car.age + type, family = Gamma, dropFirst = TRUE, data = claimsXdf)
summary(claimsGlm)

# In the claims data, the cost is set to NA if no claim was made
# Convert NA to 0 for the cost, to prepare data for using
# the Tweedie family - which is appropriate for positive data
# that also contains exact zeros
# Read the transformed data into a data frame
claims <- rxDataStep(inData = claimsXdf, transforms = list(cost = ifelse(is.na(cost), 0, cost)))

# Estimate using a Tweedie family
claimsTweedie <- rxGlm(cost ~ age + car.age + type, data = claims, family = rxTweedie(var.power = 1.15))
summary(claimsTweedie)

# Re-estimate using a Tweedie family setting link.power to 0,
# resulting in a log link function
claimsTweedie <- rxGlm(cost ~ age + car.age + type, data = claims, family = rxTweedie(var.power = 1.5, link.power = 0))
summary(claimsTweedie)

# Illustrate the use of a Tweedie family with offset
TestData <- data.frame(
    Factor1 = as.factor(c(1, 1, 1, 1, 2, 2, 2, 2)),
    Factor2 = as.factor(c(1, 1, 2, 2, 1, 1, 2, 2)),
    Discount = c(1, 2, 1, 2, 1, 2, 1, 2),
    Exposure = c(24000, 40000, 7000, 14000, 7500, 15000, 2000, 5600),
    PurePrem = c(46, 32, 73, 58, 48, 25, 220, 30)
    )

rxGlmTweedieOffset <- rxGlm(PurePrem ~ Factor1 * Factor2 - 1 + offset(log(Discount)), family = rxTweedie(var.power = 1.5, link.power = 0), data = TestData, fweights = "Exposure", dropFirst = TRUE, dropMain = FALSE)
summary(rxGlmTweedieOffset)
