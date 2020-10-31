# Mediation analysis tutorial
# https://data.library.virginia.edu/introduction-to-mediation-analysis/

# data from http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv
myData <- read.csv("mediationData.csv")

# Effect of X on Y (total effect: coefficient of X)
model.0 <- lm(Y~X, myData)
summary(model.0)
plot(with(myData, X, Y))

# Effect of M on Y
model.M <- lm(M ~ X, myData)
summary(model.M)

# Controlling for M (direct effect: coefficient of X taking account of M)
model.Y <- lm(Y ~ X + M, myData)
summary(model.Y)

library(broom)
map_df(list(model.0, model.M, model.Y), tidy)

library(mediation)
results <- mediate(model.M, model.Y, treat='X', mediator='M', boot=TRUE, sims=500)
summary(results)

