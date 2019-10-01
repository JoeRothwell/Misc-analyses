# Mediation analysis tutorial

myData <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv')

model.0 <- lm(Y~X, myData)
summary(model.0)
plot(with(myData, X, Y))
