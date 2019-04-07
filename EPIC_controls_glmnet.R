# glmnet penalised model for CRC controls
# didn't really work but useful to know how to do

#data prep from plsdata. Convert high and low scores to 1 and 0
filtmat <- plsdata[ plsdata[,1] != 3, ]
filtmat[1] <- ifelse(filtmat[1] > 3, 1, 0)
scorehighlow <- filtmat[, 1]
metabo <- as.matrix(filtmat[, -1])

library(glmnet)
# response and predictor variables must be matrices
fit <- glmnet(metabo, scorehighlow, family = "binomial")
summary(fit)

cv.fit <- cv.glmnet(metabo, scorehighlow, family = "binomial")
plot(cv.fit)
lambda.min <- cv.fit$lambda.min
#First dotted vertical line indicates minimal mse; 2nd indicates one sd from mse

plot(fit)
coef(fit)
obs2predict <- as.matrix(obs2predict)

#Predict high or low score from model
tpred <- predict(fit, obs2predict, family = "binomial", s = lambda.min, type = "class") %>% as.numeric
names(tpred) <- "glmscore"

# Bind predicted scores from glmnet model to the CRC dataset
crc <- bind_cols(glmscore = tpred, df)

# Remove odd cases or controls
crc <- crc %>% group_by(Match_Caseset) %>% filter(n() == 2) %>% ungroup

# CLR Predicted scores
library(survival)
fit.clr <- clogit(Cncr_Caco_Clrt ~ glmscore + strata(Match_Caseset), data = crc)
summary(fit.clr)
table(status = crc$Cncr_Caco_Clrt, scorehighlow = crc$glmscore)


