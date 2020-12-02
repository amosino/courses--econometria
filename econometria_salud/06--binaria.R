library(tidyverse)
library(mfx)
library(caret)

data <- read_csv("Datos/MEPSData.csv")
hist(data$exp_tot)


data$exp_tot <- as.factor(as.numeric(data$exp_tot>0))
data$female <- as.factor(as.numeric(data$female=="Female"))

# MCO

mco <- lm(as.numeric(exp_tot)~female*age, data=data)
summary(mco)

# PROBIT

probit <- glm(exp_tot~female*age, data=data,
              family=binomial(link="probit"))
summary(probit)

newdb <- data.frame(age=80, female=c("0","1"))
predict(probit, newdata=newdb, type="response")
probitmfx(exp_tot~female*age, data=data)


# LOGIT

logit <- glm(exp_tot~female*age, data=data,
              family=binomial(link="logit"))
summary(logit)
predict(logit, newdata=newdb, type="response")
logitmfx(exp_tot~female*age, data=data)


# Matrix de confusión

probit.probs <- predict(probit, newdata=data, type="response")
probit.pron <- as.factor(as.numeric(probit.probs>0.5))
probit.refs <- as.factor(data$exp_tot)
profit.conmat <- confusionMatrix(data=probit.pron, reference=probit.refs)

logit.probs <- predict(logit, newdata=data, type="response")
logit.pron <- as.factor(as.numeric(logit.probs>0.5))
logit.conmat <- confusionMatrix(data=logit.pron, reference=probit.refs)
