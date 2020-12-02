# install.packages("tidyverse")
# install.packages("car")
# install.packages("moments")
# install.packages("MASS")

library(tidyverse)
library(car)
library(moments)
library(MASS)

data <- read_csv("Datos/MEPSData.csv")
data$femaled <- as.factor(as.numeric(data$female=="Female"))
mod.lrm <- lm(exp_tot~age+femaled, data=data, subset=(exp_tot>0))
summary(mod.lrm)
hist(mod.lrm$residuals)
qqPlot(rstandard(mod.lrm))
skewness(mod.lrm$residuals) 
kurtosis(mod.lrm$residuals) 
b <- boxcox(mod.lrm)
lambda <- b$x
lik <- b$y
bc <- cbind(lambda, lik)
sorted_bc <- bc[order(-lik),]
head(sorted_bc, n = 10)

mod.lrm2 <- lm(log(exp_tot)~age+femaled, data=data, subset=(exp_tot>0))
hist(mod.lrm2$residuals)               
qqPlot(rstandard(mod.lrm2))
skewness(mod.lrm2$residuals) 
kurtosis(mod.lrm2$residuals)                
               