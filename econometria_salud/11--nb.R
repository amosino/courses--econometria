# install.packages("countreg", repos="http://R-Forge.R-project.org")

library(tidyverse)
library(margins)
library(MASS)
library(countreg)
library(AER)

db <- read_csv("Datos/MEPSData.csv")
db$female <- as.factor(as.numeric(db$female=="Female"))
hist(db$use_off)

# Regresión binomial negativa

nbm.fit <- glm.nb(use_off~ age + female, data=db)
summary(nbm.fit)

summary(margins(nbm.fit))
summary(margins(nbm.fit, at=list(age=mean(db$age)), variables="female"))

# Bondad de ajuste

rootogram(nbm.fit)
