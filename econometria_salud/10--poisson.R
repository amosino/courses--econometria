library(tidyverse)
library(margins)
library(sandwich)
#library(countreg)
library(AER)

db <- read_csv("Datos/MEPSData.csv")
hist(db$use_off)

db$female <- as.factor(as.numeric(db$female=="Female"))


# Regresión lineal
mco <- lm(use_off ~ age + female, data=db)
summary(mco)

# Regresión Poisson

poisson <- glm(use_off~ age + female, data=db, family = poisson)
summary(poisson)
summary(margins(poisson))
summary(margins(poisson, at=list(age=mean(db$age)), variables="female"))


# Errores estándar robustos

cov.poisson <- vcovHC(poisson, type="HC0")
std.err <- sqrt(diag(cov.poisson))
r.est <- cbind(Estimate= coef(poisson), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson)/std.err), lower.tail=FALSE),
               LL = coef(poisson) - 1.96 * std.err,
               UL = coef(poisson) + 1.96 * std.err)
r.est

# Prueba de sobre dispersión

dispersiontest(poisson)

