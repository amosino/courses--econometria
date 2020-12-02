library(tidyverse)

data <- read_csv("Datos/MEPSData.csv")
data <- mutate(data, femaled = as.numeric(data$female=="Female"))
mod.ols <- lm(formula= exp_tot~age+femaled, data=data, subset = (exp_tot>0))
summary(mod.ols)
res2 <- residuals(mod.ols)^2
hist(res2)


mod1.glm <- glm(formula=exp_tot~age+femaled,
                data=data, subset =(exp_tot>0))
summary(mod1.glm)

mod2.glm <- glm(formula=exp_tot~age+femaled,
                family=gaussian(link="log"),
                data=data, subset =(exp_tot>0))
summary(mod2.glm)

mod3.glm <- glm(formula= exp_tot~age+femaled,
                family=Gamma(link="log"),
                data=data, subset=(exp_tot>0))
summary(mod3.glm)

AIC.m1 <- AIC(mod1.glm, k=2)
BIC.m1 <- BIC(mod1.glm)
AIC.m2 <- AIC(mod2.glm, k=2)
BIC.m2 <- BIC(mod2.glm)
AIC.m3 <- AIC(mod3.glm, k=2)
BIC.m3 <- BIC(mod3.glm)
l.AIC <- c(AIC.m1, AIC.m2, AIC.m3)
l.BIC <- c(BIC.m1, BIC.m2, BIC.m3)
IC <- data.frame("AIC" = l.AIC, "BIC" = l.BIC)
print(IC)
