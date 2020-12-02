library(tidyverse)

# Leer datos
data <- read_csv("Datos/MEPSData.csv")

# Construcción de dummies
data$female <- as.factor(as.numeric(data$female=="Female"))
data$d_exp_tot <- as.factor(as.numeric(data$exp_tot>0))

# Primera parte: estimación logit
probit.fit <- glm(d_exp_tot~age+female, data=data,
                  family = binomial(link="probit"))
summary(probit.fit)
fp <- predict(probit.fit, data, type = "response")

# Segunda parte: modelo lineal generalizado
gamma.fit <- glm(exp_tot~age+female,
                 data=data, subset=(exp_tot>0),
                 family=Gamma(link=log))
summary(gamma.fit)
sp <- predict(gamma.fit, data, type = "response")

# Pronóstico del modelo en dos partes
y_dospartes <- fp * sp
