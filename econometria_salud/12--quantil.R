library(quantreg)
library(ggplot2)
library(tidyverse)

## Ejemplo 1: Simular datos con distribución normal y varianza no constante

x <- seq(0,100,length.out=100)
sig <- 0.1 + 0.05*x
b_0 <- 6
b_1 <- 0.1

set.seed(0)
e <- rnorm(100, mean=0, sd=sig)
y <- b_0 + b_1*x +e
data <- data.frame(x,y)

# Modelos de regresión lineal

plot(x,y)
abline(lm(y ~ x, data=data))

# Regresion cuantílica, gráfica y estimación

hist(y, probability = TRUE)
lines(density(y))

plot(x,y)
abline(rq(y ~ x, data=data, tau=0.9))

rq1.model <- rq(y ~ x, data=data, tau=0.9)
summary(rq1.model)
qs <- 1:9/10
qr2 <- rq(y ~ x, data=data, tau=qs)
coef(qr2)

# Gráficas utilizando ggplot2

ggplot(data, aes(x,y))+geom_point()+
  geom_smooth(method="lm")

ggplot(data, aes(x,y))+geom_point()+
  geom_quantile(quantiles=qs)

# Gráfica de estimación cuantílica

plot(summary(qr2), parm="x")

## Ejemplo 2: Simular datos con distribución normal y varianza constante

x <- seq(0,100,length.out=100)
b_0 <- 6
b_1 <- 0.1

set.seed(1)
e1 <- rnorm(100, mean=0, sd=0.5)
y1 <- b_0 + b_1*x +e1
data1 <- data.frame(x,y1)
hist(y1, probability = TRUE)
lines(density(y1))


qr3 <- rq(y1 ~ x, data=data1, tau=qs)
plot(summary(qr3), parm="x")


## Ejemplo 3: Utiliza datos MEPS


data <- read_csv("Datos/MEPSData.csv")
data$female <- as.factor(as.numeric(sdata$female=="Female"))

# Regresión cuantílica

meps.qr <- rq(exp_tot ~ age+female, data=data, subset=(exp_tot>0), tau=qs)
plot(meps.qr)