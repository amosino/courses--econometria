library(ggplot2)
library(dplyr)
library(VGAM)

set.seed(123)
x<- seq(11,60,1)
u<-rnorm(length(x), 0,10)
y<- -40 + 1.2*x + u

plot(x,y)
ycensurada <- y
ycensurada[which(ycensurada<=0)] <- 0

mod <- lm(y~x)
modcen <-lm(ycensurada~x)

modelo_tobit <- vglm(ycensurada ~ x, tobit(Lower = 0))
summary(modelo_tobit)

ctable <- coef(summary(modelo_tobit))
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(modelo_tobit), lower.tail = FALSE)
cbind(ctable, pvals)
