#installed.packages("sampleSelection")

library(sampleSelection)
data(Mroz87)

# Modelo por MCO
ols <- lm(log(wage) ~ educ + exper + I( exper^2 ) + city, data=subset(Mroz87, lfp==1))
summary(ols)


# Modelo de Heckman versión dos etapas
Mroz87$kids <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )
heckman <- heckit( lfp ~ age + I( age^2 ) + kids + huswage + educ,
                  log(wage) ~ educ + exper + I( exper^2 ) + city, data=Mroz87 )
summary(heckman)

# Modelo de Heckman versión máxima verosimilitud
mselection <- selection(lfp ~ age + I( age^2 ) + kids + huswage + educ, 
                        log(wage) ~ educ + exper + I( exper^2 ) + city, data=Mroz87)
summary(mselection)
