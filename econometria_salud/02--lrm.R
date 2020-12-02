library(foreign)
library(effects)
library(car)
library(margins)
library(sandwich)
library(lmtest)
library(modmarg)



# Linear regression and effects plots

mod1 <- lm(exp_tot~age+lninc, data=sdata)
smod1 <- summary(mod1)
effage <- effect("age", mod1)
plot(effage)
alleff <- allEffects(mod1)
plot(alleff)

# Extracting information from estimation

df <- mod1$df.residual
N <- nobs(mod1)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
b3 <- coef(mod1)[[3]]
sigh <- smod1$sigma^2
anov <- anova(mod1)
RSS <- anov[3,2]
TSS <- sum(anov[,2])
ESS <- TSS-RSS
vcm <- vcov(mod1)

# Variance covariance matrix and standard errors

varb1 <- vcm[1,1]
varb2 <- vcm[2,2]
varb3 <- vcm[3,3]
covb1b2 <- vcm[1,2]
covb1b3 <- vcm[1,3]
covb2b3 <- vcm[2,3]
seb2 <- sqrt(varb2)
seb3 <- sqrt(varb3)

# Interval estimation

alpha <- 0.05
tcr <- qt(1-alpha/2, df)
lowb2 <- b2 - tcr*seb2
upb2 <- b2 + tcr*seb2
lowb3 <- b3 - tcr*seb3
upb3 <- b3 + tcr*seb3

# Hypothesis testing

t0b2 <- b2/seb2
p0b2 <- 2*(1-pt(abs(t0b2), df))
t0b3 <- b3/seb3
p0b3 <- 2*(1-pt(abs(t0b3), df))

# Left-tail
# tcr <- -qt(1-alpha,df)
# pval <- pt(tval, df)

# Right-tail
# tcr <- 1-qt(1-alpha,df)
# pval <- 1-pt(tval, df)

# Polynomial regression

mod2 <- lm(exp_tot~age+I(age^2)+lninc, data=sdata)
smod2 <- summary(mod2)
effage2 <- effect("I(age^2)", mod2)
plot(effage2)

# Marginal effects for some particular levels

lninclev <- c(10,11)
b2.2 <- coef(mod2)[[2]]
b3.2 <- coef(mod2)[[3]]
meffects <- b2.2+2*b3.2*lninclev

# Goodness of fit

Rsq <- smod1$r.squared
Rsq2 <- 1-RSS/TSS

# Factor variables

sdata$female <- factor(sdata$female)
sdata$anylim <- factor(sdata$anylim)
mod3 <- lm(exp_tot~age+female+anylim, data=sdata)
smod3 <- summary(mod3)

# Interaction variables

mod4 <- lm(exp_tot~age*female+anylim, data=sdata)
smod4 <- summary(mod4)

# Recode female and anylim as 0, 1 variables

sdata$femaled <- as.factor(as.numeric(sdata$female=="Female"))
sdata$anylimd <- as.factor(as.numeric(sdata$anylim=="Activity limitation"))

# Join hypothesis test

mod5 <- lm(exp_tot~age*femaled+anylimd, data=sdata)
hyp <- c("age=0", "femaled1=0", "age:femaled1=0")
linearHypothesis(mod5, hyp)

# Robust Standard Errors

coeftest(mod5, vcov = vcovHC(mod5, "HC1")) 

  
# Marginal and incremental effects


mmod4 <- summary(margins(mod4))
matmod4.1 <- summary(margins(mod4, variables="age"))
matmod4.2 <- summary(margins(mod4, at=list(female=c("Male", "Female")), variables="age"))
matmod4.3 <- summary(margins(mod4, at=list(age=c(20,45,70)), variables="female"))

# Margins plot

plot(margins(mod4))
cplot(mod4, x="age", se.type = "shade")

# Consecuences of misspecification: Monte Carlo Simulation

ameage <- matmod4.1[1,2]


# Treatment effects

mod4glm <- glm(exp_tot~age*female+anylim, data=sdata)
smod4glm <- summary(mod4glm)
m4anylim.lev <- marg(mod=mod4glm, var_interest = "anylim", type="levels")
m4anylim.eff <- marg(mod=mod4glm, var_interest = "anylim", type="effects")
