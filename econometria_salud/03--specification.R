library(foreign)

data <- read.dta("https://www.dropbox.com/s/fa4nqsnc8bveh1s/heus_mepssample.dta?dl=1")
sdata <- data[which(data$exp_tot>0),]

# Recode female and anylim as 0, 1 variables

sdata$female <- as.factor(as.numeric(sdata$female=="Female"))
sdata$anylim <- as.factor(as.numeric(sdata$anylim=="Activity limitation"))

# Estimar por MCO

mod.lrm <- lm(exp_tot~age*female+anylim, data=sdata)
smod.lrm <- summary(mod.lrm)

# Marginal and incremental effects

library(margins)

mmod.lrm <- summary(margins(mod.lrm))
matmod.lrm1 <- summary(margins(mod.lrm, variables="age"))
matmod.lrm2 <- summary(margins(mod.lrm, at=list(female=c("0", "1")), variables="age"))
matmod.lrm3 <- summary(margins(mod.lrm, at=list(age=c(20,45,70)), variables="female"))

# Margins plot

plot(margins(mod.lrm))
cplot(mod.lrm, x="age", se.type = "shade")

# Treatment effects: using predict

anylim.0 <- data.frame(X=1, age=mean(sdata$age), female=as.factor(1),anylim=as.factor(0))
anylim.1 <- data.frame(X=1, age=mean(sdata$age), female=as.factor(1),anylim=as.factor(1))
treatment <- predict(mod.lrm, newdata=anylim.1)-predict(mod.lrm, newdata=anylim.0)

# Treatment effects: using generalized linear estimation

library(modmarg)

mod.glm <- glm(exp_tot~age*female+anylim, data=sdata)
smod.glm <- summary(mod.glm)
manylim.lev <- marg(mod=mod.glm, var_interest = "anylim", type="levels")
manylim.eff <- marg(mod=mod.glm, var_interest = "anylim", type="effects")

# Visual checks: Residual plots
# Example 1

mod.1 <- lm(exp_tot~age+lninc, data=sdata)
fit.m1<-fitted(mod.1)
res.m1 <-resid(mod.1)
plot(fit.m1, res.m1)
plot(sdata$age, res.m1)
plot(sdata$lninc, res.m1)

# Example 2
sdata$lexp_tot <- log(sdata$exp_tot)
mod.2 <- lm(lexp_tot~age+lninc, data=sdata)
fit.m2<-fitted(mod.2)
res.m2 <-resid(mod.2)
plot(fit.m2, res.m2)
plot(sdata$age, res.m2)
plot(sdata$lninc, res.m2)

# RESET test

library(lmtest)

resettest(mod.lrm, power=2:3, type="fitted")
