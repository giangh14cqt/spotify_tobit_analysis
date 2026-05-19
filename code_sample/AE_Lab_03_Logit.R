###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafa? Wo?niak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 Lab 3: Binary choice models                            #
#                                                                         #
###########################################################################

setwd("C:\\Users\\Hp\\WNE\\Advanced_Econometrics\\AE_Lab_03")

Sys.setenv(LANG = "en")
options(scipen = 5)


# ------------------------------------------------------------
# Installing the libraries
# ------------------------------------------------------------

# WNE package from Github
install.packages("devtools")
library("devtools")

install_github("Rand-0/WNE")
library(WNE)

# BaylorEdPsych from CRAN archive repository
# https://cran.r-project.org/web/packages/BaylorEdPsych/index.html
install_version(package = "BaylorEdPsych", version = "0.5")

# LogisticDx from CRAN archive repository
# https://cran.r-project.org/web/packages/LogisticDx/index.html
install_version(package = "LogisticDx", version = "0.3")

# Standard CRAN Repository

# install.packages("htmltools")
# install.packages("logistf")
install.packages("mfx")

library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("BaylorEdPsych")
# library("htmltools")
library("LogisticDx")
library("aod")

library("logistf") #Firth's bias reduction method

# ------------------------------------------------------------
# Exercise 1
# ------------------------------------------------------------

oscar = read.csv2(file="Oscar.csv", header=TRUE, sep=";")
View(oscar)
oscar = na.omit(oscar)

# probit model estimation
myprobit <- glm(winner~nominations+gglobes, data=oscar, 
                family=binomial(link="probit"))

# predicted probabilities
oscar$prob = predict(myprobit,type=c("response"))
library(pROC)
g <- roc(winner ~ prob, data = oscar)
plot(g)    

summary(myprobit)
myprobit$coefficients
summary(myprobit)$coefficients[2,3]

# ------
# Ad. a)

# Joint insignificance of all variables test
null_probit = glm(winner~1, data=oscar, family=binomial(link="probit"))
lrtest(myprobit, null_probit)

# ------
# Ad. b)

# marginal effects for the average observation
(meff = probitmfx(formula=winner~nominations+gglobes, data = oscar, atmean=TRUE))

# marginal effect for user-defined observation
# source("marginaleffects.R")
user.def.obs = c(1,7.43,1.31) #convention: (intercept, x1, x2, ...)
marginaleffects(myprobit, user.def.obs)

user.def.obs = c(1,3,5) #convention: (intercept, x1, x2, ...)
marginaleffects(myprobit, user.def.obs)

user.def.obs = c(1,0,0) #convention: (intercept, x1, x2, ...)
WNE::marginaleffects(myprobit, user.def.obs)

# ------
# Ad. c)

# R-squared statistics
PseudoR2(myprobit)

# ------
# Ad. d)

# Linktest
# source("linktest.R")
linktest_result = WNE::linktest(myprobit)
print(linktest_result)
# The specification is correct.

# -----
# Ad. e) and f)
gof.results = gof(myprobit)
gof.results$gof

# -----
# Ad. g)
myprobit <- glm(winner~nominations+gglobes, data=oscar, 
                family=binomial(link="probit"))
summary(myprobit)

myprobit_restricted <- glm(winner~nominations, data=oscar, 
                           family=binomial(link="probit"))
summary(myprobit_restricted)

lrtest(myprobit, myprobit_restricted)
# Result?


# ------------------------------------------------------------
# Exercise 2
# ------------------------------------------------------------

# Data preparation
olympics = read.csv(file="olympics.csv", header=TRUE, sep=";")
olympics = na.omit(olympics)
indices = olympics$year==76
olympics = olympics[indices, ]
olympics$log.pop = log(olympics$pop)
olympics$log.gdp = log(olympics$gdp)
View(olympics)

# -----
# Ad. a

# generate ifgold variable
olympics$ifgold = olympics$gold
olympics$ifgold[olympics$ifgold>1] = 1
View(olympics)

# linear probability model
lpm = lm(ifgold~log.pop+log.gdp+host+planned, data=olympics)
summary(lpm)

# -----
# Ad. b

# specification test
resettest(lpm, power=2:3, type="fitted")

# -----
# Ad. c

# heteroscedasticity
lpm.residuals = lpm$residuals
plot(lpm.residuals~log.pop, data=olympics)
plot(lpm.residuals~log.gdp, data=olympics)

bptest(lpm.residuals~log.pop, data=olympics)
bptest(lpm.residuals~log.pop+log.gdp+host+planned, data=olympics)

View(lpm$fitted.values)

# -----
# Ad. d

# White's estimator of the variance-covariane matrix
robust_vcov = vcovHC(lpm, data = olympics, type = "HC")
coeftest(lpm, vcov.=robust_vcov)

# to compare the simple lpm and the one with a robust vcov matrix
library("stargazer")
robust.lpm = coeftest(lpm, vcov.=robust_vcov)
stargazer(lpm, robust.lpm, type="text")


# ------------------------------------------------------------
# Exercise 3
# ------------------------------------------------------------

vote2 = read.csv(file = "vote2.csv", header=TRUE, sep=";", dec = ",")
View(vote2)
str(vote2)

vote2$income2 = vote2$income^2
vote2$region3 = 0
vote2$region3[vote2$region==3] = 1

# ii. Use R to calculate the marginal effect numerically.
dem.probit = glm(dem~income+income2+region3, 
                 data=vote2, family=binomial(link="probit"))
summary(dem.probit)

user.def.obs = c(1,34,34^2,0) #convention: (intercept, x1, x2, ...)
marginaleffects(dem.probit, user.def.obs)


# iv. Test the hypothesis H0: beta1=beta2=0 using the Wald statistics
H <- rbind(c(0,1,0,0), c(0,0,1,0))

# h %*% coef(dem.probit)
wald.test.results = wald.test(b = coef(dem.probit), 
                              Sigma = vcov(dem.probit), L = H)
wald.test.results
# We have to reject the null hypothesis.

# v. Test the hypothesis H0: beta1=beta2=0 using the likelihood ratio test
dem.U = glm(dem~income+income2+region3, data=vote2, 
            family=binomial(link="probit"))
dem.R = glm(dem~region3, data=vote2, family=binomial(link="probit"))
lrtest(dem.U, dem.R)
# The same conclusion as before.

# vi. Estimate the full model
dem.full = glm(dem~income+income2+region3+density+HS+BA, data=vote2, 
               family=binomial(link="probit"))
#Warning message:
#  glm.fit: fitted probabilities numerically 0 or 1 occurred

table(vote2$dem)
table(vote2$dem, vote2$region3)

# https://stats.idre.ucla.edu/r/dae/logit-regression/
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logisticprobit-regression-and-how-do-we-deal-with-them/
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqwhat-is-complete-or-quasi-complete-separation-in-logistic-regression-and-what-are-some-strategies-to-deal-with-the-issue/

# Firth's bias reduction
fit = logistf(dem~income+income2+region3+density+HS+BA, data=vote2)
summary(fit)


# ------------------------------------------------------------
# Exercise 4
# ------------------------------------------------------------

womenwk = read.csv(file="womenwk.csv", header=TRUE, sep=",")
str(womenwk)

# let's generate variable of interest
womenwk$work = is.na(womenwk$wage)==FALSE

# student's own work ...


# ------------------------------------------------------------
# Exercise 6
# ------------------------------------------------------------

p = seq(from=0, to=1, by=0.001)
lnL = p^5*(1-p)^3
plot(p, lnL, type="l")
abline(v=5/8, col="red")

# install.packages("maxLik")
library("maxLik")

lnL = function(p) {
  l = 5*log(p)+3*log(1-p)
  return(l)
}

res = maxNR(fn=lnL, start=0.5)
summary(res)

curve(lnL(x), from = 0, to=1)
abline(v=res$estimate, col="blue")


# ------------------------------------------------------------
# Exercise 7
# ------------------------------------------------------------

oscar = read.csv2(file="Oscar.csv", header=TRUE, sep=";")
oscar = na.omit(oscar)
library("maxLik")

probit.glm = glm(winner~nominations+gglobes, data=oscar, 
                 family=binomial(link="probit"))
summary(probit.glm)
logLik(probit.glm)

loglik <- function(parameters) {
  beta0 = parameters[1]
  beta1 = parameters[2]
  beta2 = parameters[3]
  y = oscar$winner
  xb = beta0 + beta1*oscar$nominations + beta2*oscar$gglobes
  lnL = sum(y*log(pnorm(xb))) + sum((1-y)*log(1-pnorm(xb)))
  return(lnL)
}

res <- maxNR(loglik, start = c(0.1,0.1,0.1))
summary(res)

# comparison
tmp = cbind(probit.glm$coefficients, res$estimate)
colnames(tmp) = c("glm", "our code")
show(tmp)

