
###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafal Wozniak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 Lab 01: Undergraduate Econometrics                      #
#                                                                         #
###########################################################################


Sys.setenv(LANG = "en")
setwd("C:/Users/Hp/WNE/Advanced_Econometrics/AE_Lab_01")
options(scipen = 5)


###########################################################################
# Exercise 1
###########################################################################

cps = read.csv(file="cps_small.csv", sep=",", header=TRUE)
View(cps)

# let's generate logarithm of hourly wage
cps$lnWAGE = log(cps$wage)
# interaction term
cps$blackXfemale=cps$black*cps$female

# model - option 1
model1 = lm(lnWAGE~educ+female+black+blackXfemale, data=cps)
summary(model1)

# model - option 2
model2 = lm(I(log(wage))~educ+female+black+I(female*black), data=cps)
summary(model2)

# model - option 3
model3 = lm(I(log(wage))~educ+female+black+female:black, data=cps)
summary(model3)

# model - option 4
model4 = lm(I(log(wage))~educ+female*black, data=cps)
summary(model4)

# let's compare the results
install.packages("stargazer")
library("stargazer")

stargazer(model1, model2, model3, model4, type="text")
# better readibility
stargazer(model1, model2, model3, model4, type="text", df=FALSE,
          star.cutoffs=c(0.05, 0.01, 0.001))
# the models are equivalent to each other

summary(model1)
# Output interpretation:
# No sense interpretation of the constant term here.
# Having everything else constant, an additional year of schooling
# is connected to an increase in wage by 10.158%.
# Or more precisely, by (exp(0.101593)-1)*100%
# White women have lower wage than men by 25.57% ceteris paribus.
#
# Black women have lower wage than black men by (-0.255732+0.065381)*100%
# ceteris paribus.
# 
# Black women have lower wage than white women by (-0.184185+0.065381)*100%
# ceteris paribus.
# 
# R-squared statistic is equal to 0.2729
# The model explains 27.29% of the log-wage variation.
# Adjusted R-squared statistic is not interpretable.
# F-statistic = 93.37 and its corresponding p-value = 0
# let us to reject the null hypothesis that all variables
# (except the constant) are jointly insignificant.


###########################################################################
# Exercise 2
###########################################################################

stockton = read.csv(file="stockton2.csv", header=TRUE, sep=",")
stockton$stories2 = stockton$stories
stockton$vac_sto2 = stockton$vacant*stockton$stories
stockton$lnPRICE = log(stockton$price)

# (a) option 1
model1 = lm(lnPRICE~sqft+stories2+vacant+baths, data=stockton)
summary(model1)

# (a) option 2

# (b) option 1
model2 = lm(lnPRICE~sqft+I(sqft^2)+stories2+vacant+baths+stories2:vacant,
            data=stockton)
summary(model2)

# (b) option 2


###########################################################################
# Exercise 3
###########################################################################

stockton = read.csv(file="stockton2.csv", header=TRUE, sep=",")
stockton$stories2 = stockton$stories
stockton$vac_sto2 = stockton$vacant*stockton$stories
stockton$lnPRICE = log(stockton$price)

model = lm(I(log(price))~sqft+baths, data=stockton)
summary(model)

install.packages("lmtest")
library("lmtest")

# specification test
resettest(model, power=2:3, type="fitted")

# H0: homoscedasticity
# H1: heteroscedasticity

# Breusch's and Pagan's test
bptest(model, studentize=TRUE)

# White's test
bptest(model, ~sqft+baths+I(sqft^2)+I(baths^2)+
         sqft:baths, studentize=TRUE, data=stockton)

# Is the error term normally distributed?
install.packages("tseries")
library("tseries")

jarque.bera.test(model$residuals)


###########################################################################
# Exercise 4
###########################################################################

library(haven)
budgets <- read_dta("budgets.dta")
View(budgets)

model = lm(alcohol~income+expend+as.factor(location)+nop+price, data=budgets)
summary(model)

# let's generate the quality publication table
install.packages("dummies")
library("dummies")
TableOfDummies = as.data.frame(dummy(as.factor(budgets$location)))
colnames(TableOfDummies)=c("loc1","loc2","loc3","loc4","loc5","loc6")
budgets = cbind(budgets, TableOfDummies)
View(budgets)

# model1
model1 = lm(alcohol~income+expend+loc2+loc3+loc4+loc5+loc6+nop+price, data=budgets)
# loc3 removed
model2 = lm(alcohol~income+expend+loc2+loc4+loc5+loc6+nop+price, data=budgets)
# loc4 removed
model3 = lm(alcohol~income+expend+loc2+loc5+loc6+nop+price, data=budgets)
# loc6 removed
model4 = lm(alcohol~income+expend+loc2+loc5+nop+price, data=budgets)
# all loc# variables removed
model5 = lm(alcohol~income+expend+loc2+nop+price, data=budgets)

# Quality Publication Table
library("stargazer")
stargazer(model1, model2, model3, model4, model5, type="text", 
          align=TRUE, style="default", df=FALSE)

library("car")
linearHypothesis(model=model1, c("loc2=0", "loc3=0", "loc4=0", "loc5=0", "loc6=0"))
# in convention H*Beta=h
h = rep(0, times=5)
H = cbind(matrix(0, ncol=3, nrow=5), diag(5), matrix(0, ncol=2, nrow=5))
linearHypothesis(model=model1, rhs=h, hypothesis.matrix=H)


# hypothesis verification H0: beta_klm5 = -5
linearHypothesis(model=model1, c("loc5=-5"))
# or the hard way
(t_test = (summary(model1)$coefficients[7,1]-(-5))/summary(model1)$coefficients[7,2])
df = model1$df
# p-value
(p_value = 2*(1-pt(q=abs(t_test), df=df)))


###########################################################################
# Exercise 5
###########################################################################

install.packages("sandwich")
library("sandwich")
library("lmtest")

fertil = read.csv(file="fertil2.csv", sep=",", header=TRUE)
View(fertil)
fertil=na.omit(fertil)
# a)
OLS = lm(ceb~age+agefbrth+usemeth, data=fertil)
summary(OLS)
robust0 = coeftest(OLS, vcov.=NULL)
show(robust0)
# b)
bptest(OLS, studentize=TRUE)
# c) White's robust estimator
robust1 = coeftest(OLS, vcov.=vcovHC(OLS, type="HC0"))
show(robust1)
# d) MacKinnon's and White's robust estimator
robust2 = coeftest(OLS, vcov.=vcovHC(OLS, type="HC3"))
show(robust2)
# e) clustering
robust3 = coeftest(OLS, vcov.=vcovCL(OLS, cluster=fertil$children, type="HC0"))
show(robust3)
# f) quality publication table (qpt)

stargazer(robust0, robust1, robust2, robust3, type="text")

