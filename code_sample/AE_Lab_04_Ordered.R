###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafa? Wo?niak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 Lab 04: Ordered choice models                           #
#                                                                         #
###########################################################################

setwd("C:\\Users\\Hp\\WNE\\Advanced_Econometrics\\AE_Lab_04")
Sys.setenv(LANG = "en")
options(scipen = 5)


##########################################################
# Ordered choice Models
#########################################################

# WNE package from Github
# install.packages("devtools")
library("devtools")

detach("package:WNE", unload = TRUE)
install_github("Rand-0/WNE")
library(WNE)


install.packages("pscl")
install.packages("ucminf")
install.packages("ordinal")
install.packages("reshape")
install.packages("generalhoslem")
install.packages("oglmx")
install.packages("brant")
install.packages("aod")


library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("LogisticDx")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")


# ------------------------------------------
# Exercise 1
# ------------------------------------------

rat = read.csv(file="Ratings.csv", header=TRUE, sep=",")
View(cbind(rat$rating83c, rat$ia83, rat$dia))

rr = rep(0, 98)
rr[which(rat$rating83c=="BA_B_C")]=2
rr[rat$rating83c=="BAA"]=3
rr[rat$rating83c=="AA_A"]=4
rr[rat$rating83c=="AAA"]=5

# Estimate ordered logit for \texttt{rating83c}.
# polr from MASS package
ologit = polr(as.factor(rr)~ia83+dia, data=rat)
# If I were interested in the ordered probit model
# polr(as.factor(rr)~ia83+dia, data=rat, method = "probit")
summary(ologit)
coeftest(ologit)

# Are \texttt{ia83} and \texttt{dia} jointly significant?
# Likelihood ratio test
ologit.restricted = polr(as.factor(rr)~1, data=rat)
lrtest(ologit, ologit.restricted)

# Interpret parameters' coefficients.

# Goodness-of-fit tests
lipsitz.test(ologit)
logitgof(rat$rating83c, fitted(ologit), g = 5, ord = TRUE)

# Brant's test
# the function works with polr model results
brant(ologit)
# As stated at:
# https://stats.idre.ucla.edu/stata/dae/ordered-logistic-regression/
# A significant test statistic provides evidence that the parallel
# regression assumption has been violated.
# 
# Significant result for ia83 indicates violation of the proportional
# odds assumption.
# The omnibus test results confirms violation of the assumption.
# We should apply a more advanced method.
# Generalized ordered logit model
# The stereotype model
# The continuation ratio model
# Cumulative Probit Models
# Cumulative Log-Log Links

# Pseudo-R2 statistics
pR2(ologit)


# ------------------------------------------
# Exercise 2
# ------------------------------------------

rd = read.csv(file="Randdata.csv", header=TRUE, sep=",")
View(rd[1:5,])

rd$health = 0
rd$health[rd$hlthp==1]=1
rd$health[rd$hlthf==1]=2
rd$health[rd$hlthg==1]=3
# let's remove erroneous observations
indeksy = which(rd$health==0)
rd = rd[-indeksy,]

# Estimate ordered logit for hstatus.
ologit = ologit.reg(as.numeric(health)~income+female+num, data=rd)
summary(ologit)

# Use oprobit.reg function for ordered probit model

# Pseudo-R2 statistics
pR2(ologit)
# does not work after ologit.reg
McFaddensR2.oglmx(ologit)

# Joint significance
# Likelihood ratio test
rd$health = as.factor(rd$health)
ologit.unrestricted = polr(health~income+female+num, data=rd)
ologit.restricted = polr(as.factor(health)~1, data=rd)
lrtest(ologit.unrestricted, ologit.restricted)

# Small value of the parameter
coeftest(ologit)

# goodness-of-fit tests
logitgof(rd$health, fitted(ologit.unrestricted), g = 10, ord = TRUE)
pulkrob.chisq(ologit.unrestricted, c("female"))
ologit.unrestricted = ologit.reg(health~income+female+num, data=rd)
lipsitz.test(ologit.unrestricted)
# for Lipsitz et al. test
# lipsitz.test works after polr function
rd$health = as.factor(rd$health)
ologit.unrestricted = polr(health~income+female+num, data=rd)
lipsitz.test(ologit.unrestricted)

# marginal effects
options(scipen=999)
margins.oglmx(ologit)

# marginal effects for a user-defined characteristics
# this function works with polr models
model = polr(as.factor(health)~income+female+num, data=rd, method="logistic")
summary(model)

# or a simpler way to do the same
marginaleffects(model, characteristics = atFUN(mean))
marginaleffects(model, characteristics = atFUN(median))
marginaleffects(model, characteristics = atFUN(min))
marginaleffects(model, characteristics = atFUN(max))
marginaleffects(model, characteristics = c(10000, 1, 1))
marginaleffects(model, characteristics = c("income"=2000, "female"=1, "num"=3))

# income affects cubicly than linearly
rd$income2 = rd$income^2
rd$income3 = rd$income^3

# likelihood ratio test
model_R = polr(as.factor(health)~income+female+num, data=rd)
model_U = polr(as.factor(health)~income+income2+income3+female+num, data=rd)
lrtest(model_U, model_R)
# H0: income2=0 and income3=0
# p-value < 5%, so I reject the null hypothesis


# ------------------------------------------
# Exercise 3
# ------------------------------------------



# ------------------------------------------
# Exercise 4
# ------------------------------------------

nls = read.csv(file="nlsw88.csv", sep=",", header=TRUE)
nls$union[nls$union==""]=NA
nls = na.omit(nls)
View(nls[1:5,])

# let's prepare all variables we will need
nls$white = 0
nls$white[nls$race=="white"] = 1
nls$black = 0
nls$black[nls$race=="black"] = 1
nls$agriculture = 0
nls$business = 0
nls$construction = 0
nls$entertainment = 0
nls$transport = 0
nls$finance = 0
nls$profserv = 0
nls$manufacturing = 0
nls$mining = 0
nls$perserv = 0
nls$public = 0
nls$trade = 0

nls$agriculture[nls$industry=="Ag/Forestry/Fisheries"] = 1
nls$business[nls$industry=="Business/Repair Svc"] = 1
nls$construction[nls$industry=="Construction"] = 1
nls$entertainment[nls$industry=="Entertainment/Rec Svc"] = 1
nls$finance[nls$industry=="Finance/Ins/Real Estate"] = 1
nls$manufacturing[nls$industry=="Manufacturing"] = 1
nls$mining[nls$industry=="Mining"] = 1
nls$perserv[nls$industry=="Personal Services"] = 1
nls$profserv[nls$industry=="Professional Services"] = 1
nls$public[nls$industry=="Public Administration"] = 1
nls$transport[nls$industry=="Transport/Comm/Utility"] = 1
nls$trade[nls$industry=="Wholesale/Retail Trade"] = 1

# Step 1
# general model
reg1 = lm(wage~age+as.factor(race)+married+grade+south+union+hours+
            ttl_exp+tenure+as.factor(industry), data=nls)
summary(reg1)

# test whether all insignificant variables all jointly insignificant
reg1a = lm(wage~age+white+grade+south+union+
             ttl_exp+tenure+construction+transport+finance, data=nls)
anova(reg1, reg1a)
# all insignificant variables are jointly significant
# therefore we have to drop variables in the way one after another
# let's drop "the most insignificant" variable from reg1
# that is Wholesale/Retail trade

# Step 2
reg2 = lm(wage~age+as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            agriculture+business+construction+entertainment+
            finance+manufacturing+mining+perserv+profserv+public+
            transport, data=nls)
summary(reg2)
# there are still insignificant variables in reg2 model
# let's drop "the most insignificant" variable from reg2
# that is perserv
# Can we?
# let's check
# let's estimate model reg2 without perserv
# and test joint hypothesis: beta_trade=beta_perserv=0
# in the general model that is model reg1
aux.reg = lm(wage~age+as.factor(race)+married+grade+south+union+
               hours+ttl_exp+tenure+
               agriculture+business+construction+entertainment+
               finance+manufacturing+mining+profserv+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null that
# beta_trade=beta_perserv=0
# in the general model that is model reg1
# therefore: we can drop agriculture in reg2 model

# Step 3
reg3 = lm(wage~age+as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            agriculture+business+construction+entertainment+
            finance+manufacturing+mining+profserv+public+
            transport, data=nls)
summary(reg3)
# we would like to drop age from reg3
# to do so, we have to verify joint hypothesis that
# beta_trade=beta_perserv=beta_age=0
aux.reg = lm(wage~as.factor(race)+married+grade+south+union+
               hours+ttl_exp+tenure+
               agriculture+business+construction+entertainment+
               finance+manufacturing+mining+profserv+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so age might be dropped from reg3 model

# Step 4
reg4 = lm(wage~as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            agriculture+business+construction+entertainment+
            finance+manufacturing+mining+profserv+public+
            transport, data=nls)
summary(reg4)
# we would like to drop agriculture from reg4
# to do so, we have to verify joint hypothesis that
# beta_trade=beta_perserv=beta_age=beta_agriculture=0
aux.reg = lm(wage~as.factor(race)+married+grade+south+union+
               hours+ttl_exp+tenure+
               business+construction+entertainment+
               finance+manufacturing+mining+profserv+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so agriculture might be dropped from reg4 model

# Step 5
reg5 = lm(wage~as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            business+construction+entertainment+
            finance+manufacturing+mining+profserv+public+
            transport, data=nls)
summary(reg5)
# we would like to drop mining from reg5
# to do so, we have to verify joint hypothesis that
# beta_trade=beta_perserv=beta_age=beta_agriculture=beta_mining=0
aux.reg = lm(wage~as.factor(race)+married+grade+south+union+
               hours+ttl_exp+tenure+
               business+construction+entertainment+
               finance+manufacturing+profserv+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so minig might be dropped from reg5 model

# Step 6
reg6 = lm(wage~as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            business+construction+entertainment+
            finance+manufacturing+profserv+public+
            transport, data=nls)
summary(reg6)
# we would like to drop profserv from reg6
# to do so, we have to verify joint hypothesis that
# beta_trade=beta_perserv=beta_age=beta_agriculture=beta_mining=beta_profserv=0
aux.reg = lm(wage~as.factor(race)+married+grade+south+union+
               hours+ttl_exp+tenure+
               business+construction+entertainment+
               finance+manufacturing+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so profserv might be dropped from reg6 model

# Step 7
reg7 = lm(wage~as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            business+construction+entertainment+
            finance+manufacturing+public+
            transport, data=nls)
summary(reg7)
# we would like to drop hours from reg7
# to do so, we have to verify joint hypothesis that
# beta_trade=beta_perserv=beta_age=beta_agriculture=beta_mining=beta_profserv=
# =beta_hours=0
aux.reg = lm(wage~as.factor(race)+married+grade+south+union+
               ttl_exp+tenure+
               business+construction+entertainment+
               finance+manufacturing+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so hours might be dropped from reg7 model

# Step 8
reg8 = lm(wage~as.factor(race)+married+grade+south+union+
            ttl_exp+tenure+
            business+construction+entertainment+
            finance+manufacturing+public+
            transport, data=nls)
summary(reg8)
# we would like to drop married from reg8
# to do so, we have to verify joint hypothesis that
# beta_trade=beta_perserv=beta_age=beta_agriculture=beta_mining=beta_profserv=
# =beta_hours=beta_married=0
aux.reg = lm(wage~as.factor(race)+grade+south+union+
               ttl_exp+tenure+
               business+construction+entertainment+
               finance+manufacturing+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so married might be dropped from reg8 model

# Step 9
reg9 = lm(wage~as.factor(race)+grade+south+union+
            ttl_exp+tenure+
            business+construction+entertainment+
            finance+manufacturing+public+
            transport, data=nls)
summary(reg9)
# we would like to drop race:other from reg9
# to do so, we have to verify joint hypothesis that
# beta_profserv=beta_agriculture=beta_age=beta_mining=beta_perserv=
# =hours=trade=married=race:other=0
aux.reg = lm(wage~black+grade+south+union+
               ttl_exp+tenure+
               business+construction+entertainment+
               finance+manufacturing+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so race:other might be dropped from reg9 model

# Step 10
reg10 = lm(wage~black+grade+south+union+
             ttl_exp+tenure+
             business+construction+entertainment+
             finance+manufacturing+public+
             transport, data=nls)
summary(reg10)
# we would like to drop entertainment from reg10
# to do so, we have to verify joint hypothesis that
# beta_profserv=beta_agriculture=beta_age=beta_mining=beta_perserv=
# =hours=trade=married=race:other=0
aux.reg = lm(wage~black+grade+south+union+
               ttl_exp+tenure+
               business+construction+
               finance+manufacturing+public+
               transport, data=nls)
anova(reg1, aux.reg)
# we cannot reject the null, so entertainment might be dropped from reg10 model

# Step 11
reg11 = lm(wage~black+grade+south+union+
             ttl_exp+tenure+
             business+construction+
             finance+manufacturing+public+
             transport, data=nls)
summary(reg11)
# All variables are significant in this step, so
# this ends general-to-specific procedure.


# ------------------------------------------
# Exercise 5
# ------------------------------------------


