###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafa? Wo?niak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 Labs 05: Unordered choice models                        #
#                                                                         #
###########################################################################


Sys.setenv(LANG = "en")
setwd("C:\\Users\\Hp\\WNE\\Advanced_Econometrics\\AE_Lab_05")

library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("aod")

# install.packages("nnet")
library("nnet")

library("Formula")
library("miscTools")
library("maxLik")
# install.packages("mlogit")
library("mlogit")
library("car")
library("survival")
# install.packages("AER")
library("AER")
library("stargazer")

# ------------------------------------------
# Lecture slides
# ------------------------------------------

options(scipen = 999)

# ********************************************
# The other way of estimating multinomial 
# logit models
# This method does not allow us to obtain
# marginal effects

fish = read.csv(file="Fishing_mode.csv", sep=",", header=TRUE)
View(fish[1:5,])
fish$mode = as.factor(fish$mode)
fish$income2 = fish$income^2

# descriptive statistics
summary(fish)

# multinomial model
mlogit = multinom(mode~income+income2, data=fish)
summary(mlogit)

# statistical significance
z <- summary(mlogit)$coefficients/summary(mlogit)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

stargazer(mlogit, type = "text")

# ********************************************
# The easy way to obtain marginal effects are
# presented in Exercise 1


# ------------------------------------------
# Exercise 1
# ------------------------------------------

# multinomial model
data("Fishing", package = "mlogit")

# income in thousands of dollars
Fishing$income = Fishing$income/1000

# change format of the data
Fish <- mlogit.data(Fishing, shape="wide", choice="mode", varying=2:9)
# View(Fish[1:10,])

## a pure "multinomial model"
mlogit1 = mlogit(mode ~ 0 | income, data = Fish)
options(scipen=999)
summary(mlogit1)
stargazer(mlogit1, type = "text")

# compute a data.frame containing the mean value 
# of the covariates in the sample
z <- with(Fish, data.frame(income = 
                             tapply(income, index(mlogit1)$alt, mean)))
print(z)

# compute the marginal effects for average characteristics
# impact of an additional thousand of dollars
effects(mlogit1, covariate = "income", data = z)

# compute the average marginal effects
AME = matrix(0, nrow=1182, ncol=4)
# nrow = nrow(Fishing) # Fishing not Fish
# ncol = the number of alternatives

for(iter in 1:1182) {
  income = Fish$income[Fish$chid==iter]
  z = data.frame(income = tapply(income, index(mlogit1)$alt[Fish$chid==iter], mean))
  AME[iter, ] = effects(mlogit1, covariate = "income", data = z)
}

AvMargEff = colMeans(x=AME)
print(AvMargEff)


# independence from irrelevant alternatives assumption
mlogit1 = mlogit(mode ~ 0 | income, data = Fish, reflevel="beach")
mlogit2 = mlogit(mode ~ 0 | income, data = Fish, reflevel="beach", alt.subset=c("beach", "boat", "charter"))
mlogit3 = mlogit(mode ~ 0 | income, data = Fish, reflevel="beach", alt.subset=c("beach", "boat", "pier"))
mlogit4 = mlogit(mode ~ 0 | income, data = Fish, reflevel="beach", alt.subset=c("beach", "charter", "pier"))
mlogit5 = mlogit(mode ~ 0 | income, data = Fish, reflevel="pier")
mlogit6 = mlogit(mode ~ 0 | income, data = Fish, reflevel="pier", alt.subset=c("pier", "charter", "boat"))
# compute the test
hmftest(mlogit1, mlogit2) # pier alternative H0: it does not violate the IIA assumption
hmftest(mlogit1, mlogit3) # H0: charter does not violate the assumption
hmftest(mlogit1, mlogit4) # H0: boat alt. does not violate the IIA
hmftest(mlogit5, mlogit6)

# let's test the hypothesis that private boat and charter boat 
# alternatives might be combined into one category
# i.e. non-constant variables' parameters in these categories
# are equal to themselves
#
# here: boat:beta_income=charter:beta_income

# H0: income:boat = income:charter
# H0: income:boat - income:charter = 0

linearHypothesis(model=mlogit1, c("income:boat=income:charter"))

# we have to reject the null, that private boat and charter boat
# alternatives cannot be combined into one category


# let's test the hypothesis that private boat, charter boat and pier
# alternatives might be combined into one category
# i.e. non-constant variables' parameters in these categories
# are equal to themselves
#
# here: boat:beta_income=charter:beta_income=pier:beta_income

# H0: income:boat = income:charter
#     income:charter = income:pier
# H0: income:boat - income:charter = 0
#     income:charter - income:pier = 0

linearHypothesis(model=mlogit1, c("income:boat = income:charter", 
                                  "income:charter = income:pier"))
# Conclusion?


# joint insignificance of all variables
# using the likelihood ratio test
mlogit1 = mlogit(mode ~ 0 | income, data = Fish)
Fish$chid = NULL
mlogitr = mlogit(mode ~ 1 | 1, data = Fish)
# In case of error: Error in 1:nchid : result would be too long a vector
# The joint insignificance test can benefit from the multinom function
# Alternatively, we can use the final line of the mlogit output
mlogitr = multinom(mode~1, data=Fishing)
lrtest(mlogit1, mlogitr)

# 'impure' conditional logit
mlogit1 = mlogit(mode ~ price+catch | income, data = Fish)
summary(mlogit1)

# marginal effects
z <- with(Fish, data.frame(price = tapply(price, index(mlogit1)$alt, mean),
                           catch = tapply(catch, index(mlogit1)$alt, mean),
                           income = tapply(income, index(mlogit1)$alt, mean)))
z
# compute the marginal effects
# impact of an additional thousand of dollars
effects(mlogit1, covariate = "income", data = z)

# marginal effect for price
effects(mlogit1, covariate = "price", data = z)

# marginal effect for catch rate
effects(mlogit1, covariate = "catch", data = z)

# ------------------------------------------
# Exercise 2
# ------------------------------------------


# 'pure' conditional logit -- cola dataset
cola = read.csv(file='cola.csv', sep=",", header=TRUE)
View(cola[1:5,])

# data preparation
cola$soda = 0
cola$soda[cola$pepsi==1] = 'pepsi'
cola$soda[cola$coke==1] = 'coke'
cola$soda[cola$sevenup==1] = 'sevenup'
View(cola)

names(cola) = c("id","pepsi","sevenup","coke","price.pepsi","price.sevenup","price.coke",
                "feat.pepsi","feat.sevenup","feat.coke",
                "disp.pepsi","disp.sevenup","disp.coke","soda")
# names with "." are necessary for mlogit.data function

# mlogit.data
cola2 <- mlogit.data(cola, shape="wide", choice="soda", varying=5:13)
View(cola2)
# varying - which variables are case-sensitive

# 'pure' conditional logit model
mlogit1 = mlogit(soda~feat+disp+price|0, data=cola2)
summary(mlogit1)

# marginal effects
z <- with(cola2, data.frame(feat=tapply(feat, index(mlogit1)$alt, mean),
                            disp=tapply(disp, index(mlogit1)$alt, mean),
                            price=tapply(price, index(mlogit1)$alt, mean)))
print(z)
# It does not make any sense to consider
# values other than zeros and ones for 
# dummy independent variables
# 
# That it is why we have to change some
# elements of the vector z to zeros

z[,1:2] = 0
print(z)

effects(mlogit1, covariate = "price", data = z)
effects(mlogit1, covariate = "disp", data = z)
effects(mlogit1, covariate = "feat", data = z)



# ------------------------------------------
# Exercise 3
# ------------------------------------------

# data
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, shape="wide", choice="mode", varying=2:9)
View(Fish[1:10,])

# 'impure' conditional logit
mlogit1 = mlogit(mode ~ price+catch | income, data = Fish)
summary(mlogit1)

# marginal effects
z <- with(Fish, data.frame(price = tapply(price, index(mlogit1)$alt, mean),
                           catch = tapply(catch, index(mlogit1)$alt, mean),
                           income = tapply(income, index(mlogit1)$alt, mean)))
z
# compute the marginal effects
# impact of an addtional dollar
effects(mlogit1, covariate = "income", data = z)
# marginal effect for price
effects(mlogit1, covariate = "price", data = z)
# marginal effect for catch rate
effects(mlogit1, covariate = "catch", data = z)



# ------------------------------------------
# Exercise 4
# ------------------------------------------



# ------------------------------------------
# Exercise 5
# ------------------------------------------

# read the data
car = read.csv(file="car_choice.csv", sep=",", header=TRUE)
View(car)

# "impure" condictional logit
mlogit = mlogit(choice~dealer|income, data=car, shape="long",
                chid.var="id", alt.var="car", choice="choice")
summary(mlogit)

# ------------------------------------------
# Exercise 6
# ------------------------------------------

# read the data
dane = read.csv(file="fmld142_part.csv", header=TRUE, sep=",")

# multinom function from nnet library
mlogit = multinom(empltyp1~age_ref+as.factor(sex_ref)+fam_size+as.factor(marital1), 
                  data=dane)
summary(mlogit)
stargazer(mlogit, type="text")

# MARITAL	3	Divorced
# MARITAL	1	Married
# MARITAL	5	Never married
# MARITAL	4	Separated
# MARITAL	2	Widowed

# preparing data for mlogit.data
table(dane$empltyp1)
dane_mlogit = mlogit.data(dane, choice="empltyp1", shape="wide", varying=NULL)
View(dane_mlogit)

# mlogit function from mlogit library
mlogit1 = mlogit(empltyp1~0|age_ref+as.factor(sex_ref)+fam_size+as.factor(marital1), data=dane_mlogit)
summary(mlogit1)
stargazer(mlogit1, type="text")






