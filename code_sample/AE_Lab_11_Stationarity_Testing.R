###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafa≥ Woüniak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#   Materials based on dr Piotr Wojcik Time Series Analysis for QF        #
#                                                                         #
#                 Lab 11: Stationarity testing                            #
#                                                                         #
###########################################################################

# We start with defining path to current working directory

setwd("C:\\Users\\Rafal\\WNE\\Advanced_Econometrics\\AE_Lab_10")

# install and load needed libraries

install.packages("tseries")
install.packages("urca")
install.packages("fUnitRoots")


library(xts)
library(lmtest)
library(tseries)
library(urca)
library(fUnitRoots)

options(scipen=999)
Sys.setenv(LANG = "en")

###################################################
#  1. Differencing of non-stationary time series  
#     testing order of integration - ADF test     
###################################################

# lets load daily data for SP500

SP500 <- read.csv("SP500.txt",
                  sep = ",",
                  dec = ".",
                  header = T,
                  stringsAsFactors = FALSE)

head(SP500)

tail(SP500)

# lets convert dates into format understood by R as date
SP500$Date <- as.Date(as.character(SP500$Date), "%Y%m%d")

head(SP500)

# lets select just close price into the xts object
SP500 <- xts(SP500$Close, order.by=SP500$Date)

head(SP500)

# and assign a name to the column with a close price into SP500
names(SP500)[1] <- "SP500"

head(SP500)

# To make it easier the above steps have been put
# into a function import_data_into_xts() stored 
# in the file "function_import_data_into_xts.R"

# lets load it
source("function_import_data_into_xts.R")

# The function assumes that the data is stored in 
# the subdirectory called "data".
# It requires just one argument, which is the name 
# of the file (without .txt extension)

# lets import SP500 data again
# we remove the created object
rm(SP500)

# and import it with the function
SP500 <- import_data_into_xts("SP500")

head(SP500)

class(SP500)

# Plot of SP500 index
plot(SP500$SP500, main = "SP500 index")

# Basing on visual inspection of data we can say that 
# the time series is non-stationary 

# lets limit our data to days since the beginning of 2000

# it is easy and fast for xts objects:
# we put limiting dates into brackets: 
# ["start_date_or_just_year/end_date_or_just_year"]
# One of the end can be skipped if one puts a limit 
# only on one side
# for examples see here: 
# http://stackoverflow.com/questions/11871572/subsetting-tricks-for-xts-in-r


SP500 <- SP500["2000/",]

head(SP500)

plot(SP500$SP500, main = "SP500 index")

# First, we will conduct ADF test using R function ur.df()

# the test can be applied either to 

# DF test
# The first version of the Dickey-Fuller test
# Because it looks like a Random walk without a drift
df.test <- ur.df(SP500$SP500,       # vector tested
                 type = c("none"),  # constant deterministic 
                                    # component (variant 1 of the test)
                 lags = 0)   # lags for augmentation of ADF part

summary(df.test)

# Value of test-statistic is: 0.8863 
# 
# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau1 -2.58 -1.95 -1.62

# Critical region (-infinity,-1.95)
# H0: the series is non-stationary
#     it is integrated of order at least 1

# We cannot reject the null hypothesis.



# to be sure that conlusions may be drawn we need to check whether 
# there is no autocorrelation in residuals
resids_ <- df.test@testreg$residuals
bgtest(resids_~1, order = 1)
bgtest(resids_~1, order = 2)
bgtest(resids_~1, order = 3)
bgtest(resids_~1, order = 4)
bgtest(resids_~1, order = 5)

# tau statistic (-0.0233) is higher than the 5% critical value (-2.86)
# so we cannot reject the null about non-stationarity of SP500

# lets check ADF with 1 augmentation

# ADF test
adf.test <- ur.df(SP500$SP500, 
                  type = c("none"), 
                  # selectlags = c("Fixed", "AIC", "BIC")
                  lags = 1)   

summary(adf.test)

# Value of test-statistic is: 0.1542 0.5545 

# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau2 -3.43 -2.86 -2.57 HERE!
# phi1  6.43  4.59  3.78

# Critical value -2.86
# Critical region is (-infinity, -2.86)


# to be sure that conlusions may be drawn we need to check whether there is 
# no autocorrelation in residuals
resids_ <- adf.test@testreg$residuals
bgtest(resids_~1, order = 1)
bgtest(resids_~1, order = 2)
bgtest(resids_~1, order = 3)
bgtest(resids_~1, order = 4)
bgtest(resids_~1, order = 5)

# tau statistic (0.1542) is higher than the 5% critical value (-2.86)
# so we cannot reject the null about non-stationarity of SP500

# but in fact we do not know which of the above tests is correct.
# Maybe we should add even more augmentations?


# As an alternative, to perform ADF test we can use a function testdf()
#  written by the lecturer and defined in the file function_testdf.R
#  This approach has an advantage over ur.df() since it displays the
#  values of the Breusch-Godfrey statistic that tests for autocorrelation
#  of residuals in the testing equation. This gives us a hint, which
#  number of augmentations to choose.

source("function_testdf2.R")

# test.type = {"nc", "c", "ct"}
testdf2(variable = SP500$SP500, # vector tested
       test.type = "nc", # test type
       max.augmentations = 3, # maximum number of augmentations added
       max.order=3)  # maximum number of analysed autocorrelation


# if you want to see the numbers in a traditional notation
# we can put a "penalty" on the scientific notation
options(scipen = 4)

testdf2(variable = SP500$SP500, # vector tested
       test.type = "nc", # test type
       max.augmentations = 5,  # maximum number of augmentations added
       max.order=5)           # maximum order of residual lags for BG test


# For no augmentations (row 1 with augmentations = 0) 
# residuals are autocorrelated
# (BG test rejects H0 of no autocorrelation in residuals in all cases,
# i.e. for order 1 its p-value = 0.00003471589 <<< 0.05)

# Therefore we add one augmentation 
# (see second row with augmentations = 1).
# This is enough NOT to reject LACK of autocorrelation of order 1  
# (p_bg = 0.84688579206 >> 0.05)
# but for the rest orders p-values are below 5% significance level (however, above 1%)

#Two augmentations give us all p-values significantly above any reasonable significance level.
# p-values 0.98500157014 0.997153487580 0.979939791006  0.85021968490 0.273595879359

# As a result, we can see that the correct ADF statistic 
# is equal 0.27418797
# It's p-value = 0.9764016, so the test does NOT reject 
# H0 about NON-stationarity,
# so SP500 index is NOT stationary in the analyzed period


# In the second stage we repeat the ADF test on the first differences 
#  of the SP500 index to check if SP500 is integrated of order 1 or higher.

# lets use testdf() function on the first differences of the SP500 index
plot(diff.xts(SP500$SP500))

testdf2(variable = diff.xts(SP500$SP500), 
       test.type = "nc",
       max.augmentations = 3,max.order=2)
# p-value<5% threshold
# we have to reject the null hypothesis
# H0: variable is non-stationary
# diff(SP500) are stationary
# SP500 ~ I(1)

# For no augmentations (row 1 with augmentations = 0) 
# residuals are not autocorrelated on 1% alpha level
# (BG test does not reject H0 of no autocorrelation in residuals,
# order 1 p-value for BG test is p-value = 0.8474314 >> 0.05)

# For 1 augmentation (row 2 with augmentations = 1) 
# residuals are not autocorrelated on all reasonable levels
# (BG test does not reject H0 of no autocorrelation in residuals,
# p-values = 0.9854562     0.99726832     0.98161385     0.85661534     0.281822275)

# Therefore the correct (A)DF statistic is equal -50.17631
# It's p-value < 0.01 (see warnings), so the test 
# DOES reject H0 about non-stationarity,
# of the first differences of SP500 index.
# so dif(SP500) IS already STATIONARY

# Putting the results of the two above mentioned tests together:

# SP500 is NOT stationary 
# dif(SP500) is stationary

# conclusion: SP500 is integrated of order 1


## lets use alternative tests as well

# Phillips-Perron (PP) - similar to ADF, but with better power
# the null and alternative hypotheses are the same as in ADF
# H0: time series is not stationary (integrated of order >= 1)
# Ha: time series is stationary (integrated of order = 0)

pp.test <- ur.pp(SP500$SP500,           # tested series
                 type = c("Z-tau"),     # standardization of the test statistic
                 model = c("constant")) # constant deterministic component
                 # which means we assume that any trends in the data are stochastic

summary(pp.test)

# Value of the test-statistic Z-tau (0.363) is higher 
# than the 5% critical value (-2.862734)
# The critical region is (-infinity, -2.86)
# so we CANNOT reject the null about non-stationarity of SP500

# lets test SP500's first differences
pp.test.d <- ur.pp(diff.xts(SP500$SP500), 
                   type = c("Z-tau"), 
                   model = c("constant")) # constant deterministic component

summary(pp.test.d)

# Value of the test-statistic Z-tau (-70.7898) is lower than 
# the 5% critical value (-2.862734)
# so we reject the null about non-stationarity 
# of SP500's first differences

# so again finally SP500 ~ I(1)


# KPSS
# In KPSS test the null and alternative hypotheses are reversed
# as compared to ADF and PP
# KPSS
# H0: time series IS stationary (integrated of order = 0)
# Ha: time series is NON-stationary (integrated of order >= 1)

# In KPSS if the test statistic is higher than the critical value, 
# we reject the null hypothesis and when test statistic is lower 
# than the critical value, we cannot reject the null hypothesis.

kpss.test <- ur.kpss(SP500$SP500, 
                     type = c("mu")) # constant deterministic component
        
summary(kpss.test)

# Critical region (0.463, +infinity)

# the KPSS test statistic (21.8263) is higher than
# the 5% critical value (0.463)
# so we reject the null about STATIONARITY of SP500

# lets test SP500's first differences
kpss.test.d <- ur.kpss(diff.xts(SP500$SP500), 
                       type = c("mu")) # constant deterministic component
        
summary(kpss.test.d)

# Critical region (0.463, +infinity)
# Test statistic = 0.474

# In this case we would like to believe that KPSS test results 
# reflect the type-I error that we reject a true hypothesis.


# the KPSS test statistic (0.4739) is slightly higher than 
# the 5% critical value (0.463), but on the other hand 
# lower than 1% critical value
# so we CANNOT reject the null about
# STATIONARITY of SP500's first differences 
# on 1% significance level, but we slightly reject 
# the null at 5% level

# looking on the graph of diff(SP500)
# one can finally conclude that SP500 ~ I(1)

# conclusion: all tests give (almost) identical result: 
# SP500 ~ I(1)




######################################################################################################################
# Exercises 3

# Exercise 3.1 
# Using the function import_data_into_xts()
# import a selected data set for one of the time series 
# available in the subdirectory "data"

DAX = import_data_into_xts("DAX")


################ 
# Exercise 3.2
# Plot the figure of the series - does it look stationary?

plot(DAX)


################ 
# Exercise 3.3
# Limit the data starting from the beginning of 2005

DAX = DAX["2005/",]
plot(DAX)

################ 
# Exercise 3.4
# Test for stationarity and find integration order 
# of your series using the ADF test.
# Which order of the ADF test has to be used?
# What is the correct test statistic?
# What are the conclusions?

# no constant option
testdf2(variable = DAX$DAX, # vector tested
        test.type = "nc", # test type
        max.augmentations = 3, # maximum number of augmentations added
        max.order=3)
# DAX is non-stationary

testdf2(variable = diff.xts(DAX$DAX), # vector tested
        test.type = "nc", # test type
        max.augmentations = 3, # maximum number of augmentations added
        max.order=3)
# diff(DAX) are stationary

# DAX ~ I(1)

# --------------------------------------------------------------------------

# drift option
testdf2(variable = DAX$DAX, # vector tested
        test.type = "c", # test type
        max.augmentations = 3, # maximum number of augmentations added
        max.order=3)
# DAX is non-stationary

testdf2(variable = diff.xts(DAX$DAX), # vector tested
        test.type = "c", # test type
        max.augmentations = 3, # maximum number of augmentations added
        max.order=3)
# diff(DAX) are stationary

# DAX ~ I(1)

################ 
# Exercise 3.5
# Test for stationarity and find integration order 
# of your series using the PP test.
# What is the correct test statistic?
# What are the conclusions?

pp.test = ur.pp(DAX$DAX,           # tested series
                type = c("Z-tau"),     # standardization of the test statistic
                model = c("constant"))
summary(pp.test)
# DAX is integrated

pp.test = ur.pp(diff.xts(DAX$DAX),           # tested series
                type = c("Z-tau"),     # standardization of the test statistic
                model = c("constant"))
summary(pp.test)
# I reject the null.
# diff(DAX) are stationary
# DAX ~ I(1)



################ 
# Exercise 3.6
# Test for stationarity and find integration order 
# of your series using the KPSS test.
# What is the correct test statistic?
# What are the conclusions?


kpss.test <- ur.kpss(DAX$DAX, 
                     type = c("mu")) # constant deterministic component

summary(kpss.test)

# Crtical region (0.463, +infinity)
# Value of test-statistic is: 22.4177
# We have to reject the H0: the variable is stationary

kpss.test <- ur.kpss(diff.xts(DAX$DAX), 
                     type = c("mu")) # constant deterministic component

summary(kpss.test)

# Crtical region (0.463, +infinity)
# Value of test-statistic is: 0.0784
# We cannot reject the H0: the variable is stationary
# diff(DAX) are stationary
# DAX ~ I(1)


