# -*- coding: utf-8 -*-
"""
# Advanced Econometrics
# Spring semester
# Lab 07: Limited Dependent Variable models
# The code comes from the textbook by
# Florian Heiss, Daniel Brunner,
# Using Python for Introductory Econometrics
"""

# packages
import wooldridge as woo
import numpy as np
import patsy as pt
import scipy.stats as stats
import statsmodels.formula.api as smf
import statsmodels.base.model as smclass


# working directory
import os as os
os.getcwd()
os.chdir('C:\\Users\\Hp\\WNE\\Advanced_Econometrics\\AE_Lab_07')
os.getcwd()

# Mroz dataset on married women
mroz = woo.dataWoo("mroz")
y, X = pt.dmatrices("hours ~ nwifeinc + educ + exper +"
                    "I(exper**2)+ age + kidslt6 + kidsge6",
                    data=mroz, return_type="dataframe")

"""
  Obs:   753

  1. inlf                     =1 if in labor force, 1975
  2. hours                    hours worked, 1975
  3. kidslt6                  # kids < 6 years
  4. kidsge6                  # kids 6-18
  5. age                      woman's age in yrs
  6. educ                     years of schooling
  7. wage                     estimated wage from earns., hours
  8. repwage                  reported wage at interview in 1976
  9. hushrs                   hours worked by husband, 1975
 10. husage                   husband's age
 11. huseduc                  husband's years of schooling
 12. huswage                  husband's hourly wage, 1975
 13. faminc                   family income, 1975
 14. mtr                      fed. marginal tax rate facing woman
 15. motheduc                 mother's years of schooling
 16. fatheduc                 father's years of schooling
 17. unem                     unem. rate in county of resid.
 18. city                     =1 if live in SMSA
 19. exper                    actual labor mkt exper
 20. nwifeinc                 (faminc - wage*hours)/1000
 21. lwage                    log(wage)
 22. expersq                  exper^2

"""


# generate starting solution:
reg_ols = smf.ols(formula="hours ~ nwifeinc + educ + exper + I(exper**2) +"
                          "age + kidslt6 + kidsge6", data=mroz)
results_ols = reg_ols.fit()
sigma_start = np.log(sum(results_ols.resid ** 2) / len(results_ols.resid))
params_start = np.concatenate((np.array(results_ols.params), sigma_start),
                              axis=None)

# extend statsmodels class by defining nloglikeobs:
class Tobit(smclass.GenericLikelihoodModel):
    # define a function that returns the negative log likelihood per observation
    # for a set of parameters that is provided by the argument "params":
    def nloglikeobs(self, params):
        # objects in "self" are defined in the parent class:
        X = self.exog
        y = self.endog
        p = X.shape[1]
        # for details on the implementation see Wooldridge (2019), formula 17.22:
        beta = params[0:p]
        sigma = np.exp(params[p])
        y_hat = np.dot(X, beta)
        y_eq = (y == 0)
        y_g = (y > 0)
        ll = np.empty(len(y))
        ll[y_eq] = np.log(stats.norm.cdf(-y_hat[y_eq] / sigma))
        ll[y_g] = np.log(stats.norm.pdf((y - y_hat)[y_g] / sigma)) - np.log(sigma)
        # return an array of log likelihoods for each observation:
        return -ll
    
# results of MLE:
reg_tobit = Tobit(endog=y, exog=X)
results_tobit = reg_tobit.fit(start_params=params_start, maxiter=10000, disp=0)
print(f"results_tobit.summary(): \n{results_tobit.summary()}\n")

