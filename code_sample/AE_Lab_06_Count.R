###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafa? Wo?niak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 Lab 06: Models for count data                           #
#                                                                         #
###########################################################################
# install.packages("installr")
# library(installr)
# updateR()

# install.packages("MASS")
# install.packages("pscl")
# install.packages("sandwich")
# install.packages("car")
# install.packages("lmtest")

library("MASS")
library("sandwich")
library("car")
library("lmtest")
library("pscl")

# install.packages("coin")
library(coin) #test
# install.packages("DescTools")
library(DescTools)


Sys.setenv(LANG = "en")
setwd("C:\\Users\\Hp\\WNE\\Advanced_Econometrics\\AE_Lab_06")

load(file="DebTrivedi.rda")

# Deb and Trivedi (1997) analyze data on 4406 individuals, aged 66 and over, who are covered
# by Medicare, a public insurance program. Originally obtained from the US National Medical
# Expenditure Survey (NMES) for 1987/88, the data are available from the data archive of 
# the Journal of Applied Econometrics at http://www.econ.queensu.ca/jae/1997-v12.3/deb-trivedi/. 
# It was prepared for an R package accompanying Kleiber and Zeileis (2008)
# and is also available as DebTrivedi.rda in the Journal of Statistical Software together with
# Zeileis (2006). 


# The objective is to model the demand for medical care - as captured by the
# number of physician/non-physician office and hospital outpatient visits - by the covariates
# available for the patients. 
# 
# dependent variable:
#   ofp - the number of physician office visits as the dependent variable 
# 
# independent vars:
#   hosp - number of hospital stays, 
#   health - self-perceived health status, 
#   numchron - number of chronic conditions, 
#   gender, 
#   school - number of years of education 
#   privins - private insurance indicator

table(DebTrivedi$health)

dt <- DebTrivedi[, c(1, 6:8, 13, 15, 18)]


#EDA
  #histogram of dependent variable /truncated poisson
  hist(dt$ofp, breaks = 0:90 -0.5)
  plot(table(dt$ofp))
  
  # does this distribution look similar to Poisson or Negative Binomial distirbution?
  # why not?
    # 1. tail-shape/truncated distribution/excess number of zeros? 
    # 2. fat tails - overdispersion

  
  #lets see how poisson distributions looks like with n from DT dtatset and different lambda params
  hist(rpois(n=4406, lambda=1), breaks = 0:90 -0.5)
  hist(rpois(n=4406, lambda=2), breaks = 0:90 -0.5)
  hist(rpois(n=4406, lambda=5), breaks = 0:90 -0.5)
  # with a mean from sample
  mean(dt$ofp)
  hist(rpois(n=4406, lambda=5.774399), breaks = 0:90 -0.5) 
  hist(rpois(n=4406, lambda=10), breaks = 0:90 -0.5)
  hist(rpois(n=4406, lambda=50), breaks = 0:90 -0.5)
  
  #and negative binomial?
  #note: theta - dispersion parameter = 1 - geometric distibution
  
  hist(rnegbin(n=4406, mu=1,theta=1.2), breaks = 0:90 -0.5) 
  hist(rnegbin(n=4406, mu=2,theta=1.2), breaks = 0:90 -0.5) 
  hist(rnegbin(n=4406, mu=5.774399,theta=1.2), breaks = 0:90 -0.5) 
  hist(rnegbin(n=4406, mu=10,theta=1.2), breaks = 0:90 -0.5)
  hist(rnegbin(n=4406, mu=50,theta=1.2), breaks = 0:500 -0.5)
  # max(rnegbin(n=4406, mu=50,theta=1.2))
  
  #is this variable coming from Poisson?
  #and Negative Binomial?
  par(mfrow=c(3,1))
  hist(dt$ofp, breaks = 0:90 -0.5)
  hist(rpois(n=4406, lambda=5.774399), breaks = 0:90 -0.5) 
  hist(rnegbin(n=4406, mu=5.774399,theta=1.2), breaks = 0:90 -0.5)
  

    
    
  #univariate relationships between depvar and regressors
    par(mfrow=c(1,1))
    plot(dt$ofp ~ dt$numchron, data = dt)
    #difficult to interpret as many points with the same value of independent variable
    
    # As there are zero counts as well, we use a convenience function
    # clog() providing a continuity-corrected logarithm.
    clog <- function(x) log(x + 0.5)
    
    # For transforming a count variable to a factor (for visualization purposes only), we define
    # another convenience function cfac()
    
    x<-dt$numchron
    cfac <- function(x, breaks = NULL) {
      if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
      x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
      levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
                          c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                           sep = "")
      return(x)
      }
    
    #insted of scatter plot - box plot with aggregated independent variable - 
      #better interpretation
        plot(clog(dt$ofp) ~ cfac(dt$numchron), data = dt)
      # conclusions?
      # with higher number of chronic diseases higher number of visits
        # library(vcd) #assocstats
        library(coin) #test
        library(DescTools)
        
        Tabla = as.table(table(dt$ofp,dt$numchron))
        
        # Sommers D
        # a measure of association for ordinal factors in a two-way table
        SomersDelta(Tabla, direction="column", conf.level=0.95)
        
        # direction="column" or "row" - Sommers' D is not symmetric
        # "row" calculates Somers' D (R | C) ("column dependent").
        
        # Kendall's tau-b - correlation coefficient
        # Calculate Kendall's tau-b statistic, a measure of association for ordinal factors in a two-way table.
        KendallTauB(Tabla, direction="column", conf.level=0.95)
        
        # Goodman Kruskal's Gamma
        # Calculate Goodman Kruskal's Gamma statistic, a measure of association for ordinal factors in a two-way table.
        GoodmanKruskalGamma(Tabla, direction="column", conf.level=0.95)
        
        
        #linear-by-linear test - formal test for univariate relation
        prop.table(Tabla,  margin = NULL)   ### proportion in the table
        spineplot(Tabla)
        LxL = lbl_test(Tabla)
        LxL
        
       #plots for all indepvars  
         plot(clog(ofp) ~ factor(dt$health,levels=c("poor","average","excellent"),labels("poor","average","excellent"),order=T), data = dt, varwidth = TRUE)
         plot(clog(ofp) ~ cfac(numchron), data = dt)
         plot(clog(ofp) ~ privins, data = dt, varwidth = TRUE)
         plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = dt)
         plot(clog(ofp) ~ gender, data = dt, varwidth = TRUE)
         plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = dt, breaks = 9)
       
       # conclusions?
         
         
         
    #########################################
         
    ### POISSON
         
    #########################################    
         
         
         fm_pois <- glm(ofp ~ ., data = dt, family = poisson)
         
         summary(fm_pois)
         
         # not a formal rule
         # abs(coeff)>0.5 => [exp(coeff)-1]*100%
         # 0.1 instead of 0.5
         (exp(summary(fm_pois)$coefficients[,1])-1)*100
         
         
         # All coefficient estimates confirm the results from the univariate exploratory analysis. 
         # All coefficients are highly significant with the health variables leading to somewhat larger Wald
         # statistics compared to the socio-economic variables. 
         # However, the Wald test results might be too optimistic due to a misspecification of the likelihood. 
         # 
         # As the exploratory analysis suggested that over-dispersion is present in this data set, we re-compute 
         # the Wald tests using sandwich standard errors via:
         
         coeftest(fm_pois, vcov = sandwich)
         mean(dt$ofp)
         var(dt$ofp)
         
         # over-dispersion problem Var(y|x)>>E(y|x)
         # a) robust estimator within Poisson model
         # b) negative-binomial regression
         # c) more advanced models
         
         # Poisson Var(y|x)=E(y|x)


     #########################################
     
     ### Negative binomial regression  
     
     #########################################
         
         fm_nbin <- MASS::glm.nb(ofp ~ ., data = dt)
         summary(fm_nbin)
         summary(fm_nbin)$aic
         
         # Table2
         cbind(summary(fm_pois)$coefficients[,1], summary(fm_qpois)$coefficients[,1], summary(fm_nbin)$coefficients[,1])
         
         # As shown in Table 2, both regression coeficients and standard errors are rather similar to the
         # quasi-Poisson and the sandwich-adjusted Poisson results above. Thus, in terms of predicted
         # means all three models give very similar results; the associated partial Wald tests also lead
         # to the same conclusions.
         # 
         # One advantage of the negative binomial model is that it is associated with a formal likelihood
         # so that information criteria are readily available. Furthermore, the expected number of zeros
         # can be computed from the fitted densities
         
         # comparison of poisson and negative binomial:
           summary(fm_pois)$aic
           summary(fm_nbin)$aic
           
           # dispersion parameter (1 - geometric distirbution, infinity - Poisson distirbution)
           fm_nbin$theta
           
           # Poisson nested in Negative Binomial with theta = Infinity
           # LR test can be applied
           options(scipen=100)
           # the test statistic
           (LRtest = 2*(logLik(fm_nbin) - logLik(fm_pois)))
           # LRtest ~ chi-squared distribution with g degrees of freedom
           # g = 1
           # p-value
           pchisq(LRtest, df = 1, lower.tail = FALSE)
           # H0: Poisson is equally good as negative binomial regresion
           #     Negative binomial model reduces to Poisson regression
           # Neg-Bin is more appropriate
           
   
     #########################################
     
     ### Hurdle model  
     
     #########################################
           hist(dt$ofp)
           table(dt$ofp)
           
           fm_hurdle0 <- hurdle(ofp ~ ., data = dt, dist = "negbin")   
           
           # This uses the same type of count data model as in the preceeding section but it is now
           # truncated for ofp < 1 and has an additional hurdle component modeling zero vs. count
           # observations. By default, the hurdle component is a binomial GLM with logit link which
           # contains all regressors used in the count model. The associated coefficient estimates and
           # partial Wald tests for both model components are displayed via
           
         
           summary(fm_hurdle0)
           
           # The coeficients in the count component resemble those from the previous models, but the
           # increase in the log-likelihood (see also Table 2) conveys that the model has improved by
           # including the hurdle component. However, it might be possible to omit the health variable
           # from the hurdle model. To test this hypothesis, the reduced model is fitted via:
             
          fm_hurdle <- hurdle(ofp ~ . | hosp + numchron + privins + school + gender, data = dt, dist = "negbin")
          summary(fm_hurdle)
          lrtest(fm_hurdle0, fm_hurdle)
          # H0: restrictions are correct
          #     healthexcellent = 0 & healthpoor = 0
          # the restricted model is more appropriate
          
          #conclusions? - we may ommit health in hurdle compomnent
            
          
          
    #########################################
    
    ### Zero-inflated regression  
    
    #########################################
          
          # always-zero group
          # not-always-zero group
          
          
          # Zero-Inflated Negative binomial regression (ZINB)
          fm_zinb0 <- zeroinfl(ofp ~ ., data = dt, dist = "negbin")
          
          summary(fm_zinb0)
          
          fm_zinb <- zeroinfl(ofp ~ . | hosp + numchron + privins + school + gender, data = dt, dist = "negbin")
          
          summary(fm_zinb)
          
          waldtest(fm_zinb0, fm_zinb)
          lrtest(fm_zinb0, fm_zinb)
          
          # comapraing ZINB and NB mode
          vuong(fm_zinb0,fm_nbin)
          # this test should not be USED
          
          install.packages("vcdExtra")
          library("vcdExtra")
          
          zero.test(dt$ofp)
          # H0: ZINB model reduces to NB
          
    # Comparison of models
          
          #cofficient comparison
          fm <- list("ML-Pois" = fm_pois, "NB" = fm_nbin,
                     "Hurdle-NB" = fm_hurdle, "ZINB" = fm_zinb)
          sapply(fm, function(x) coef(x)[1:8])
          
          # The result shows that there are some small differences, especially between the
          # GLMs and the zero-augmented models. However, the zero-augmented models have to be
          # interpreted slightly differently: 
          #   
          # While the GLMs all have the same mean function , 
          # the zero-augmentation also enters the mean function. 
          # 
          # Nevertheless, the overall impression is that the estimated mean functions are rather similar. 
          # 
          # Moreover, the associated estimated standard errors are very similar as well:
          
          cbind("ML-Pois" = sqrt(diag(vcov(fm_pois))),
                 "Adj-Pois" = sqrt(diag(sandwich(fm_pois))),
                 sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:8]))
          
            # The only exception are the model-based standard errors for the Poisson model, when treated
            # as a fully specified model, which is obviously not appropriate for this data set.
          
        # In summary, the models are not too different with respect to their fitted mean functions. The
        # differences become obvious if not only the mean but the full likelihood is considered:
          
          rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
                 Df = sapply(fm, function(x) attr(logLik(x), "df")),
                 AIC=sapply(fm, function(x) AIC(x,k=3))
                )
          
          BIC(fm_hurdle)
          BIC(fm_zinb)
          
          # The ML Poisson model is clearly inferior to all other fits. 
          # 
          # The quasi-Poisson model and the sandwich-adjusted Poisson model are not associated with a fitted likelihood. 
          # 
          # The negative binomial already improves the fit dramatically but can in turn be improved by the hurdle and
          # zero-inflated models which give almost identical fits. 
          # 
          # This also reflects that the over-dispersion in the data is captured better by the negative-binomial-based 
          # models than the plain Poisson model.
          
        # Additionally, it is of interest how the zero counts are captured by the various models.
        # Therefore, the observed zero counts are compared to the expected number of zero counts for
        # the likelihood-based models:
            
          round(c("Obs" = sum(dt$ofp < 1),
                   "ML-Pois" = sum(dpois(0, fitted(fm_pois))),
                   "NB" = sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)),
                   "NB-Hurdle" = sum(predict(fm_hurdle, type = "prob")[,1]),
                   "ZINB" = sum(predict(fm_zinb, type = "prob")[,1])))
          
          # Thus, the ML Poisson model is again not appropriate whereas the negative-binomial-based
          # models are much better in modeling the zero counts. 
          
          # By construction, the expected number of zero counts in the hurdle model matches the observed number.
          
        # In summary, the hurdle and zero-inflation models lead to the best results (in terms of likelihood)
        # on this data set. Above, their mean function for the count component was already
        # shown to be very similar, below we take a look at the fitted zero components:
          
          t(sapply(fm[4:5], function(x) round(x$coefficients$zero, digits = 3)))
          
          # This shows that the absolute values are rather different - which is not surprising as they
          # pertain to slightly different ways of modeling zero counts - but the signs of the coefficients
          # match, i.e., are just inversed. For the hurdle model, the zero hurdle component describes
          # the probability of observing a positive count whereas, for the ZINB model, the zero-inflation component predicts 
          # the probability of observing a zero count from the point mass component.
          
          # Overall, both models lead to the same qualitative results and very similar model fits. Perhaps
          # the hurdle model is slightly preferable because it has the nicer interpretation: there is one
          # process that controls whether a patient sees a physician or not, and a second process that
          # determines how many office visits are made.
          
        
          
          
          
          
          
          