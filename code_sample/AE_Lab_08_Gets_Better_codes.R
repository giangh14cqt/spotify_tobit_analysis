
###########################################################################
#		Advanced Econometrics                                                 #
#   Spring semester                                                       #
#   dr Marcin Chlebus, dr Rafa≥ Woüniak                                   #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                                                                         #
#                 Lab 08: General-to-Specific Approach                    #
#                                                                         #
###########################################################################

Sys.setenv(LANG = "en")
setwd("C:\\Users\\rwozniak\\Desktop\\AE_Lab_08")

# prof. David F. Hendry
# Gets - the LSE method
# London School of Economics Method
# superior to specific-to-general approach

# Start: general model
# a) test all insignificant variables jointly
#     i) yes - then get rid of them at once
#     ii) no - then apply step-by-step approach

# Stop: final model without insignificant variables

# Specific-to-General Approach



# ------------------------------------------
# Exercise 1
# ------------------------------------------

nls = read.csv(file="nlsw88.csv", sep=",", header=TRUE)
nls$union[nls$union==""]=NA
nls = na.omit(nls)
View(nls[1:5,])

table(nls$industry)

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

general = lm(wage~age+as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            agriculture+business+construction+entertainment+
            finance+manufacturing+mining+perserv+profserv+public+
            transport+trade, data=nls)

# test whether all insignificant variables all jointly insignificant
reg1a = lm(wage~white+grade+south+union+
             ttl_exp+tenure+construction+transport+finance, data=nls)
anova(reg1, reg1a)
# H0: they are jointly insignificant
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
library("car")
linearHypothesis(general, c("trade=0", "perserv=0"))

# we cannot reject the null that
# beta_trade=beta_perserv=0
# in the general model that is model reg1
# therefore: we can drop perserv in reg2 model

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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0"))


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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0"))

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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0", "mining=0"))

# we cannot reject the null, so mining might be dropped from reg5 model

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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0", "mining=0", "profserv=0"))

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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0", "mining=0", "profserv=0",
                            "hours=0"))
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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0", "mining=0", "profserv=0",
                            "hours=0", "marriedsingle=0"))
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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0", "mining=0", "profserv=0",
                            "hours=0", "marriedsingle=0",
                            "as.factor(race)other=0"))
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
linearHypothesis(general, c("trade=0", "perserv=0", "age=0",
                            "agriculture=0", "mining=0", "profserv=0",
                            "hours=0", "marriedsingle=0",
                            "as.factor(race)other=0", "entertainment=0"))
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

library("stargazer")
stargazer(general, reg11, type="text")


# Step 2
reg1d = lm(wage~age+as.factor(race)+married+grade+south+union+
            hours+ttl_exp+tenure+
            agriculture+business+construction+entertainment+
            finance+manufacturing+mining+perserv+profserv+public+
            transport+trade, data=nls)
summary(reg1d)
stargazer(reg1d, reg11, type="text")

# ------------------------------------------
# Exercise 2
# ------------------------------------------

data = read.delim(file="Artificial_data.txt", header=TRUE)


# -----------------------------------------------------------------------------------
# Exercise 3
# General-to-specific procedure
# -----------------------------------------------------------------------------------

library("plm")

crime = read.csv(file="crime.csv", header=TRUE, sep=",")
# alternatively
crime = pdata.frame(crime, index=c("county", "year"))
View(crime)

# -----------------------------------------------------------------------
# Step 1
# -----------------------------------------------------------------------

# general model
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+ltaxpc+lwcon+lwfir,
            data=crime, model="within")
summary(fixed)

library("car")
linearHypothesis(model=fixed, c("lavgsen=0", "ldensity=0", "ltaxpc=0", "lwfir=0"))

# I remove them at once
fixed2 = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lpolpc+lwcon,
              data=crime, model="within")
summary(fixed2)

# -----------------------------------------------------------------------
# Step 2
# -----------------------------------------------------------------------
# general model without ldensity
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ltaxpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# general model
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+ltaxpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# ldensity and ltaxpc
h <- rbind(c(0,0,0,0,0,1,0,0,0), c(0,0,0,0,0,0,1,0,0))
wald.test.results = wald.test(b = coef(fixed), Sigma = vcov(fixed), L = h)
wald.test.results


# -----------------------------------------------------------------------
# Step 3
# -----------------------------------------------------------------------
# general model without ldensity and ltaxpc
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# general model
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+ltaxpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# ldensity and ltaxpc
h <- rbind(c(0,0,0,0,0,1,0,0,0), c(0,0,0,0,0,0,1,0,0), c(0,0,0,1,0,0,0,0,0))
wald.test.results = wald.test(b = coef(fixed), Sigma = vcov(fixed), L = h)
wald.test.results


# -----------------------------------------------------------------------
# Step 4
# -----------------------------------------------------------------------
# general model without ldensity and ltaxpc and lavgsen
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lpolpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# general model
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+ltaxpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# ldensity and ltaxpc
h <- rbind(c(0,0,0,0,0,1,0,0,0), c(0,0,0,0,0,0,1,0,0), c(0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,0,0,1))
wald.test.results = wald.test(b = coef(fixed), Sigma = vcov(fixed), L = h)
wald.test.results

# lwfir might be removed from the model

# -----------------------------------------------------------------------
# Step 5
# -----------------------------------------------------------------------
# general model without ldensity and ltaxpc and lavgsen and lwfir
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lpolpc+lwcon,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# general model
fixed = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+ltaxpc+lwcon+lwfir,
            data=crime, index=c("county", "year"), model="within")
summary(fixed)

# ldensity and ltaxpc
h <- rbind(c(0,0,0,0,0,1,0,0,0), c(0,0,0,0,0,0,1,0,0), c(0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,0,0,1))
wald.test.results = wald.test(b = coef(fixed), Sigma = vcov(fixed), L = h)
wald.test.results


