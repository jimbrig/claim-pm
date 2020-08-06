library(car)
library(MASS)
library(dplyr)



# First need to load back in data w/ incremental pmnts
# between ages 18 and 30:

sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)
sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ] 
sample$status <- ifelse(sample$status == "C", 0, 1)  #setting Closed to to 0 and Open to 1
sample$status_1 <- ifelse(sample$status_1 == "C", 0, 1) #Status_1 represents clm status at 18 months devt
sample$status_2 <- ifelse(sample$status_2 == "C", 0, 1) #Status_2 represents clm status at 6 months devt
sample$status_2_1 <- ifelse(sample$status_1 == 0, 0, paste(sample$status_2, sample$status_1, sep = "_"))
sample$status_2_1 <- factor(sample$status_2_1)


sample <- sample[ , -c(1:4, 9:17, 28)]
names(sample)

sample$tot_pd_incr <- sample$bi_pd_incr_1 + sample$prop_pd_incr_1 + sample$exp_pd_incr_1


# Form training and testing samples
set.seed(1982) #use different seed than logistic regression so not overfitting data

rows <- sample(1:nrow(sample), nrow(sample) *0.75)
training <- sample[rows, ]
testing <- sample[-rows,]

training_open <- training[training$status == 1, ]
training_closed <- training[training$status == 0, ]

training_closed_1 <- training[training$status_1 == 0, ]

#Lets look at open first

summary(training_open$tot_pd_incr)
hist(training_open$tot_pd_incr)
boxplot(training_open$tot_pd_incr)
nonzeroes <- training_open[!training_open$tot_pd_incr == 0, ]
1889/2010 #94% of claims in dataset have incremental payments greater than zero

#Our response variable is heavily skewed with a lot of outliers

training_open$tot_pd_incr_log <- log(training_open$tot_pd_incr)


hist(nonzeroes$tot_pd_incr_log)
fit <- fitDist(nonzeroes$tot_pd_incr_log)
fit$fits #fits for lots of various distribution and their AICs

#for log of nonzero values, generalized t distribution fits well

histDist(tot_pd_incr_log, family = fit$family, data = nonzeroes)

names(nonzeroes)
nonzeroes <- nonzeroes[ , -c(1, 15, 17, 19, 21, 23, 25)]

fullmod_para <- gamlss(tot_pd_incr_log ~ rept_lag + suit_lag + suit + bi_rx_1 + bi_pd_1 + bi_coll_1
                         + prop_rx_1 + prop_pd_1 + prop_coll_1 + exp_rx_1 + exp_pd_1 
                         + exp_coll_1 + bi_rx_incr_1 + bi_pd_incr_1 + prop_rx_incr_1 
                         + prop_pd_incr_1 + exp_rx_incr_1 + exp_pd_incr_1 + status_2_1,
                          data = nonzeroes, family = fit$family)

initialmod_para <- gamlss(tot_pd_incr_log ~ bi_rx_1 + status_2_1, data = nonzeroes, family = fit$family)


mumodel <- stepGAIC(initialmod_para, direction = "forward", scope = list(lower =~1, upper = formula(fullmod_para)),
         what = "mu", mu.try = TRUE, sigma.try = FALSE, nu.try = FALSE, tau.try = FALSE)

summary(mumodel)
muform <- mumodel$mu.formula

sigmodel <- stepGAIC(mumodel, direction = "forward", scope = list(lower = ~1, upper = formula(fullmod_para)),
          what = "sigma", mu.try = FALSE, sigma.try = TRUE, nu.try = FALSE, tau.try = FALSE)

summary(sigmodel)
sigmodel <- gamlss(muform, sigma.formula =~exp_pd_1 + bi_pd_1 + status_2_1 
                   + bi_rx_1 + prop_pd_incr_1 + rept_lag + bi_rx_incr_1, 
                   family = fit$family, data = nonzeroes )
sigform <- sigmodel$sigma.formula

mod1 <- gamlss(tot_pd_incr_log ~ bi_rx_1 + status_2_1 + prop_pd_incr_1 + exp_pd_incr_1 
                  + rept_lag + prop_coll_1 + prop_pd_1, sigma.formula =~ exp_pd_1 + status_2_1 
                  + prop_pd_incr_1 + rept_lag, family = fit$family, data = nonzeroes)
summary(mod1) #bi_rx_1 not significant 
muform <- mod1$mu.formula
sigform <- mod1$sigma.formula

numodel <- stepGAIC(mod1, direction = "forward", scope = list(lower = ~1, upper = formula(fullmod_para)),
                     what = "nu", mu.try = FALSE, sigma.try = FALSE, nu.try = TRUE, tau.try = FALSE)  

summary(numodel)

mod1 <- gamlss(muform, sigma.formula = sigform, nu.formula =~ prop_pd_incr_1 + prop_pd_1, 
               family = fit$family, data = nonzeroes)
summary(mod1)
nuform <- mod1$nu.formula

mod1 <- gamlss(muform, sigma.formula = sigform, nu.formula = nuform, family = fit$family, data = nonzeroes)
summary(mod1)

taumodel <- stepGAIC(mod1, direction = "forward", scope = list(lower = ~1, upper = formula(fullmod_para)),
                     what = "tau", mu.try = FALSE, sigma.try = FALSE, nu.try = FALSE, tau.try = TRUE)

summary(taumodel)

mod1 <- gamlss(tot_pd_incr_log ~ status_2_1 + prop_pd_incr_1 + exp_pd_incr_1 + rept_lag + prop_pd_1,
               sigma.formula = sigform, nu.formula = nuform, tau.formula =~prop_pd_1 + bi_rx_incr_1 + exp_rx_1,
               family = fit$family, data = nonzeroes)

muform <- mod1$mu.formula
tauform <- mod1$tau.formula


mymodel <- gamlss(muform, sigma.formula = sigform, nu.formula = nuform, tau.formula = tauform,
                  family = fit$family, data = nonzeroes)

summary(mymodel)
plot(mymodel)
AIC(mymodel, fullmod_para, initialmod_para)
wp(mymodel)

pdf.plot(mymodel, min = 0, max = 15, step = .02)
histDist(tot_pd_incr_log, family = fit$family, data = nonzeroes)

testing_open <- testing[testing$status == 1, ]
testing_open$tot_pd_incr_log <- log(testing_open$tot_pd_incr)
testing_open$zeroes <- ifelse(testing_open$tot_pd_incr == 0, 1, 0)
  
nonzeroes$linear_preds <- predict(mymodel, what = "mu", type = "link")
nonzeroes$preds <- exp(nonzeroes$linear_preds)

mymodel2 <- gamlss(tot_pd_incr_log ~ status_2_1 + pb(bi_rx_1, method = "GAIC") + pb(prop_rx_1, method = "GAIC") + pb(exp_rx_1, method = "GAIC") + suit, family = fit$family, data = nonzeroes)

preds <- exp(predict(mymodel2, what = "mu", type = "response"))
sum(preds)

sum(nonzeroes$tot_pd_incr)

?predict
muform
sigmaform
nuform
tauform

mymodel_nonpara <- gamlss(tot_pd_incr_log ~   , sigma.formula =~  , nu.formula =~   , tau.formula =~   , 
                          family = fit$family, data = nonzeroes)
summary(mymodel_nonpara)



#check for interaction/ log / and non-parametric variables

scopes <- nonzeroes[, -c(13, 21)]
scopes$rept_lag_log <- log(scopes$rept_lag)
scopes$bi_rx_1_log <- log(scopes$bi_rx_1)
scopes$prop_rx_1_log <- log(scopes$prop_rx_1)
scopes$exp_rx_1_log <- log(scopes$exp_rx_1)
scopes$bi_pd_1_log <- log(scopes$bi_pd_1)
scopes$prop_pd_1_log <- log(scopes$prop_pd_1)
scopes$exp_pd_1_log <- log(scopes$exp_pd_1)
scopes$bi_pd_incr_1_log <- log(scopes$bi_pd_incr_1)
scopes$prop_pd_incr_1_log <- log(scopes$prop_pd_incr_1)
scopes$exp_pd_incr_1_log <- log(scopes$exp_pd_incr_1)

s <- gamlss.scope(scopes, response = 20, smoother = "pd")
s

addterm(mymodel, s, test = "Chisq")






fullmod_nonpara <- gamlss(tot_pd_incr_log ~ pb(rept_lag, method = "GAIC") + pb(bi_rx_1, method = "GAIC") 
             + pb(prop_pd_1, method = "GAIC") + pb(prop_coll_1, method = "GAIC") 
             + pb(exp_rx_1, method = "GAIC") + pb(bi_rx_incr_1, method = "GAIC")
             + pb(prop_pd_incr_1, method = "GAIC") + pb(exp_pd_incr_1, method = "GAIC")
             + factor(status_2_1), data = nonzeroes, sigma.formula =~pb(bi_rx_1, method = "GAIC")
             + pb(exp_rx_1, method = "GAIC") + factor(status_2_1), nu.formula =~pb(bi_rx_1, method = "GAIC")
             + pb(exp_rx_1, family = fit$family)


             
#need to estimate how many zeroes will occur:

training_open$zeroes <- ifelse(training_open$tot_pd_incr == 0, 1, 0)
fit2 <- fitDist(training_open$zeroes)

fit2$fits





#Lets look at the incremental payment distribution for each type of claim:

summary(training$bi_pd_incr)  #including zeroes
summary(training$bi_pd_incr[training$bi_pd_incr!=0])  #excluding zeroes, mean of $62,730

bi_nonzero <- training$bi_pd_incr[!training$bi_pd_incr == 0]
hist(bi_nonzero, breaks = 2000, freq = FALSE, xlim = c(0,200000))
#for bi_incr pmnts between ages 18 and 30 excluding clms with 0 pmnts, distribution is positive only and skewed right

hist(log(bi_nonzero[bi_nonzero > 0]), freq = FALSE)  #historgram of ln of non-zero incr pmnts looks almost normal
hist(1 / (bi_nonzero[bi_nonzero > 0]), breaks = 1000, freq = FALSE, xlim = c(0, .002)) 

#have a couple options for positive, skewed distributions:
# 1) Loglinear model (assumes normality)
# 2) GLM using gamma as underlying distribution for Y and reciprocal as link function
# 3) GLM using gamma as underlying distribution for Y and ln as link function

#To get total incremental payment projections for a claim, we will need 3 separate models for BI, Prop, and Expense
#Need a model for claims expected to be closed at age 30 and claims expected to be open at age 30:

# create separate training datasets for closed and open claims
training_open <- training[training$status == 1, ]

training_closed <- training[training$status == 0, ]

training_closed_1 <- training[training$status_1 == 0, ]  #claims that are closed as of age 18 assumed to have incr pmnts of zero


# Separate model for BI, Prop, and Exp Incremental Pmnts:

#Lets look at the incremental payment distribution for each type of claim:

summary(training_open$bi_pd_incr)  #including zeroes, mean of $32,930
summary(training_open$bi_pd_incr[training_open$bi_pd_incr!=0])  #mean of $132,600 for bi_open non zero pmnt clms

bi_open_nonzero <- training_open$bi_pd_incr[!training_open$bi_pd_incr <= 0]
hist(bi_open_nonzero, breaks = 2000, freq = FALSE, xlim = c(0,200000))
#for open bi_incr pmnts between ages 18 and 30 excluding clms with 0 pmnts, distribution is positive only and skewed right

hist(log(bi_open_nonzero), freq = FALSE)  #historgram of ln of non-zero incr pmnts looks almost normal
hist(1 / (bi_open_nonzero), breaks = 5000, freq = FALSE, xlim = c(0, .0005)) #skewed right like gamma dist.

#looks like a loglinear model would work well for bi_incrs, lets look at a model:

bi_open_fullmod <- lm(formula = bi_pd_incr ~ rept_lag + suit_lag + suit +bi_rx_1 + bi_pd_1 + bi_coll_1 + prop_rx_1 + prop_pd_1 
                      + prop_coll_1 + exp_rx_1 + exp_pd_1 + exp_coll_1 + bi_rx_incr_1 + bi_pd_incr_1 + prop_rx_incr_1 
                      + prop_pd_incr_1 + exp_rx_incr_1 + exp_pd_incr_1 + status_2_1, data = training_open)

bi_open_backwards = step(bi_open_fullmod) 

formula(bi_open_backwards)

summary(bi_open_backwards)  #everything significant except intercept (r -squared of .2842) 

vif(bi_open_backwards) #lots of multicollinearity problems to deal with

bi_open_backwards <- update(bi_open_backwards, .~.-bi_pd_1)
bi_open_backwards <- update(bi_open_backwards, .~.-exp_pd_incr_1)
bi_open_backwards <- update(bi_open_backwards, .~.-exp_rx_incr_1)
summary(bi_open_backwards)
vif(bi_open_backwards)
prop_open_fullmod <- lm(formula = prop_pd_incr ~ rept_lag + suit_lag + suit + bi_rx_1 + bi_pd_1 + bi_coll_1 + prop_rx_1 + prop_pd_1 
                        + prop_coll_1 + exp_rx_1 + exp_pd_1 + exp_coll_1 + bi_rx_incr_1 + bi_pd_incr_1 + prop_rx_incr_1 
                        + prop_pd_incr_1 + exp_rx_incr_1 + exp_pd_incr_1 + status_2_1, data = training_open)

exp_open_fullmod <- lm(formula = exp_pd_incr ~ rept_lag + suit_lag + suit + bi_rx_1 + bi_pd_1 + bi_coll_1 + prop_rx_1 + prop_pd_1 
                       + prop_coll_1 + exp_rx_1 + exp_pd_1 + exp_coll_1 + bi_rx_incr_1 + bi_pd_incr_1 + prop_rx_incr_1 
                       + prop_pd_incr_1 + exp_rx_incr_1 + exp_pd_incr_1 + status_2_1, data = training_open)


prop_open_backwards = step(prop_open_fullmod)
exp_open_backwards = step(exp_open_fullmod)

formula(prop_open_backwards)
formula(exp_open_backwards)

summary(prop_open_backwards) #intercept, prop_pd_incr_1, and status 1_1 not significant. 
#exp_rx_1 and status 0_1 semi-significant. (R-squared of .285)
summary(exp_open_backwards) #intercept and prop_rx_1 not significant, R-squared of .3723


# Remove insignificant predictors and multicollinear predictors

car::vif(bi_open_backwards) #exp_pd_1 and exp_pd_incr_1 too correllated
bi_open <- update(bi_open_backwards, ~.-exp_pd_incr_1) #remove exp_incr_1
summary(bi_open)  #maybe get rid of prop_coll_1 but will leave in for now

prop_open <- prop_open_backwards
vif(prop_open)  #all fine (<5)

prop_open <- update(prop_open, ~.-prop_pd_incr_1)
summary(prop_open) #leave status_2_1 in for now, negative coefficients make some sense

vif(exp_open_backwards)
exp_open <- update(exp_open_backwards, ~.-bi_pd_incr_1)
summary(exp_open)
exp_open <- update(exp_open, ~.-prop_rx_1)

#Now we have our inital three models for incremental payments between ages 18 and 30 for BI, Property, and Expenses

formula(bi_open)
formula(prop_open)
formula(exp_open)

summary(bi_open)  
summary(prop_open)
summary(exp_open)

#Lets run a first trial of predictions and see what the outcomes like:

training_open$incre_bi_paid_sim <- predict(bi_open, type = "response")
training_open$incre_prop_paid_sim <- predict(prop_open, type = "response")
training_open$incre_exp_paid_sim <- predict(exp_open, type = "response")
training_open$incre_tot_paid_sim <- training_open$incre_bi_paid_sim +
                                        training_open$incre_prop_paid_sim + 
                                                training_open$incre_exp_paid_sim
head(training_open[0:10,])



