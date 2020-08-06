library(ggplot2) # for plotting
library(caret) # for modeling
library(gamlss)
library(dplyr)

# load data
# data created in 'nzm-01-data-prep.R'
sample <- readRDS("../00-data/03-nzm/nzm-model-data.RDS")
# take out problematic claim
sample <- sample[!(sample$claim_number %in% c("2011194582", "2006107399")), ]   


# Train doesnt allow you to set ks (dfs) with method = "gam", and method = "gamSpline" doesn't support poisson families,
# so will use a manual training and testing set to manually determine best dfs for the splines:

set.seed(1234)

pds <- sample$tot_pd_incr_act
rows <- createDataPartition(pds, p = .75, list = FALSE)

training <- sample[rows, ]
testing <- sample[-rows, ]

training_18 <- training[training$devt == 18, ]
training_30 <- training[training$devt == 30, ]
training_42 <- training[training$devt == 42, ]

# Form Models (will want to use train in future, just takes a while..also gam functions have method and select arguments that can be
# used in future as well):
mod_18 <- mgcv::gam(tot_pd_incr_act ~ status_act + s(tot_rx) + s(tot_pd_incr),
                    data = training_18[training_18$tot_rx > 0, ],
                    family = quasipoisson(link = "log"))

mod_30 <- mgcv::gam(tot_pd_incr_act ~ status_act + s(tot_rx) + s(tot_pd_incr),
                    data = training_30[training_30$tot_rx > 0, ],
                    family = quasipoisson(link = "log"))


mod_42 <- mgcv::gam(tot_pd_incr_act ~ status_act + s(tot_rx),
                    data = training_42[training_42$tot_rx > 0, ],
                    family = quasipoisson(link = "log"))

my_models_nz <- list(mod_18, mod_30, mod_42)

# my_models_nz <- readRDS("../00-data/03-nzm/nzm-linear-models.RDS")

# save models for reuse
saveRDS(my_models_nz, file = "../00-data/03-nzm/nzm-poisson-gam-models.RDS")

# Quick Model Diagnostics
summary(my_models_nz[[1]])
summary(my_models_nz[[2]])
summary(my_models_nz[[3]])

plot(mod_18)
plot(mod_30)
plot(mod_42)

mgcv::gam.check(mod_18)
mgcv::gam.check(mod_30)
mgcv::gam.check(mod_42)


# Now we need to simulate pmnts for claims w/ zero reserves.  Will use a fitted distribution and a gamlss model
# predicting the distributions parameters:

sample <- sample[sample$devt == 18, ]
#sample <- sample[sample$devt > 6, ]
sample_zrx <- sample[sample$tot_rx == 0, ]
sample_zrx <- sample_zrx[sample_zrx$tot_pd_incr_act > 0, ]
  
# Fit initial distribution to data:
#fits <- gamlss::fitDist(sample_zrx$tot_pd_incr_act, type = "realplus")
#fits$fit  

#fit_zrx <- gamlss::histDist(tot_pd_incr_act, family = "GIG", data = sample_zrx, nbins = 50)
#fit_zrx <- gamlss::histDist(tot_pd_incr_act, family = "LOGNO", data = sample_zrx, nbins = 50)
#fit_zrx <- gamlss::histDist(tot_pd_incr_act, family = "GG", data = sample_zrx, nbins = 50)
#fit_zrx <- gamlss::histDist(tot_pd_incr_act, family = "GA", data = sample_zrx, nbins = 50)
fit_zrx <- gamlss::histDist(tot_pd_incr_act, family = "WEI3", data = sample_zrx, nbins = 50)  #Weibull and Gamma dists are only ones that 
fit_zrx                                                                                       #catche left side of dist, will go with WEI3.

# Fit Model using gamlss and initial distribution used to choose
# starting points for mu and sigma:
zrxmod <- gamlss::gamlss(tot_pd_incr_act ~ status_act + tot_pd_incr, 
                    sigma.formula = ~ status_act + tot_pd_incr,
                    data = sample_zrx,
                    mu.start = fit_zrx$mu,
                    sigma.start = fit_zrx$sigma,
                    family = "WEI3")

# Simulate pmnts using weibull distribution and predicted means and sds from model:
f <- function(m, s) {
  mean(rWEI3(10, mu = m, sigma = s))
}

test <- predictAll(zrxmod, newdata = sample_zrx[1:20, ])
predict(zrxmod, what = "sigma")

rWEI3(n = 1, mu = test$mu[1], sigma = test$sigma[1])
sample_zrx$preds <- mapply(f, zrxmod$mu.fv, zrxmod$sigma.fv)

# Add simulations into whole sample:
preds <- sample_zrx[, c("claim_number", "devt", "preds")]
sample <- dplyr::left_join(sample, preds, by = c("claim_number", "devt"))

# Separate sample by devt age and compare actual incremental pmnts vs predicted/simulated ones:
sample_18 <- sample[sample$devt == 18, ]
sample_30 <- sample[sample$devt == 30, ]
sample_42 <- sample[sample$devt == 42, ]

sample_18$preds <- ifelse(sample_18$tot_rx == 0, sample_18$preds, predict(mod_18, newdata = sample_18, type = "response"))
sample_30$preds <- ifelse(sample_30$tot_rx == 0, sample_30$preds, predict(mod_30, sample_30, type = "response"))
sample_42$preds <- ifelse(sample_42$tot_rx == 0 , sample_42$preds, predict(mod_42, sample_42, type = "response"))

sum(sample_18$tot_pd_incr_act) ; sum(sample_18$preds)  #6M under
sum(sample_30$tot_pd_incr_act) ; sum(sample_30$preds)  #1M over
sum(sample_42$tot_pd_incr_act) ; sum(sample_42$preds)  #2M over


###########################################################################
sample_18$residuals <- sample_18$tot_pd_incr_act - sample_18$preds
sample_30$residuals <- sample_30$tot_pd_incr_act - sample_30$preds
sample_42$residuals <- sample_42$tot_pd_incr_act - sample_42$preds

hist(sample_18$residuals, breaks = 50) #add density curve to this plot
summary(sample_18$residuals) ; sd(sample_18$residuals)

hist(sample_30$residuals, breaks = 50) 
summary(sample_30$residuals) ; sd(sample_30$residuals) 

hist(sample_42$residuals, breaks = 50)
summary(sample_42$residuals) ; sd(sample_42$residuals)



