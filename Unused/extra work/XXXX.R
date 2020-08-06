#Set up data

sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)
sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ] 
sample$status <- ifelse(sample$status == "C", 0, 1)  #setting Closed to to 0 and Open to 1
sample$status_1 <- ifelse(sample$status_1 == "C", 0, 1) #Status_1 represents clm status at 18 months devt
sample$status_2 <- ifelse(sample$status_2 == "C", 0, 1) #Status_2 represents clm status at 6 months devt
sample$status_2_1 <- ifelse(sample$status_1 == 0, 0, paste(sample$status_2, sample$status_1, sep = "_"))
sample$status_2_1 <- factor(sample$status_2_1)

sample <- sample[ , -c(1:4, 9:17, 28)]
names(sample)

sample$pd_incr <- sample$bi_pd_incr + sample$prop_pd_incr + sample$exp_pd_incr  

sample <- sample[!sample$pd_incr < 0,]
summary(sample$pd_incr)

hist(sample$pd_incr)  #heavily skewed because of all the zeroes (54.8 % of claims have incr pmnts of zero)

#Create samples for open claims at age 30 and closed claims at age 30

sample_open <- sample[sample$status == 1, ]

sample_closed <- sample[sample$status == 0, ]

sample_open$tot_rx_1 <- sample_open$bi_rx_1 + sample_open$prop_rx_1 + sample_open$exp_rx_1
plot(pd_incr ~ tot_rx_1, data = sample_open)

mod1 <- gamlss(pd_incr ~ poly(tot_rx_1, 3), data = sample_open, family = NO(mu.link = log))
summary(mod1)
#Look at Open claims sample first
#create nonzero sample:

sample_open_nonzero <- sample_open[!sample_open$pd_incr == 0,]
summary(sample_open_nonzero$pd_incr)  #mean of $47,530

hist(sample_open_nonzero$pd_incr, breaks = 10000, xlim = c(0,200000))

fit <- gamlss::fitDist(pd_incr, type = "realplus", data = sample_open) 
fit #Box-Cox Power Exponential distribution fits best

fit$fits  #other options are generalized gamma, lognormal, inverse gaussian, gamma, etc.

fit$sigma.link
histDist(pd_incr, family = fit$family, data = sample_open, xlim = c(0,200000), nbins = 5000)
histDist(pd_incr, family = "GA", data = sample_open_nonzero, xlim = c(0,200000), nbins = 5000)
histDist(pd_incr, family ="LOGNO", data = sample_open_nonzero, xlim= c(0,200000), nbins = 5000 )
histDist(pd_incr, family = "IG", data = sample_open_nonzero, xlim = c(0,200000), nbins = 5000)

#gamma is only one that goes high enough on far left side, but box-cox-power exponential technically has 
#best "fit" with data. Lets try some models:


names(sample_open)

m1 <- gamlss(formula = pd_incr ~ 1, family = PARETO2, data = sample_open, mu.fix = FALSE, sigma.fix = FALSE)

checklink(which.link = mu, which.dist = NULL, link = NULL, link.List = NULL)

sample_open$pd_incr_nonzero <- ifelse(sample_open$pd_incr > 0, 1, 0)

initialmod <- gamlss(pd_incr ~ pb(suit_lag, method = "GAIC") + pb(bi_rx_1, method = "GAIC") 
                     + pb(prop_rx_1, method = "GAIC") + pb(exp_rx_1, method = "GAIC") 
                     + factor(suit) + factor(status_2_1), sigma.formula =~pb(suit_lag, method = "GAIC") + pb(bi_rx_1, method = "GAIC") 
                     + pb(prop_rx_1, method = "GAIC") + pb(exp_rx_1, method = "GAIC") 
                     + factor(suit) + factor(status_2_1), nu.formula =~pb(suit_lag, method = "GAIC") + pb(bi_rx_1, method = "GAIC") 
                     + pb(prop_rx_1, method = "GAIC") + pb(exp_rx_1, method = "GAIC"), 
                     data = sample_open, family = "ZAGA", mu.fix = TRUE, sigma.fix = TRUE, nu.fix = TRUE)
summary(initialmod)
addterm(m1, scope =~(rept_lag + suit_lag + suit + bi_rx_1 + bi_pd_1 + bi_coll_1
        + prop_rx_1 + prop_pd_1 + prop_coll_1 + exp_rx_1 + exp_pd_1 
        + exp_coll_1 + bi_rx_incr_1 + bi_pd_incr_1 + prop_rx_incr_1 
        + prop_pd_incr_1 + exp_rx_incr_1 + exp_pd_incr_1 + status_2_1), test = "GAIC")



gamlss.scope(sample_nonzero, response = 28, smoother = "pb")
x <- stepGAICAll.A(initialmod, sigma.try = FALSE, nu.try = FALSE, tau.try = FALSE )
mumodel <- x

y <- stepGAICAll.A(initialmod, what = "sigma", mu.try = FALSE, nu.try = FALSE, tau.try = FALSE)
sigmodel <- y

sample <- select(sample, -pd_incr_log)
sample$nonzero <- ifelse(pd_incr > 0, 1, 0)

gamlss.scope(sample, response = 29, smoother = "pb")
logmod <- gamlss(nonzero ~., family = "BI", data = sample)
z <- stepGAICAll.A(logmod, sigma.try = FALSE, nu.try = FALSE, tau.try = FALSE)
logmod <- z

formula(mumodel)
formula(sigmodel)
formula(logmod)

gamlss(mumodel, sigma.formula = sigmodel, nu.formula = logmod, family = "ZAGA", data = sample)

#predict
