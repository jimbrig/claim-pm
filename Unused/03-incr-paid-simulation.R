library(car)
library(MASS)
library(dplyr)
library(caret)
library(gamlss)

# First need to load back in data w/ incremental pmnts
# between ages 18 and 30:

# load data

sample <- read.csv("data/claims_30.csv", stringsAsFactors = FALSE)

sample$status <- ifelse(sample$status == "C", 0, 1)  
sample$status_1 <- ifelse(sample$status_1 == "C", 0, 1) 
sample$status_2 <- ifelse(sample$status_2 == "C", 0, 1) 
sample$status_2_1 <- ifelse(sample$status_1 == 0, 0, paste(sample$status_2, sample$status_1, sep = "_"))
sample$status_2_1 <- factor(sample$status_2_1)

sample_nz <- sample[sample$tot_pd_incr > 0, ]

# Form training and testing samples
set.seed(992) #use different seed than logistic regression so not overfitting data

incr_pmnts <- sample_nz$tot_pd_incr

training_rows <- createDataPartition(incr_pmnts, p = .7, list = FALSE )


training <- sample_nz[training_rows, ]

testing <- sample_nz[-training_rows, ]


mod <- glm(log(tot_pd_incr) ~ status_1_0 + suit_1 + sqrt(tot_inc_1) + tot_rx_1_perc + tot_pd_incr_1, data =training)
summary(mod)

names(training)
training$status_1_0 <- (ifelse(training$status_1_0 == "O_C", "C_C", training$status_1_0))
testing$status_1_0 <- (ifelse(testing$status_1_0 == "O_C", "C_C", testing$status_1_0))

mod <- glm(log(tot_pd_incr) ~ status_1_0 + suit_1 + sqrt(tot_inc_1) + tot_rx_1_perc + tot_pd_incr_1, data =training)
summary(mod)

training$logpreds <- predict(mod, type = "response")
training$preds <- exp(training$logpreds)

testing$logpreds <- predict(mod, type = "response", newdata = testing)
testing$preds <- exp(testing$logpreds)

sum(training$preds)
sum(training$tot_pd_incr)

sum(testing$preds)
sum(testing$tot_pd_incr)

logmod <- glm(status ~ status_2_1 + suit_1 + tot_rx_1_perc + sqrt(tot_inc_1), data = training, family = binomial)
summary(logmod)

training$logits <- predict(logmod)
training$logprobs <- predict(logmod, type = "response")
testing$logits <- predict(logmod, newdata = testing)
testing$logprobs <- predict(logmod, newdata = testing, type = "response")

sim <- training

sim$status_sim <- sapply(training$logprobs, function(x) rbinom(n=1, size =1, prob = x))

sim$status_1_0_sim <- paste(sim$status_1, sim$status_sim, sep = "_")
sim$status_1_0_sim <- ifelse(sim$status_1_0_sim == "1_0", "0_0", sim$status_1_0_sim)

sim_mod <- glm(log(tot_pd_incr) ~ suit_1 + sqrt(tot_inc_1) + tot_rx_1_perc + tot_pd_incr_1, data = sim)
summary(sim_mod)

sim$pd_sim <- exp(predict(sim_mod, type = "response"))

names(sim)
head(sim[, c(1, 4, 20, 25, 26, 27, 28 )])

sum(sim$pd_sim)
sum(sim$tot_pd_incr)  #7 million over, not terrible..



