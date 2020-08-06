sample <- read.csv("data/claims_30.csv", stringsAsFactors = FALSE)
str(sample)

sample$status <- factor(sample$status)
sample$status_1 <- factor(sample$status_1)
sample$status_2 <- factor(sample$status_2)
sample$status_1_0 <- factor(sample$status_1_0)
levels(sample$status_1_0) <- c("C", "C", "C_O", "O_O")
sample$new_open <- ifelse(sample$status_2_1 == "NA_O", 1, 0)
sample$status_2_1 <- ifelse(sample$status_1 == "C", "C", paste(sample$status_2, sample$status_1, sep = "_"))

names(sample)

sample <- sample[sample$tot_pd_incr > 0, ]

set.seed(150)

incr_pmnts <- sample$tot_pd_incr

training_rows <- createDataPartition(incr_pmnts, p = .75, list = FALSE )

training <- sample[training_rows, ]
testing <- sample[-training_rows, ]

names(training)

mod_train <- train(tot_pd_incr~., data =training[, -c(1, 2, 3, 5, 15, 17, 18)], method = "lmStepAIC", 
      trControl = trainControl(method = "repeatedcv", repeats = 10))

mod_train <- train(tot_pd_incr~., data =training[, -c(1, 2, 3, 5, 15, 17, 18)], method = "lmStepAIC", 
                   trControl = trainControl(method = "repeatedcv", repeats = 10))

mod_train$finalModel


training$preds <- predict(mod_train, newdata = training)
testing$preds <- predict(mod_train, newdata = testing)

sum(testing$preds)
sum(testing$tot_pd_incr)

                          
logmod <- glm(status ~ status_2_1 + suit_1 + tot_rx_1_perc + sqrt(tot_inc_1), data = training, family = binomial)
summary(logmod)

sample$status_prob <- predict(logmod, newdata = sample, type = "response")

sample$status_sim <- sapply(sample$status_prob, function(x) rbinom(n=1, size =1, prob = x))
sample$status_sim <- factor(ifelse(sample$status_sim == 1, "O", "C"))


sample$status_1_0_sim <- factor(paste(sample$status_1, sample$status_sim, sep = "_"))

levels(sample$status_1_0_sim)
levels(sample$status_1_0_sim) <- c("C", "C_O", "C", "O_O")

simdata <- sample
simdata$status_1_0 <- simdata$status_1_0_sim

simdata$preds <- predict(rlm_mod, newdata = simdata)

sum(simdata$preds)
sum(simdata$tot_pd_incr)   

simdata$residuals <- simdata$tot_pd_incr - simdata$preds

names(simdata)
head(simdata[, c(1, 20, 23, 24, 26)])

plot(sim$tot_pd_incr, sim$residuals)

rlm_train <- train(tot_pd_incr~ status_1_0 + rept_lag + suit_lag + suit_1 + bi_rx_1 + bi_pd_1 + prop_rx_1 + prop_pd_1 + exp_rx_1 + 
                     tot_rx_incr_1 + tot_pd_incr_1 + new_open, data =training, method = "rlm", 
                   trControl = trainControl(method = "repeatedcv", repeats = 10))

rlm_train

rlm_train$finalModel

simdata$preds_rlm <- predict(rlm_mod, newdata = simdata)

sum(simdata$preds_rlm)
sum(simdata$tot_pd_incr)  



