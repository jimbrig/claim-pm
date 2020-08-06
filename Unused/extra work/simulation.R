#simulate 





simulate <- function(n) {

set.seed(n)

rows <- sample(1:nrow(sample), nrow(sample) *0.75)
training <- sample[rows, ]
testing <- sample[-rows,]

#form our own contrasts instead of using automated R ones:

training <- form_contrasts(training, training)
testing <- form_contrasts(training, testing)  #want to use weights from training set when contrasting

contrast_model <- glm(status ~ c1 + c2 + c3 + suit_lag + bi_rx_1 + prop_rx_1 + exp_rx_1 + bi_pd_incr_1 + prop_pd_incr_1 + exp_pd_incr_1, data = training, family = binomial)

status_2_1_model <- glm(status ~ status_2_1 + suit_lag + bi_rx_1 + prop_rx_1 + exp_rx_1 + bi_pd_incr_1 + prop_pd_incr_1 + exp_pd_incr_1, data = training, family = binomial)

status_1_model <- glm(status ~ status_1 + suit_lag + bi_rx_1 + prop_rx_1 + exp_rx_1 + bi_pd_incr_1 + prop_pd_incr_1 + exp_pd_incr_1, data = training, family = binomial)


testing$prob <- predict(contrast_model, newdata = testing, type = "response")
contrast_error <- abs(sum(testing$prob) - sum(testing$status))

testing$prob <- predict(status_2_1_model, newdata = testing, type = "response")
status_2_1_error <-  abs(sum(testing$prob) - sum(testing$status))

testing$prob <- predict(status_1_model, newdata = testing, type = "response")
status_1_error <-  abs(sum(testing$prob) - sum(testing$status))

errors <- c(contrast_error, status_2_1_error, status_1_error)
names(errors) <- c("Contrast Error", "Status_2_1 Error", "Status 1 Error")

errors
}


n <- runif(30, 1, 8000)
n <- round(n[1:30], 0)

simulation <- lapply(n, simulate)

hold <- lapply(simulation, as.data.frame)


m <- as.matrix(dplyr::bind_rows(hold))

dim(m) <- c(3, 30)

sums <- apply(m, 1, sum)

Contrast_Avg_Error <- sums[1] / 30
Status_2_1_Avg_Error <- sums[2] / 30
Status_1_Avg_Error <- sums[3] / 30

Contrast_Avg_Error ; Status_2_1_Avg_Error ; Status_1_Avg_Error

#Contrast has lowest average error, but barely.

