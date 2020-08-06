#simulate 





compare_mods <- function(n, mod1, mod2, mod3) {
  
  model1 <- mod1
  model2 <- mod2
  model3 <- mod3

set.seed(n)

rows <- sample(1:nrow(sample), nrow(sample) *0.75)
training <- sample[rows, ]
testing <- sample[-rows,]

testing$prob1 <- predict(model1, newdata = testing, type = "response")
model1_error <- abs(sum(testing$prob1) - sum(testing$status))

testing$prob2 <- predict(model2, newdata = testing, type = "response")
model2_error <-  abs(sum(testing$prob2) - sum(testing$status))

testing$prob3 <- predict(model3, newdata = testing, type = "response")
model3_error <-  abs(sum(testing$prob3) - sum(testing$status))

errors <- c(model1_error, model2_error, model3_error)
names(errors) <- c("Model 1 Error", "Model 2 Error", "Model 3 Error")

errors
}


simulate <- function(n, mod1, mod2, mod3) {
  
  model1 <- mod1
  model2 <- mod2
  model3 <- mod3
  
  x <- runif(n, 1, 8000)
  x <- round(x[1:n], 0)

simulation <- lapply(x, compare_mods, model1, model2, model3)

hold <- lapply(simulation, as.data.frame)

m <- as.matrix(dplyr::bind_rows(hold))

dim(m) <- c(3, n)

sums <- apply(m, 1, sum)

vars <- apply(m, 1, var)

Mod1_Avg_Error <- sums[1] / n
Mod2_Avg_Error <- sums[2] / n
Mod3_Avg_Error <- sums[3] / n

Mod1_Error_SD <- sqrt(vars[1])
Mod2_Error_SD <- sqrt(vars[2])
Mod3_Error_SD <- sqrt(vars[3])



output <- cbind(Avg_Errors = c(Mod1_Avg_Error, Mod2_Avg_Error, Mod3_Avg_Error) ,
                              SDs = c(Mod1_Error_SD , Mod2_Error_SD , Mod3_Error_SD))

row.names(output) <- c("Mod 1", "Mod 2", "Mod 3")
output

}


model1 <- glm(status ~ status_1 + suit_lag + prop_rx_1 + prop_pd_1 + exp_rx_1 + bi_pd_1 , data = training, family = binomial)
model2 <- glm(status ~ status_2_1 + suit_lag + prop_rx_1 + prop_pd_1 + exp_rx_1 + bi_pd_1, data = training, family = binomial)
model3 <- glm(status ~ c1 + c2 + c3 + suit_lag + prop_rx_1 + prop_pd_1 + exp_rx_1 + bi_pd_1, data = training, family = binomial)


simulate(100, model1 , model2, model3)


