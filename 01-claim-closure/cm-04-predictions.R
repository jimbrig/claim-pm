library(dplyr)
library(tidyr)

# read in the models
closure_models <- readRDS("../00-data/01-cm/cm-models.RDS")
devt_periods <- c(18, 30)

# apply models to the 2015-11-30 data we want to make predictions on (now using current values of predictors to predict future unknown
# statuses)
predict_data <- readRDS("../00-data/04-predictions/00-initial-predict-data.RDS")  

set.seed(1234)
my_predictions <- vector("list", length = length(devt_periods))
for (i in seq_along(devt_periods)) {
  my_data <- predict_data[predict_data$devt == devt_periods[i], ]
  my_predictions[[i]] <- cbind(my_data[, c("devt", "claim_number")], 
                               "prob_open" = predict(closure_models[[i]], newdata = my_data, type = "prob")$O)
  rm(my_data)
}


my_predictions <- bind_rows(my_predictions)

# select the number of simulations you want to run
n_sims <- 3

# create a matrix of predicted statuses
preds <- lapply(my_predictions$prob_open, rbinom, n = n_sims, size = 1)
preds <- matrix(unlist(preds), ncol = n_sims, byrow = TRUE)


# attach predicted status with the `my_predictions` data frame
my_predictions <- cbind(my_predictions, preds)

my_predictions <- dplyr::select(my_predictions, -prob_open)
# gather those predictions
my_predictions <- tidyr::gather(my_predictions, key = "sim_num", 
                                value = "status_sim",  -devt, -claim_number)

saveRDS(as.data.frame(my_predictions), "../00-data/04-predictions/01-cm-predictions.RDS")
