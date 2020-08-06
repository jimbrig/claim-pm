# using the simple incremental model to make predictions

z_models <- readRDS(file = "../00-data/03-nzm/nzm-poisson-gam-models.RDS")

z_18 <- z_models[[1]]


z_predictions <- exp(predict(z_18))


sapply(z_predictions, rpois, n = 1)