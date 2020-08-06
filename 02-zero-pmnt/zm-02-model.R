library(ggplot2) # for plotting
library(caret) # for modeling



#' model_zero_pmnt
#' 
#' @param data the data to fit to the zero_pmnt model
#' @param devt development time to be modeled
#' 
#' @imports caret
#' 
#' @examples 
#' my_sample <- readRDS("../00-data/02-zm/zm-model-data.RDS")
#' 
#' my_model <- model_zero_pmnt(data = my_sample, devt = 30)
model_zero_pmnt <- function(data, devt) {
  # filter for development time to model and drop 'devt' column
  sample <- data[data$devt == devt, setdiff(names(data), c("devt", "claim_number"))]
  
  caret::train(zero ~ ., 
               data = sample,
               method = "glmStepAIC",
               preProcess = c("center", "scale", "YeoJohnson"),
               trControl = trainControl(method = "repeatedcv", repeats = 10))
} 



# load data
# data created in 'zm-01-data-prep.R'
sample <- readRDS("../00-data/02-zm/zm-model-data.RDS")

# find development periods we want to fit model to
devt_periods <- c(18, 30)

# set seed
set.seed(6001)

# run model for all development periods
my_models_z <- lapply(devt_periods, model_zero_pmnt, data = sample)

# save model for reuse
saveRDS(my_models_z, file = "../00-data/02-zm/zm-models.RDS")

summary(my_models_z[[1]])  #Model for age 18 predicting probability of zero pmnt b/w 18 and 30
summary(my_models_z[[2]])  #Model for age 30 predicting probability of zero pmnt b/w 30 and 42
