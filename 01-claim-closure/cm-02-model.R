library(ggplot2) # for plotting
library(caret) # for modeling

#' model_closure
#' 
#' @param data the data to fit to the closure model
#' @param devt development time to be modeled
#' 
#' @imports caret
#' 
#' @examples 
#' my_sample <- readRDS("../00-data/01-cm/cm-model-data.RDS")
#' 
#' my_model <- model_closure(data = my_sample, devt = 18)
model_closure <- function(data, devt) {
  # filter for development time to model and drop 'devt' and 'claim_number' columns
  sample <- data[data$devt == devt, setdiff(names(data), c("devt", "claim_number"))]
  
  train(status_act ~ ., 
        data = sample,
        method = "glmStepAIC",
        preProcess = c("center", "scale", "YeoJohnson"),
        trControl = trainControl(method = "repeatedcv", repeats = 10))
  
} 



# load data
# data created in 'cm-01-data-prep.R'
sample <- readRDS("../00-data/01-cm/cm-model-data.RDS")

# find development periods we want to fit model to (2 total models, 1st uses data as of age 6 to predict status at age 18, 
# second uses data as of age 18 to predict status at age 30, etc.)
devt_periods <- c(18, 30)

# set seed
set.seed(6001)

# run model for all development periods
my_models <- lapply(devt_periods, model_closure, data = sample)

# save model for reuse
saveRDS(my_models, file = "../00-data/01-cm/cm-models.RDS")
