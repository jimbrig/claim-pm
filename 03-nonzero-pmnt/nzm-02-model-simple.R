library(ggplot2) # for plotting
library(caret) # for modeling

#' model_nz_pmnt
#' 
#' @param data the data to fit to the non_zero_pmnt model
#' @param devt development time to be modeled
#' 
#' @imports caret
#' 
#' @examples 
#' my_sample <- readRDS("../00-data/03-nzm/nzm-model-data.RDS")
model_nz_pmnt <- function(data, devt, remove_outlier = TRUE, outlier_pct_cut = 0.99) {
  # filter for development time to model and drop 'devt' column
  sample <- data[data$devt == devt, ]
  
  m_outliers <- caret::train(tot_pd_incr_act ~ tot_rx, 
                             data = sample,   
                             method = "gam",
                             trControl = trainControl(method = "repeatedcv", repeats = 10))
  
  # find cooks distance
  if (remove_outlier) {
    cooks <- cooks.distance(m_outliers$finalModel)
    # find claims with cooks distance > outlier_cut
    outlier_cut <- quantile(cooks, probs = outlier_pct_cut)
    outliers <- sample[cooks > outlier_cut, ]
    # remove outliers
    sample <- sample[cooks <= outlier_cut, ]
    
    # fit model with outliers removed from data
    m <- caret::train(tot_pd_incr_act ~ tot_rx, 
                      data = sample,   
                      method = "gam",
                      trControl = trainControl(method = "repeatedcv", repeats = 10))
  } else {
    m <- m_outliers
  }
  
  list("model" = m,
       "outliers" = outliers)
} 


# load data
# data created in 'nzm-01-data-prep-simple.R'
sample <- readRDS("../00-data/03-nzm/nzm-model-data-simple.RDS")

# find development periods we want to fit model to
devt_periods <- c(18, 30)

# set seed
set.seed(6001)

sample_open <- dplyr::filter(sample,
                             status_act == "O") %>%
                 dplyr::select(-status_act)

sample_closed <- dplyr::filter(sample,
                               status_act == "C") %>%
                   dplyr::select(-status_act)

# Run models for open claims in 12 months:
my_models_nz_open <- lapply(devt_periods, 
                            model_nz_pmnt, 
                            data = sample_open)

# Run models for closed claims in 12 months:
my_models_nz_closed <- lapply(devt_periods, 
                              model_nz_pmnt, 
                              data = sample_closed,
                              outlier_cut = 10)

# save models for reuse
saveRDS(my_models_nz_open, file = "../00-data/03-nzm/nzm-open-gam-models-simple.RDS")
saveRDS(my_models_nz_closed, file = "../00-data/03-nzm/nzm-closed-gam-models-simple.RDS")

# Model Diagnostics:

summary(my_models_nz_open[[1]][[1]])
my_models_nz_open[[1]][[2]] # outliers
summary(my_models_nz_open[[2]][[1]])
my_models_nz_open[[2]][[2]] # outliers

summary(my_models_nz_closed[[1]])
summary(my_models_nz_closed[[2]])
summary(my_models_nz_closed[[3]])


mod_18_o <- my_models_nz_open[[1]]
mod_30_o <- my_models_nz_open[[2]]
mod_42_o <- my_models_nz_open[[3]]

mod_18_c <- my_models_nz_closed[[1]]
mod_30_c <- my_models_nz_closed[[2]]
mod_42_c <- my_models_nz_closed[[3]]


plot(mod_18_o$finalModel) 
plot(mod_18_c$finalModel) 

sample_18 <- sample[sample$devt == 18, ]
sample_30 <- sample[sample$devt == 30, ]
sample_42 <- sample[sample$devt == 42, ]


sample_18$preds <- ifelse(sample_18$status_act == "O", predict(mod_18_o, sample_18), predict(mod_18_c, sample_18))
sample_30$preds <- ifelse(sample_30$status_act == "O", predict(mod_30_o, sample_30), predict(mod_30_c, sample_30))
sample_42$preds <- ifelse(sample_42$status_act == "O", predict(mod_42_o, sample_42), predict(mod_42_c, sample_42))

sum(sample_18$tot_pd_incr_act) ; sum(sample_18$preds)
sum(sample_30$tot_pd_incr_act) ; sum(sample_30$preds)
sum(sample_42$tot_pd_incr_act) ; sum(sample_42$preds)

sample_18$residuals <- sample_18$tot_pd_incr_act - sample_18$preds
sample_30$residuals <- sample_30$tot_pd_incr_act - sample_30$preds
sample_42$residuals <- sample_42$tot_pd_incr_act - sample_42$preds

plot(sample_18$preds, sample_18$residuals)
plot(sample_30$preds, sample_30$residuals)
plot(sample_42$preds, sample_42$residuals)




