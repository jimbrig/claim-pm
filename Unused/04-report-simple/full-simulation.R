library(dplyr)
library(tidyr)
library(caret)
library(mgcv)
library(gamlss)
library(lubridate)
library(ggplot2)
library(xtable)

# load data
claims <- readRDS("../00-data/model-claims.RDS")

# development age to project from
# e.g. if we set this to 18 we will use information available
# as of the 18 month evaluation to predict stuff at age 30)
devt_period <- 30

# year to predict with model
# evaluations at or greater than this time will not be included in
# the model fit
predict_eval <- as.Date("2014-11-30")

# remove unneeded data
claims <- filter(claims, eval <= predict_eval)

# we only need the claims at the selected `dev_period` for our models
claims <- dplyr::filter(claims, devt %in% devt_period) %>%
            dplyr::select(-devt)


# we don't need incremental payments for the closure model
#cm_data <- dplyr::select(claims, -tot_pd_incr_act)

#cm_data$status_1_0 <- ifelse(cm_data$status == "C", 
#                             "C", 
#                             paste0(cm_data$status_1, "_" , 
#                                    cm_data$status))
#cm_data$status_1_0 <- as.factor(cm_data$status_1_0)


# remove columns not used in cm fit
model_data <- dplyr::filter(claims, eval < predict_eval)
#  dplyr::select(-status, -status_1, -eval)


# function for fitting closure model
cm_model <- caret::train(status_act ~ status + tot_rx + tot_pd_incr, 
                         data = model_data,
                         method = "glmStepAIC",
                         trace = FALSE,
                         preProcess = c("center", "scale", "YeoJohnson"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 2))

# remove all claims that have a closed closed status from the data
# these will be set to incremental payments of 0
zm_model_data <- filter(model_data, status == "O" |  status_act == "O")

#Add in response variable for zero payment:
zm_model_data$zero <- factor(ifelse(zm_model_data$tot_pd_incr_act == 0, 
                                    "Zero", "NonZero"))

#Remove unnecessary columns:
#zm_model_data <-  dplyr::select(zm_model_data, -status_act, -status, -status_1, 
#                                -tot_pd_incr_act, -eval)


# filter for development time to model and drop 'devt' column
#sample <- dplyr::select(zm_model_data, -devt, -claim_number)

zm_model <- caret::train(zero ~ status + status_act + tot_rx + tot_pd_incr, 
                         data = zm_model_data,
                         method = "glmStepAIC",
                         trace = FALSE,
                         preProcess = c("center", "scale", "YeoJohnson"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 2))


#Take out zero pmnts:
nzm_model_data <- zm_model_data[zm_model_data$tot_pd_incr_act > 0, ]

# take out claims with a current reserve of zero
weibull_model_data <- nzm_model_data[nzm_model_data$tot_rx <= 0, ]

nzm_model_data <- nzm_model_data[nzm_model_data$tot_rx > 0, ]

  
# fit incremental payment model
nzm_model <- mgcv::gam(tot_pd_incr_act ~ status_act + s(tot_rx) + s(tot_pd_incr),
                       data = nzm_model_data,
                       family = quasipoisson(link = "log"))

######
# Weibull Fit
######
# fit
weibull_fit <- gamlss::gamlss(tot_pd_incr_act ~ status_act + tot_pd_incr, 
                              sigma.formula = ~ status_act + tot_pd_incr,
                              data = weibull_model_data[, c("tot_pd_incr_act", 
                                                            "status_act", 
                                                            "tot_pd_incr")],
                              
                              family = "WEI3")


######
# Simulation
######
set.seed(1234)
n_sims <- 1000

cm_pred_data <- dplyr::filter(claims, eval == predict_eval)

cm_probs <- cbind(cm_pred_data, 
                  predict(cm_model, newdata = cm_pred_data, type = "prob"))

cm_pred <- lapply(cm_probs$O, rbinom, n = n_sims, size = 1)
cm_pred <- matrix(unlist(cm_pred), ncol = n_sims, byrow = TRUE)
cm_pred <- ifelse(cm_pred == 1, "O", "C")
cm_pred <- as.data.frame(cm_pred)

  


cm_pred <- cbind(cm_probs[, "claim_number", drop = FALSE], cm_pred)

# gather `cm_pred` into a long data frame
cm_pred <- tidyr::gather(cm_pred, key = "sim_num", 
                         value = "status_sim",  
                         -claim_number)




# join `zm_pred_data` to predictions from closure model
# remove status_act and rename the simulated states as status_act
zm_pred_data <- left_join(cm_pred, cm_probs, by = "claim_number") %>%
  dplyr::select(-status_act) %>%
  dplyr::rename(status_act = status_sim)

# remove all claims that have a closed closed status from the data
# these will be set to incremental payments of 0
closed_closed_data <- dplyr::filter(zm_pred_data, status == "C" &  status_act == "C")

zm_pred_data <- dplyr::filter(zm_pred_data, status == "O" |  status_act == "O")
  
zm_pred <- cbind(zm_pred_data, 
                 predict(zm_model, newdata = zm_pred_data, type = "prob"))

zm_pred$zero_sim <- sapply(zm_pred$NonZero, rbinom, n = 1, size = 1)
zm_pred$zero_sim <- ifelse(zm_pred$zero_sim == 1, "NonZero", "Zero")


# separate zeros from non zeros
zero_claims <- filter(zm_pred, is.na(zero_sim) | zero_sim == "Zero")

nzm_pred <- filter(zm_pred, zero_sim == "NonZero")

# the claims with zero current tot_rx will be passed to the weibull simulation
nzm_weibull <- nzm_pred[nzm_pred$tot_rx == 0, ]

# the claims with > zero current tot_rx will be passed to the quasi poisson non zero model
nzm_quasi <- nzm_pred[nzm_pred$tot_rx > 0, ]

#### Weibull simulation
# use predict all to get the parameters
weibull_params <- as.data.frame(
                    predictAll(weibull_fit, 
                               newdata = nzm_weibull[, c("status_act", "tot_pd_incr")]))

wei_predictions <- vector("numeric", length = nrow(weibull_params))
for (i in seq_along(wei_predictions)) {
  wei_predictions[i] <- rWEI3(n = 1, 
                              mu = weibull_params$mu[i], 
                              sigma = weibull_params$sigma[i])
}

nzm_weibull$tot_pd_incr_sim <- wei_predictions

### Quasi Poisson Simulation
nzm_quasi$tot_pd_incr_fit <- exp(predict(nzm_model, newdata = nzm_quasi))

nzm_quasi$tot_pd_incr_sim <- sapply(nzm_quasi$tot_pd_incr_fit,
                                    function(x) {
                                      rnbinom(n = 1, size = x ^ (1/5), prob = 1 / (1 + x ^ (4/5))) 
                                    })


#### combine the 
closed_closed_data$tot_pd_incr_sim <- 0
closed_closed_data$status_act <- closed_closed_data$status_sim
zero_claims$tot_pd_incr_sim <- 0
zero_claims$status_act <- zero_claims$status_sim

closed_closed_data$sim_type <- "Close_Close"
zero_claims$sim_type <- "Zero"
nzm_weibull$sim_type <- "Weibull"
nzm_quasi$sim_type <- "Quasi"


cols <- c("sim_num", "claim_number", "status_act", "tot_pd_incr_sim", "sim_type")

sim_1 <- closed_closed_data[, cols]
sim_2 <- zero_claims[, cols]
sim_3 <- nzm_weibull[, cols]
sim_4 <- nzm_quasi[, cols]


full_sim <- rbind(sim_1, sim_2, sim_3, sim_4)
tail(full_sim)
full_sim[full_sim$claim_number == "2013246543", ]