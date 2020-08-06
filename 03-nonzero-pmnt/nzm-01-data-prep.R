library(dplyr)

# load bi claims
claims <- readRDS("../00-data/model-claims.RDS")

# Take out prediction set
model_data <- dplyr::filter(claims, eval != "2015-11-30") 

#Remove unnecessary columns:
model_data <- dplyr::select(model_data, devt, claim_number, status_act, tot_rx, tot_pd_incr, tot_pd_incr_act)

#Take out zero pmnts:
model_data <- model_data[model_data$tot_pd_incr_act > 0, ]


saveRDS(as.data.frame(model_data), file = "../00-data/03-nzm/nzm-model-data.RDS")

#simple model will just use tot_rx, for this prep will want to add in any other relevant predictors.