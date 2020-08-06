library(dplyr)


# load bi claims
claims <- readRDS("../00-data/model-claims.RDS") %>% dplyr::select(-tot_rx)

# adjust levels of suit_lag_group.  Removing insignificant levels
claims$suit_lag_group <- as.character(claims$suit_lag_group)
claims$suit_lag_group <- ifelse(claims$suit_lag_group %in% c("lag_24", "lag_30"),
                                 "lag_18",
                                 claims$suit_lag_group)
claims$suit_lag_group <- as.factor(claims$suit_lag_group)
claims$suit_lag_group <- relevel(claims$suit_lag_group, ref = "NS")


# Isolate data for model fitting ----------------------------------------
#Add in status_0_act factor:
model_data <- dplyr::filter(claims, eval != "2015-11-30") 

# remove all claims that have a closed closed status from the data
# these will be set to incremental payments of 0
model_data <- filter(model_data, status == "O" |  status_act == "O")

model_data$status_0_act <- factor(paste0(model_data$status, "_" , model_data$status_act))

#Add in response variable for zeroes:
model_data$zero <- factor(ifelse(model_data$tot_pd_incr_act == 0, "Zero", "NonZero"))


#Remove unnecessary columns:
model_data <-  dplyr::select(model_data, -status_act, -status, -status_1, -eval, -tot_pd_incr_act)
                

saveRDS(as.data.frame(model_data), file = "../00-data/02-zm/zm-model-data.RDS")

# Isolate data for making predictions -----------------------------------
predict_data <- dplyr::filter(claims, eval == "2015-11-30") %>%
                  dplyr::select(-status_act, -status_1, -eval, -tot_pd_incr_act)

saveRDS(predict_data, file = "../00-data/04-predictions/00-zm-predict-data.RDS")