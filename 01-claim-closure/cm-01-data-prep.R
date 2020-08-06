library(dplyr)

claims <- readRDS("../00-data/model-claims.RDS")

# we don't need incremental payments for this model
claims <- dplyr::select(claims, -tot_pd_incr_act, -tot_rx)

claims$status_1_0 <- ifelse(claims$status == "C", "C", paste0(claims$status_1, "_" , claims$status))
claims$status_1_0 <- as.factor(claims$status_1_0)


# Isolate data for model fitting ----------------------------------------
model_data <- dplyr::filter(claims, eval != "2015-11-30") %>% # don't want to look at claims w/o future dev info
                dplyr::select(-status, -status_1, -eval)
                

saveRDS(as.data.frame(model_data), file = "../00-data/01-cm/cm-model-data.RDS")

# Isolate data for making predictions -----------------------------------
# organize the most recent data.  We will be using the models fitted to the model_data
# along with this data to make the predictions.  We want to use the same names as the model_data
predict_data <- dplyr::filter(claims, eval == "2015-11-30") %>% # only want claims that have not yet developed
                dplyr::select(-status_1, -eval)

saveRDS(as.data.frame(predict_data), file = "../00-data/04-predictions/00-initial-predict-data.RDS")