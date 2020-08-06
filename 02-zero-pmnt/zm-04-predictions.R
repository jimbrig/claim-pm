library(dplyr)
# read in zero closure models
zero_models <- readRDS("../00-data/02-zm/zm-models.RDS")

# read in status model predictions
preds <- readRDS("../00-data/04-predictions/01-cm-predictions.RDS")

# read in data to predict
zm_predict_data <- readRDS("../00-data/04-predictions/00-zm-predict-data.RDS")

# attach zm_predic_data to our status predictions
preds <- left_join(preds, zm_predict_data, by = c("devt", "claim_number"))

preds$status_sim <- ifelse(preds$status_sim == 0, "C", "O")
preds$status_0_act <- paste0(preds$status, "_", preds$status_sim)

# remove C_C from data
closed_closed <- dplyr::filter(preds, status_0_act == "C_C")
preds <- dplyr::filter(preds, status_0_act != "C_C")

preds_18 <- filter(preds, devt == 18)

test <- predict(zero_models[[1]], newdata = preds_18, type = "prob")
preds_18[preds_18$claim_number == "2014275496", ]
