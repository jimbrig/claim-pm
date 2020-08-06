library(dplyr)

claims <- readRDS("../00-data/model-claims-full.RDS")

# Remove variables deemed unecessary in data analysis script:
claims <- dplyr::select(claims, 
                        -tot_pd_incr_act,
                        -status_1_0,
                        -status_1,
                        -suit_lag, 
                        -rept_lag, 
                        -rept_lag_group, 
                        -prop_rx, 
                        -exp_rx, 
                        -newclm, 
                        -status_0_act, 
                        -tot_pd, 
                        -tot_inc, 
                        -tot_rx_incr)

# For suit_lag, we will be using different levels for different devt age models, form those for 30 and 42:
claims$suit_lag_group_30 <- claims$suit_lag_group
levels(claims$suit_lag_group_30) <- c("NS", "lag_24", "lag_24", "lag_24", "lag_24_plus", "lag_24")
claims$suit_lag_group_42 <- ifelse(claims$suit_lag_group == "lag_30", 1, 0)

# Save
saveRDS(as.data.frame(claims), file = "../00-data/01-cm/cm-model-data-post-analysis.RDS")

