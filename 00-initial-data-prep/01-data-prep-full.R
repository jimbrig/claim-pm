library(dplyr)
library(lubridate)

# load data
claims <- readRDS("../00-data/bi-claims.RDS")

# total rx and pd
claims$tot_rx <- claims$bi_rx + claims$prop_rx + claims$exp_rx
claims$tot_pd <- claims$bi_pd + claims$prop_pd + claims$exp_pd

claims$suit_lag <- as.Date(claims$suit_date) - as.Date(claims$dol)
claims$suit_lag <- ifelse(claims$suit_lag < 0, 0, claims$suit_lag)

claims$rept_lag <- as.Date(claims$report_date) - as.Date(claims$dol)
claims$rept_lag <- ifelse(claims$rept_lag <0, 0, claims$rept_lag)

claims$suit_lag_group <- cut(claims$suit_lag,
                             breaks = c(0, 182.5, 365, 547.5, 730, Inf), 
                             labels = c("lag_6", "lag_12", "lag_18", "lag_24", "lag_30"))

claims$rept_lag_group <- cut(claims$rept_lag,
                             breaks = c(0, 182.5, 365, 547.5, 730, Inf), 
                             labels = c("lag_6", "lag_12", "lag_18", "lag_24", "lag_30"))
# claims 
claims$suit_lag_group <- as.character(claims$suit_lag_group)
claims$suit_lag_group[is.na(claims$suit_lag_group)] <- "NS"
claims$suit_lag_group <- as.factor(claims$suit_lag_group)
claims$suit_lag_group <- relevel(claims$suit_lag_group, ref = "NS")

claims$rept_lag_group <- as.character(claims$rept_lag_group)
claims$rept_lag_group[is.na(claims$rept_lag_group)] <- "No Rept Date"
claims$rept_lag_group <- as.factor(claims$rept_lag_group)
claims$rept_lag_group <- relevel(claims$rept_lag_group, ref = "No Rept Date")

# select out columns we do not want and re-organize:
claims <- dplyr::select(claims, eval, claim_number, fiscal_year, devt, status, suit_lag, suit_lag_group, 
                        rept_lag, rept_lag_group, bi_rx, bi_pd, exp_rx, exp_pd, prop_rx, prop_pd, tot_rx, tot_pd, tot_inc)                                 

# find lags
claims <- dplyr::group_by(claims, claim_number) %>%
  dplyr::mutate(status_act = lead(status, order_by = devt),
                status_1 = lag(status, order_by = devt),
                tot_pd_act = lead(tot_pd, order_by = devt),
                tot_rx_1 = lag(tot_rx, order_by = devt),
                tot_pd_1 = lag(tot_pd, order_by = devt),
                tot_rx_incr = ifelse(is.na(tot_rx_1), tot_rx, tot_rx - tot_rx_1),
                tot_pd_incr = ifelse(is.na(tot_pd_1), tot_pd, tot_pd - tot_pd_1),
                tot_pd_incr_act = ifelse(is.na(tot_pd), tot_pd_act, tot_pd_act - tot_pd)) %>%
  dplyr::ungroup() %>%
  dplyr::select(eval, devt, claim_number, status_act, status, status_1, suit_lag, suit_lag_group, rept_lag, rept_lag_group, 
                bi_rx, bi_pd, exp_rx, exp_pd, prop_pd, prop_rx, tot_rx, tot_pd, tot_inc, tot_rx_incr, tot_pd_incr, tot_pd_incr_act)

claims <- claims[!claims$eval == "2015-11-30", ]

claims$status_act <- factor(claims$status_act)
claims$status <- factor(claims$status)
claims$status_1 <- factor(claims$status_1)

claims$newclm <- ifelse(is.na(claims$status_1), 1, 0) 
claims$status_1_0 <- factor(paste0(claims$status_1, "_" , claims$status))
claims$status_0_act <- factor(paste0(claims$status, "_" , claims$status_act))

saveRDS(as.data.frame(claims), file = "../00-data/model-claims-full.RDS")
