# data prep to make BI claims dataset
# the output of this data prep script will be used as the
# input for all other data prep scripts

library(dplyr)
library(lubridate)



# read in the data
claims_2003 <- read.csv("00-data/00-original/revised-fedex-al-2002-11-30.csv", stringsAsFactors = FALSE)
claims_2004 <- read.csv("00-data/00-original/revised-fedex-al-2003-11-30.csv", stringsAsFactors = FALSE)
claims_2005 <- read.csv("00-data/00-original/revised-fedex-al-2004-11-30.csv", stringsAsFactors = FALSE)
claims_2006 <- read.csv("00-data/00-original/revised-fedex-al-2005-11-30.csv", stringsAsFactors = FALSE)
claims_2007 <- read.csv("00-data/00-original/revised-fedex-al-2006-11-30.csv", stringsAsFactors = FALSE)
claims_2008 <- read.csv("00-data/00-original/revised-fedex-al-2007-11-30.csv", stringsAsFactors = FALSE)
claims_2009 <- read.csv("00-data/00-original/revised-fedex-al-2008-11-30.csv", stringsAsFactors = FALSE)
claims_2010 <- read.csv("00-data/00-original/revised-fedex-al-2009-11-30.csv", stringsAsFactors = FALSE)
claims_2011 <- read.csv("00-data/00-original/original-fedex-al-2010-11-30.csv", stringsAsFactors = FALSE)
claims_2012 <- read.csv("00-data/00-original/original-fedex-al-2011-11-30.csv", stringsAsFactors = FALSE)
claims_2013 <- read.csv("00-data/00-original/original-fedex-al-2012-11-30.csv", stringsAsFactors = FALSE)
claims_2014 <- read.csv("00-data/00-original/original-fedex-al-2013-11-30.csv", stringsAsFactors = FALSE)
claims_2015 <- read.csv("00-data/00-original/original-fedex-al-2014-11-30.csv", stringsAsFactors = FALSE)
claims_2016 <- read.csv("00-data/00-original/original-fedex-al-2015-11-30.csv", stringsAsFactors = FALSE)

# claims evaluated at 2015-11-30 have 2 additional columns
# I am removing the two additional columns to make one large dataset
# the two additional columns will be added in again later

# 1: extract suit date from 2016 data and rename
lit <- claims_2016[, c("Fiscal.Year", "Claim.Number", "Suit.Date")]
names(lit) <- c("fiscal_year", "claim_number", "suit_date")
# 2: remove the columns
claims_2016 <- dplyr::select(claims_2016, -Litigation, -Suit.Date)


# set eval date for each data set
claims_2003$eval <- as.Date("2002-11-30")
claims_2004$eval <- as.Date("2003-11-30")
claims_2005$eval <- as.Date("2004-11-30")
claims_2006$eval <- as.Date("2005-11-30")
claims_2007$eval <- as.Date("2006-11-30")
claims_2008$eval <- as.Date("2007-11-30")
claims_2009$eval <- as.Date("2008-11-30")
claims_2010$eval <- as.Date("2009-11-30")
claims_2011$eval <- as.Date("2010-11-30")
claims_2012$eval <- as.Date("2011-11-30")
claims_2013$eval <- as.Date("2012-11-30")
claims_2014$eval <- as.Date("2013-11-30")
claims_2015$eval <- as.Date("2014-11-30")
claims_2016$eval <- as.Date("2015-11-30")

# create identical names for each data set
names(claims_2015) <- names(claims_2016)
names(claims_2014) <- names(claims_2015)
names(claims_2013) <- names(claims_2014)
names(claims_2012) <- names(claims_2013)
names(claims_2011) <- names(claims_2012)
names(claims_2010) <- names(claims_2011)
names(claims_2009) <- names(claims_2010)
names(claims_2008) <- names(claims_2009)
names(claims_2007) <- names(claims_2008)
names(claims_2006) <- names(claims_2007)
names(claims_2005) <- names(claims_2006)
names(claims_2004) <- names(claims_2005)
names(claims_2003) <- names(claims_2004)

# combine data sets
claims <- rbind(claims_2016, claims_2015, claims_2014, claims_2013, claims_2012,
                claims_2011, claims_2010, claims_2009, claims_2008, claims_2007,
                claims_2006, claims_2005, claims_2004, claims_2003)

# remove claims from fiscal years prior to 2002
claims <- claims[claims$Fiscal.Year >= 2003, ]

# remove original claim information from working directory
rm(list = c(setdiff(ls(), c("claims", "lit"))))

# there are both positive and negative collections in data, for purposes of 
# this study, I will assume all collections are negative (i.e. when added to paid amounts they
# decrease paid amounts)

claims$BI.Collection <- -abs(claims$BI.Collection)
claims$PD.Collection <- -abs(claims$PD.Collection)
claims$Expense.Collection <- -abs(claims$Expense.Collection)


# We only want to deal with BI claims (only use claims w/ BI inc > 0)
# we are defining BI claims as any claim with BI incurred >= 1 at any evaluation time
# (e.g. a claim with BI incurred of < 1 at its first evaluation and BI incurred >= 1 at its 
# second evaluation will be included as a BI claim at both evaluation times.
# to do this we need to find all claims with BI incurred >= 1 at any evaluation time

claims$bi_inc <- claims$BI.Paid + claims$BI.Collection + claims$BI.Reserve

bi_only <- claims %>%
  mutate(incurred_nz = ifelse(bi_inc >= 1, 1, 0)) %>%
  filter(incurred_nz == 1) %>%
  as.data.frame()

bi_only <- unique(bi_only$Claim.Number)

# filter out claims that never have BI incurred greater than zero
claims <- filter(claims, Claim.Number %in% bi_only)

claims <- dplyr::select(claims, -bi_inc)


# Now we have our initial dataset. 
# Using these variables, we want to construct a universal set of all possible variables that can be used later in our regression
# models : 

#Company code is always three and by definition inc sum is not independent of other variables, so they can be removed.
claims <- dplyr::select(claims, -Company.Code, -Incurred.Sum)

# update column names
names(claims) <- c("fiscal_year", 
                   "claim_number",
                   "dol",
                   "report_date", 
                   "bi_rx" , 
                   "bi_pd", 
                   "bi_coll" , 
                   "prop_rx" , 
                   "prop_pd" , 
                   "prop_coll", 
                   "exp_rx", 
                   "exp_pd",
                   "exp_coll",
                   "status", 
                   "eval")

# create column for months of development for each fiscal year
claims$eval_2 <- as.numeric(paste0(year(claims$eval), ".5"))
claims$devt <- (claims$eval_2 - claims$fiscal_year + 1) * 12
claims <- dplyr::select(claims, -eval_2)

# Create Suit Date:
# 1: remember the lit data frame we created earlier

# 2: join lit date to database and convert it to a POSIX date
claims <- left_join(claims, lit, by = c("fiscal_year", "claim_number"))

# create binary operator for whether or not a suit has occurrred
claims$suit_date[claims$suit_date == ""] <- NA
claims$suit_date <- as.Date(claims$suit_date)
claims$report_date <- as.Date(claims$report_date)
claims$dol <- as.Date(claims$dol)

# removing all claims that have negative incremental payments at
# any age of development
claims$tot_pd <- claims$bi_pd + claims$prop_pd + claims$exp_pd

tot_pd_incr_neg <- claims %>% group_by(claim_number) %>%
  mutate(tot_pd_1 = lag(tot_pd, order_by = devt),
         tot_pd_incr = ifelse(is.na(tot_pd_1), tot_pd, tot_pd - tot_pd_1)) %>%
  filter(tot_pd_incr < 0) %>%
  as.data.frame()

tot_pd_incr_neg <- unique(tot_pd_incr_neg$claim_number)
  
# filter out claims that have negative incremental payments
claims <- filter(claims, !(claim_number %in% tot_pd_incr_neg))
claims <- dplyr::select(claims, -tot_pd)

qs::qsave(as.data.frame(claims), "00-data/bi-claims.qs")
# saveRDS(as.data.frame(claims), "00-data/bi-claims.RDS")

# make claims for models

# total rx and pd
claims$tot_rx <- claims$bi_rx + claims$prop_rx + claims$exp_rx
claims$tot_pd <- claims$bi_pd + claims$prop_pd + claims$exp_pd

claims$suit_lag <- as.Date(claims$suit_date) - as.Date(claims$dol)
claims$suit_lag <- ifelse(claims$suit_lag < 0, 0, claims$suit_lag)

claims$suit_lag_group <- cut(claims$suit_lag,
                             breaks = c(0, 365, 547.5, 730, Inf), 
                             labels = c("lag_12", "lag_18", "lag_24", "lag_30"))

# claims 
claims$suit_lag_group <- as.character(claims$suit_lag_group)
claims$suit_lag_group[is.na(claims$suit_lag_group)] <- "NS"
claims$suit_lag_group <- as.factor(claims$suit_lag_group)
claims$suit_lag_group <- relevel(claims$suit_lag_group, ref = "NS")

# select out columns we do not want and re-organize:
claims <- dplyr::select(claims, claim_number, eval, devt, status, suit_lag_group, 
                        bi_rx, bi_pd, exp_rx, exp_pd, tot_rx, tot_pd)                                 

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
  dplyr::select(eval, devt, claim_number, status_act, status, status_1, suit_lag_group, bi_rx, 
                bi_pd, exp_rx, exp_pd, tot_rx_incr, tot_pd_incr, tot_pd_incr_act, tot_rx)


saveRDS(as.data.frame(claims), file = "../00-data/model-claims.RDS")
