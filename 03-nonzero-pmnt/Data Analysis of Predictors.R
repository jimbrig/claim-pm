library(caret)
library(e1071)
library(corrplot)


#Load Data
sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)
sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ]  #only want claims at 30 months devt

#IDEA:
#Since we have total values for paid and reserves between the three types (BI, Prop, and Expense), instead of using those values 
#directly, which would bring collinearity into a model..we could instead create new variables that represent the weights or
#proportions of each of the three i.e. bi_rx_wt = .5 means half the total reserve is for BI..etc.

sample$bi_rx_1 <- ifelse(sample$tot_rx_1 == 0, 0, sample$bi_rx_1 / sample$tot_rx_1)
sample$prop_rx_1 <- ifelse(sample$tot_rx_1 == 0, 0, sample$prop_rx_1 / sample$tot_rx_1)
sample$exp_rx_1 <- ifelse(sample$tot_rx_1 == 0, 0, sample$exp_rx_1 / sample$tot_rx_1)

sample$bi_pd_1 <- ifelse(sample$tot_pd_1 == 0, 0, sample$bi_pd_1 / sample$tot_pd_1)
sample$prop_pd_1 <- ifelse(sample$tot_pd_1 == 0, 0, sample$prop_pd_1 / sample$tot_pd_1)
sample$exp_pd_1 <- ifelse(sample$tot_pd_1 == 0, 0, sample$exp_pd_1 / sample$tot_pd_1)

#Not going to worry about percentages for incremental payments for now, just overall values..


names(sample)
sample <- select(sample, status, status_1, status_2_1, rept_lag, suit_lag, suit_1, bi_rx_1, bi_pd_1, bi_coll_1,
                 prop_rx_1, prop_pd_1, prop_coll_1, exp_rx_1, exp_pd_1, exp_coll_1, tot_rx_1, tot_pd_1, 
                 tot_rx_incr_1, tot_pd_incr_1, tot_pd_incr)



#After our analysis of the final response variable, we want to limit our sample to only non-zero incremental payments,
#and use the log as our response.

sample <- sample[sample$tot_pd_incr > 0, ]
sample$logpmnt <- log(sample$tot_pd_incr)

#First we need to narrow down our possible predictors and get rid of ones that aren't feasible for modeling:
#(near zero variances, multicollinearities, etc.)

names(sample)
continuous_predictors <- sample[, c(4, 5, 7:19)]

caret::nearZeroVar(continuous_predictors)  #all three collections should go               
names(continuous_predictors)               

sample <- select(sample, -bi_coll_1, -prop_coll_1, -exp_coll_1)
continuous_predictors <- select(continuous_predictors, -bi_coll_1, -prop_coll_1, -exp_coll_1)

skew_values <- apply(continuous_predictors, 2, e1071::skewness)
skew_values

#Almost all predictors are heavily skewed right (bi_rx_1 skewed left), could cause some issues.  Might want to consider preProcessing.


correlations <- cor(continuous_predictors)
corrplot::corrplot(correlations, order = "hclust")

correlations
highcorr <- findCorrelation(correlations, cutoff = .7) #algorithm on page 47 of book
head(highcorr)  #indicates that tot_pd_1 and tot_rx_1 should be removed due to their multicollinearities with the incrs...
                #however, we need something in the model to be indicative of how large claims are, therefore if i am going to 
                #get rid of them I will do this: create a total incurred predictor and include ratios for paid versus rx of the incurred
                #amount..one of them will have to go due to independence issues but we will re-run the algortihm to see which one:

sample$tot_inc_1 <- sample$tot_rx_1 + sample$tot_pd_1
sample$tot_rx_1 <- ifelse(sample$tot_inc_1 == 0, 0, sample$tot_rx_1 / sample$tot_inc_1)
sample$tot_pd_1 <- ifelse(sample$tot_inc_1 == 0, 0, sample$tot_pd_1 / sample$tot_inc_1)

names(sample)
continuous_predictors <- sample[, c(4, 5, 7:16, 19)]

nearZeroVar(continuous_predictors) #GOOD

skew_values <- apply(continuous_predictors, 2, skewness)
skew_values  #Now tot_rx_1 has left skew

correlations <- cor(continuous_predictors)
corrplot::corrplot(correlations, order = "hclust")

correlations
highcorr <- findCorrelation(correlations, cutoff = .7) #algorithm on page 47 of book
head(highcorr)

#REMOVE TOT_PD_1

sample <- select(sample, -tot_pd_1)
continuous_predictors <- select(continuous_predictors, -tot_pd_1)

#NOW WE HAVE AN INITIAL SET OF PREDICTORS THAT CAN BE USED FOR MODELING THEY INCLUDE:
# 1) Status/status_1/status_2_1
# 2) Report Lag in Days (Report Date - Accident Date)
# 3) Suit Lag in Days (Suit Date - Accident Date)
# 4) Suit_1 (binary), whether or not the claim filed suit before as of 18 months devt.
# 5) bi_rx_1 - proportion of reserve at 18 months allocated to BI
# 6) bi_pd_1 - propportion of total paid at 18 months allocated to BI
#                                 .
#                                 .
#                                 .
# 11) Total Reserve at age 18 as proportion of Total Incurred at age 18
# 12) tot_rx_incr_1 - reserve change in dollars between age 6 and 18
# 13) tot_pd_incr_1 - paid amount change in dollars between age 6 and 18
# 14) tot_inc_1 - total incurred amount at age 18


#Lets save a copy of this newly formed dataset so these steps wont have to be repeated in the future for mdoel formation..
#remember that for the claim closure model and the zero-indicator model our dataset should include zero pmnts:

#For non-zeroes (including logpmnt columnn):
write.csv(sample, file = "data/claims_nz.csv", row.names = FALSE)

#Including Zeroes 
sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)
sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ]
sample$bi_rx_1 <- ifelse(sample$tot_rx_1 == 0, 0, sample$bi_rx_1 / sample$tot_rx_1)
sample$prop_rx_1 <- ifelse(sample$tot_rx_1 == 0, 0, sample$prop_rx_1 / sample$tot_rx_1)
sample$exp_rx_1 <- ifelse(sample$tot_rx_1 == 0, 0, sample$exp_rx_1 / sample$tot_rx_1)

sample$bi_pd_1 <- ifelse(sample$tot_pd_1 == 0, 0, sample$bi_pd_1 / sample$tot_pd_1)
sample$prop_pd_1 <- ifelse(sample$tot_pd_1 == 0, 0, sample$prop_pd_1 / sample$tot_pd_1)
sample$exp_pd_1 <- ifelse(sample$tot_pd_1 == 0, 0, sample$exp_pd_1 / sample$tot_pd_1)

sample <- select(sample, status, status_1, status_2_1, rept_lag, suit_lag, suit_1, bi_rx_1, bi_pd_1, bi_coll_1,
                 prop_rx_1, prop_pd_1, prop_coll_1, exp_rx_1, exp_pd_1, exp_coll_1, tot_rx_1, tot_pd_1, 
                 tot_rx_incr_1, tot_pd_incr_1, tot_pd_incr)

sample <- select(sample, -bi_coll_1, -prop_coll_1, -exp_coll_1)

sample$tot_inc_1 <- sample$tot_rx_1 + sample$tot_pd_1
sample$tot_rx_1 <- ifelse(sample$tot_inc_1 == 0, 0, sample$tot_rx_1 / sample$tot_inc_1)
sample$tot_pd_1 <- ifelse(sample$tot_inc_1 == 0, 0, sample$tot_pd_1 / sample$tot_inc_1)

sample <- select(sample, -tot_pd_1)

write.csv(sample, file = "data/claims_z.csv", row.names = FALSE)
















#Now we need to look at the distributions of our predictors (by themselves, against our response, and between eachother):

names(sample)

plot(logpmnt ~ rept_lag, data = sample)

#Lets look at tot_rx_1:

summary(sample$tot_rx_1)
hist(sample$tot_rx_1)

tot_pd_incr_nz <- sample[sample$tot_pd_incr > 0, c("tot_pd_incr", "tot_rx_1")]

tot_pd_incr_nz$logpmnt <- log(tot_pd_incr_nz$tot_pd_incr)

plot(logpmnt ~ tot_rx_1, data = tot_pd_incr_nz) #not very linear b/c of zero-heavy tot_rx_1s, lets try some transformations:

tot_pd_incr_nz$logrx <- log(tot_pd_incr_nz$tot_rx_1 + 1)

plot(logpmnt ~ logrx, data = tot_pd_incr_nz)

tot_pd_incr_nz$cubrx <- tot_pd_incr_nz$tot_rx_1 ^(1/3)

plot(logpmnt ~ log(cubrx + .001), data = tot_pd_incr_nz)