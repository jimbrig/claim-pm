library(caret)
library(e1071)
library(dplyr)
library(ggplot2)
library(corrplot)

#Purpose of this file is to perform a descriptive analysis of the data before the initial formation of the model:

#load data
sample <- readRDS("../00-data/model-claims-full.RDS")

# We will be forming different models for different devt periods..for the purpose of this analysis I will be looking at just claims at 
# 18, 30 and 42 months devt:
devt_ages <- c(18, 30, 42)
sample <- sample[sample$devt %in% devt_ages, ]
sample_18 <- sample[sample$devt == 18, ]
sample_30 <- sample[sample$devt == 30, ]
sample_42 <- sample[sample$devt == 42, ]

# 1) Analyze the distribution of Status at Age 30 (the response variable)

summary(sample_18$status_act)  #Of 10,101 claims at 18 mnths devt, 7573 are closed and 2528 are open in 12 months (25.03% open)
plot(sample_18$status_act)

summary(sample_30$status_act)  #Of 9125 claims at 30 months devt 7922 are closed and 1203 are open in 12 months
plot(sample_30$status_act)

summary(sample_42$status_act)  #Of 8213 claims at 42 months devt 7704 are closed and 514 are open in 12 months 
plot(sample_42$status_act)

# As you can see the class imbalance of closed v open in 12 months increases over time (i.e. less likely for claims to remain open the more
# developed they become), and our models will need to take this into consideration.

summary(sample_18$status_0_act) #Of 4988 claims that are closed at 18 mnths devt, 4842 remain closed at age 30, and 146 re-open.
plot(sample_18$status_0_act)    #Of 5113 claims that are open at 18 months devt, 2731 close by age 30, and 2382 remain open.

# Purpose of model will be to predict which of the closed claims will stay closed vs re-open and why? and for open claims, which will
# close out and which will stay open and why?


# Our initial set of predictors:
predictors_18 <- dplyr::select(sample_18, -eval, -devt, -claim_number, -tot_pd_incr_act)
predictors_30 <- dplyr::select(sample_30, -eval, -devt, -claim_number, -tot_pd_incr_act)
predictors_42 <- dplyr::select(sample_42, -eval, -devt, -claim_number, -tot_pd_incr_act)

# check for NZV variables:
caret::nearZeroVar(predictors_18)  #indicates suit_lag_group has too little variance
caret::nearZeroVar(predictors_30)  #indicates suit_lag_group, bi_rx, prop_rx, and newclm have too little variance
caret::nearZeroVar(predictors_42)  #same as 32

# For suit lag:
summary(sample_18[sample_18$status_act == "C",]$suit_lag)
summary(sample_18[sample_18$status_act == "O",]$suit_lag) # mean for open about 150 days higher
boxplot(suit_lag ~ status_act, data = sample_18)

summary(glm(status_act ~ suit_lag, data = sample_18, family = binomial))  #since suit_lag has so many NAs this model isnt very indicative

# We will want to use suit_lag group because as a factor, it can distiguish between non-suit and suit claims while simultaneously
# including levels for different suit_lag lengths for claims with suits, but need to figure out best way to level the factor
# so its not nzv:

summary(sample_18[sample_18$status_act == "C",]$suit_lag_group)
summary(sample_18[sample_18$status_act == "O",]$suit_lag_group)
plot(suit_lag_group ~ status_act, data = sample_18)

# Suit clms definitely indicative of staying open longer, but since its a nzv predictor we want to try and group together 
# levels with similar odds ratios in a logistic regression:

summary(glm(status_act ~ status + suit_lag_group, data = sample_18, family = binomial))
summary(glm(status_act ~ status + suit_lag_group, data = sample_30, family = binomial))
summary(glm(status_act ~ status + suit_lag_group, data = sample_42, family = binomial))


# Need to try and group together levels with similar log odds to increase the number of occurences in each factor level to help
# with newar zero variance problem:

# For devt = 18, all suit_lags are significant and dont seem to need to be altered
# For devt = 30, 24 seems like suitible cutoff
# For devt = 42, 30 seems like suitible cutoff

# To accomodate for different levels of significance between factor levels for different devt ages, 
# different levels should be made for each devt age:

# For 18 keep the same, for 30 relevel for just NS 24 and 24 plus.  For 42 just add variable for suit_lag_24_plus (seems like 
# as devt age gets bigger, the fact that a claim is suit vs non suit becomes less and less significant)

levels(sample_30$suit_lag_group) <- c("NS", "lag_24", "lag_24", "lag_24", "lag_24_plus", "lag_24")
sample_42$suit_lag_24_plus <- ifelse(sample_42$suit_lag_group == "lag_30", 1, 0)


summary(glm(status_act ~ status + suit_lag_group, data = sample_18, family = binomial))
summary(glm(status_act ~ status + suit_lag_group, data = sample_30, family = binomial))
summary(glm(status_act ~ status + suit_lag_24_plus, data = sample_42, family = binomial))


# Now lets look at rept lag:

boxplot(log(rept_lag + 1) ~ status_act, data = sample_42)  #rept lag not a good predictor, essentially exactly the same distributions
summary(glm(status_act ~ rept_lag, data = sample_18, family = binomial))

# exclude report lag

# Now report lag group

summary(glm(status_act ~ status + rept_lag_group, data = sample_18, family = binomial))
summary(glm(status_act ~ status + rept_lag_group, data = sample_30, family = binomial))
summary(glm(status_act ~ status + rept_lag_group, data = sample_42, family = binomial))

# lag_6 significant for age 18 but thats it, it also has negative coefficient which we dont necessarilly want..exclude rept lag group

# Now need to look at status vs status_1_0: 
plot(status_1_0 ~ status_act, data = sample_18)

summary(glm(status_act ~ status_1_0, data = sample_18, family = binomial))  #O_c not significant, NA_C not really either
summary(glm(status_act ~ status_1_0, data = sample_30, family = binomial))  #NA_C not significant
summary(glm(status_act ~ status_1_0, data = sample_42, family = binomial))  #NA_C not significant


# Lets check and see if this model is better than one simply using status:
mod1 <- glm(status_act ~ status_1_0, data = sample_18, family = binomial)
mod2 <- glm(status_act ~ status, data = sample_18, family = binomial)
anova(mod2, mod1, test = "LRT")
control <- trainControl(method = "repeatedcv", repeats = 10, p = .75)
train(status_act ~ status, data = sample_18, method = "glm", family = binomial, trControl = control)
train(status_act ~ status_1_0, data = sample_18, method = "glm", family = binomial, trControl = control)

# ANOVA indicative that status_1_0 is better, however accuracy is tiny bit smaller and has negative kappa..

mod1 <- glm(status_act ~ status_1_0, data = sample_30, family = binomial)
mod2 <- glm(status_act ~ status, data = sample_30, family = binomial)
anova(mod2, mod1, test = "LRT")

train(status_act ~ status, data = sample_30, method = "glm", family = binomial, trControl = control)
train(status_act ~ status_1_0, data = sample_30, method = "glm", family = binomial, trControl = control)

mod1 <- glm(status_act ~ status_1_0, data = sample_42, family = binomial)
mod2 <- glm(status_act ~ status, data = sample_42, family = binomial)
anova(mod2, mod1, test = "LRT")

train(status_act ~ status, data = sample_42, method = "glm", family = binomial, trControl = control)
train(status_act ~ status_1_0, data = sample_42, method = "glm", family = binomial, trControl = control)

# Status_1_0 not better than status, exclude



# Lastly lets check newclm

plot(newclm ~ status_act, data = sample_18)
summary(glm(status_act ~ newclm, data = sample_18, family = binomial))
summary(glm(status_act ~ status + newclm, data = sample_18, family = binomial))

# Odd...newclm is significant alone, but when added in with status its coefficient turns negative (not good b/c newclm should be indicative of
# higher liklihood of statying open) and it becomes insignificant newclm probably not a good predictor b/c its predictive abilities
# are already taken into account with just status.


# Conclusions for categorical predictors:
#  1) For suit_lag dont use continuous suit_lag, instead use suit_lag group.  
#          -For 18 model levels should be: NS, 6, 12, 18, 24, and 24+
#          -For 30 model levels should be: NS, 24, 24+
#          -For 42 model make new variable for 24+ only
#  2) Exclude rept lag and rept lag group
#  3) Use status not status_1_0
#  4) Exclude newclm


# Lets form an initial model with just these categorical variables and see how it performs:

mod18 <- glm(status_act ~ suit_lag_group + status, data = sample_18, family = binomial)
mod30 <- glm(status_act ~ suit_lag_group + status, data = sample_30, family = binomial)
mod42 <- glm(status_act ~ suit_lag_24_plus + status, data = sample_42, family = binomial) 
summary(mod18)
summary(mod30)
summary(mod42)  #insignificant NA_C okay



# Now need to go over continuous variables:

cont_preds_18 <- sample_18[, c(11:21)]
cont_preds_30 <- sample_30[, c(11:21)]
cont_preds_42 <- sample_42[, c(11:21)]

caret::featurePlot(x = log(sample_18[, c(11:16)] + 1),
                   y = sample_18$status_act,
                   plot = "density",
                   scales = list(x = list(relation = "free"),
                                 y = list(relation = "free")),
                   adjust = 1.5,
                   layout = c(3, 2),
                   auto.key = list(columns = 2))

# Looks like distribution doesnt vary between rx/pd type (i.e. densities look very similar across all three)

caret::featurePlot(x = log(sample_18[, c(11:16)] + 1),
                   y = sample_18$status_act,
                   plot = "box",
                   scales = list(x = list(relation = "free"),
                                 y = list(relation = "free")),
                   layout = c(3, 2),
                   auto.key = list(columns = 2))

# Form a model:

mod <- glm(status_act ~ suit_lag_group + status + bi_pd + bi_rx + prop_rx + prop_pd + exp_rx + exp_pd, 
           data = sample_18, family = binomial)

summary(mod)

train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_rx + exp_pd, 
               data = sample_18, 
               method = "glm",
               family = binomial,
               preProcess = c("center", "scale", "YeoJohnson"),
               trControl = trainControl(method = "repeatedcv", repeats = 3))

train$results
plot(varImp(train, scale = FALSE))


# Now for totals:

caret::featurePlot(x = log(sample_18[, c(17:21)] + 1),
                   y = sample_18$status_act,
                   plot = "density",
                   scales = list(x = list(relation = "free"),
                                 y = list(relation = "free")),
                   adjust = 1.5,
                   layout = c(3, 2),
                   auto.key = list(columns = 2))

caret::featurePlot(x = log(sample_18[, c(17:21)] + 1),
                   y = sample_18$status_act,
                   plot = "box",
                   scales = list(x = list(relation = "free"),
                                 y = list(relation = "free")),
                   layout = c(3, 2),
                   auto.key = list(columns = 2))


# tot_rx and tot_rx_incr look redundant, tot_inc not very good, tot_pd not good, tot_pd_incr not good

# Form a model:

mod <- glm(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + tot_rx, 
           data = sample_18, family = binomial)

summary(mod)

train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + tot_rx,
               data = sample_18, 
               method = "glm",
               family = binomial,
               preProcess = c("center", "scale", "YeoJohnson"),
               trControl = trainControl(method = "repeatedcv", repeats = 3))

train$results
plot(varImp(train, scale = FALSE))


# After inital analysis looks like our best continuous options are bi_pd, bi_rx, exp_pd, and tot_rx.
# but recall that bi_rx was a near zero variance predictor for later devt periods



train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + tot_rx,
               data = sample_18, 
               method = "glm",
               family = binomial,
               preProcess = c("center", "scale", "YeoJohnson"),
               trControl = trainControl(method = "repeatedcv", repeats = 3))

train$results 
summary(train)
plot(train$finalModel)

# Check for collinearities:
model_data_18 <- dplyr::select(sample_18, status_act, status, suit_lag_group, bi_pd, bi_rx, exp_pd, tot_rx)

correlations <- cor(model_data_18[, -c(1:3)])
corrplot(correlations, order = "hclust")

# bi_rx and tot_rx are almost perfectly correlated, but model with both has better accuracy and kappa so will
# keep both..

# Form final model:

train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
               data = sample_18, 
               method = "glm",
               family = binomial,
               preProcess = c("center", "scale", "YeoJohnson"),
               trControl = trainControl(method = "repeatedcv", repeats = 3))
train$results
summary(train$finalModel)



