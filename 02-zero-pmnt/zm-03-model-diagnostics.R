library(corrplot)
library(caret)
library(ggplot2)
library(dplyr)
library(ROCR)

# Model Diagnostics/ Analysis

# load data
# data created in 'zm-01-data-prep.R'
sample <- readRDS("../00-data/02-zm/zm-model-data.RDS")
sample$zero <- relevel(sample$zero, ref = "Zero")

# load models 
my_models_z <- readRDS("../00-data/02-zm/zm-models.RDS")
mod_18 <- my_models_z[[1]]
mod_30 <- my_models_z[[2]]
mod_42 <- my_models_z[[3]]
mod_54 <- my_models_z[[4]]


# Plot correlation matrix (all ages):
cont_preds <- sample[, c("bi_pd", "bi_rx", "exp_pd", "exp_rx", "tot_rx_incr", "tot_pd_incr")]
correlations <- cor(cont_preds)
corrplot::corrplot(correlations, order = "hclust")  #tot_rx_incr correllated with bi_rx (.60144 OK)
                                                    #tot_pd_incr correllated with bi_pd (.49348 OK)

# Plot Variable Importance for each model 
mod_18_Imp <- caret::varImp(mod_18, scale = FALSE)
mod_30_Imp <- caret::varImp(mod_30, scale = FALSE)
mod_42_Imp <- caret::varImp(mod_42, scale = FALSE)
mod_54_Imp <- caret::varImp(mod_54, scale = FALSE)

plot(mod_18_Imp)
plot(mod_30_Imp)
plot(mod_42_Imp)
plot(mod_54_Imp)


# Make Predictions:

# Model Diagnostics: First make predictions using actual status_0_act factors (i.e. using the real status in 12 monthes):

sample_18 <- sample[sample$devt == 18, ]
sample_30 <- sample[sample$devt == 30, ]
sample_42 <- sample[sample$devt == 42, ]
sample_54 <- sample[sample$devt == 54, ]

samplePred_18 <- predict(mod_18, sample_18)
samplePred_30 <- predict(mod_30, sample_30)
samplePred_42 <- predict(mod_42, sample_42)
samplePred_54 <- predict(mod_54, sample_54)

# Form Confusion Matrices:

caret::confusionMatrix(samplePred_18, sample_18$zero)
caret::confusionMatrix(samplePred_30, sample_30$zero)
caret::confusionMatrix(samplePred_42, sample_42$zero)
caret::confusionMatrix(samplePred_54, sample_54$zero)

# Could plot accuracy/kappa/sensitivity/specificity/detection rates comparing across each devt if want to.

# Form probabilities and graph logistic curve:
devt_periods <- c(18, 30, 42, 54)
predict_sample <- sample

my_predictions <- vector("list", length = length(devt_periods))
for (i in seq_along(devt_periods)) {
  my_data <- predict_sample[predict_sample$devt == devt_periods[i], ]
  my_predictions[[i]] <- cbind(my_data, predict(my_models_z[[i]], newdata = my_data, type = "prob"))
  rm(my_data)
}

my_predictions <- bind_rows(my_predictions)

my_predictions$zero_sim <- sapply(my_predictions$Zero, function(x) {
  rbinom(n = 1, size = 1, prob = x)
})

group_by(my_predictions, devt) %>%
  summarise(Zero = sum(zero_sim))

my_predictions$zero_sim <- factor(ifelse(my_predictions$zero_sim == 1, "Zero", "NonZero"))

caret::confusionMatrix(my_predictions$zero_sim, my_predictions$zero)  #Confusion Matrix for all ages together

# Graph Logistic Curve:

my_predictions$logits <- log(my_predictions$Zero / my_predictions$NonZero)
my_predictions$zero_num <- ifelse(my_predictions$zero == "Zero", 1, 0)

ggplot(my_predictions, aes(x = logits, y = zero_num)) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability of Zero Pmnt - Devt Ages 18 - 66") +
  xlab("Logit Odds") +
  ggtitle("Logit Model for Zero Pmnt Claims at 18-54 Months Development \n (Using Actual Status in 12 Monthes)") +
  xlim(c(-10, 10))


# Now lets do the same process except using the simulated statuses in 12 months from the closure model:


# load closure data
predict_data <- readRDS("../00-data/01-cm/cm-model-data.RDS")

# load closure models
my_models_c <- readRDS("../00-data/01-cm/cm-models.RDS")

# Predict status in 12 monthes for each devt:
devt_periods <- c(18, 30, 42, 54)
predict_sample <- predict_data[predict_data$devt %in% devt_periods, ]

my_predictions <- vector("list", length = length(devt_periods))
for (i in seq_along(devt_periods)) {
  my_data <- predict_sample[predict_sample$devt == devt_periods[i], ]
  my_predictions[[i]] <- cbind(my_data, predict(my_models_c[[i]], newdata = my_data, type = "prob"))
  rm(my_data)
}

my_predictions <- bind_rows(my_predictions)

my_predictions$status_sim <- sapply(my_predictions$O, function(x) {
  rbinom(n = 1, size = 1, prob = x)
})

group_by(my_predictions, devt) %>%
  summarise(open = sum(status_sim))

my_predictions$status_sim <- factor(ifelse(my_predictions$status_sim == 1, "O", "C"))

# Make status_0_act variable except using simulated statuses from closure model instead of actual:
my_predictions$status <- factor(ifelse(my_predictions$status_1_0 == "C", "C", "O"))
my_predictions$status_0_act <- factor(paste0(my_predictions$status, "_" , my_predictions$status_sim))

# Re-level suit_lag for zero model:
levels(my_predictions$suit_lag_group) <- c("NS", "lag_12", "lag_18", "lag_18", "lag_18")

# Predict zero pmnts using zero mods:

predict_sample <- dplyr::select(my_predictions, -status_act, -status_1_0, -C, -O, -status_sim, -status)

my_predictions <- vector("list", length = length(devt_periods))
for (i in seq_along(devt_periods)) {
  my_data <- predict_sample[predict_sample$devt == devt_periods[i], ]
  my_predictions[[i]] <- cbind(my_data, predict(my_models_z[[i]], newdata = my_data, type = "prob"))
  rm(my_data)
}

my_predictions <- bind_rows(my_predictions)

my_predictions$zero_sim <- sapply(my_predictions$Zero, function(x) {
  rbinom(n = 1, size = 1, prob = x)
})

group_by(my_predictions, devt) %>%
  summarise(Zero = sum(zero_sim))

my_predictions$zero_sim <- factor(ifelse(my_predictions$zero_sim == 1, "Zero", "NonZero"))


# Add in actual zeroes for comparison:
zeroes <- sample[ , c("claim_number", "devt", "zero")] 
my_predictions <- left_join(my_predictions, zeroes, by = c("claim_number", "devt"))
my_predictions$zero <- factor(ifelse(my_predictions$zero == "Zero", "Zero", "NonZero"))

caret::confusionMatrix(my_predictions$zero_sim, my_predictions$zero)  #This model is worse than model using actual statuses in 12 monthes

# Graph logistic curve:

my_predictions$logits <- log(my_predictions$Zero / my_predictions$NonZero)
my_predictions$zero_num <- ifelse(my_predictions$zero == "Zero", 1, 0)

ggplot(my_predictions, aes(x = logits, y = zero_num)) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability of Zero Pmnt - Devt Ages 18 - 66") +
  xlab("Logit Odds") +
  ggtitle("Logit Model for Zero Pmnt Claims at 18-54 Months Development \n (Using Simulated Status in 12 Months from Closure Model)") +
  xlim(c(-10, 10))

logmod.scores <- ROCR::prediction(my_predictions$Zero, my_predictions$zero)

#ROC Curve plotting the performance function which checks how our model is performing:

plot(performance(logmod.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")




