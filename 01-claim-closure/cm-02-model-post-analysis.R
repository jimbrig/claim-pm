library(ggplot2) # for plotting
library(caret) # for modeling


# load data
# data created in 'cm-01-data-prep-post-analysis.R'
sample <- readRDS("../00-data/01-cm/cm-model-data-post-analysis.RDS")


# Form Models:
set.seed(1234)

train_18 <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
                  data = sample[sample$devt == 18, ],
                  method = "glm",
                  family = binomial,
                  subset = createDataPartition(sample[sample$devt == 18, ]$status_act, p = .75, list = FALSE),
                  preProcess = c("center", "scale", "YeoJohnson"),
                  trControl = trainControl(method = "repeatedcv", repeats = 5))

train_30 <- train(status_act ~ suit_lag_group_30 + status + bi_pd + bi_rx + tot_pd_incr,
                  data = sample[sample$devt == 30, ],
                  method = "glm",
                  family = binomial,
                  subset = createDataPartition(sample[sample$devt == 30, ]$status_act, p = .75, list = FALSE),
                  preProcess = c("center", "scale", "YeoJohnson"),
                  trControl = trainControl(method = "repeatedcv", repeats = 5))

train_42 <- train(status_act ~ suit_lag_group_42 + status + bi_pd + exp_pd,
                  data = sample[sample$devt == 42, ],
                  method = "glm",
                  family = binomial,
                  subset = createDataPartition(sample[sample$devt == 42, ]$status_act, p = .75, list = FALSE),
                  preProcess = c("center", "scale", "YeoJohnson"),
                  trControl = trainControl(method = "repeatedcv", repeats = 5))

mod_18 <- train_18$finalModel
mod_30 <- train_30$finalModel
mod_42 <- train_42$finalModel

summary(mod_18)
summary(mod_30)
summary(mod_42)

train_18$results
train_30$results
train_42$results

my_models_zero <- list(mod_18, mod_30, mod_42)

# save model for reuse
saveRDS(my_models_zero, file = "../00-data/01-cm/cm-models-post-analysis-logistics.RDS")
