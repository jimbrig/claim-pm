library(caret)
library(ggplot2)
library(ROCR)

# Purpose of this file is to look at some other classification models and compare them to our 
# base logistic regression model:

sample <- readRDS("../00-data/01-cm/cm-model-data-post-analysis.RDS")


# Form Models (for purpose of this script will only look at devt = 18):
set.seed(1234)

# First lets form or base logistic regression model with logit link:
logmod_train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
                  data = sample[sample$devt == 18, ],
                  method = "glm",
                  family = binomial,
                  subset = createDataPartition(sample[sample$devt == 18, ]$status_act, p = .75, list = FALSE),
                  preProcess = c("center", "scale", "YeoJohnson"),
                  trControl = trainControl(method = "repeatedcv", repeats = 5))

logmod_train$results

# Form model using probit link instead of logit:
logmod_train_probit <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
                             data = sample[sample$devt == 18, ],
                             method = "glm",
                             family = binomial(link = "probit"),
                             subset = createDataPartition(sample[sample$devt == 18, ]$status_act, p = .75, list = FALSE),
                             preProcess = c("center", "scale", "YeoJohnson"),
                             trControl = trainControl(method = "repeatedcv", repeats = 5))

logmod_train_probit$results  #probit performed better (without binomial simulation that is)

# Form LDA Model:
LDA_train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
                             data = sample[sample$devt == 18, ],
                             method = "lda",
                             subset = createDataPartition(sample[sample$devt == 18, ]$status_act, p = .75, list = FALSE),
                             preProcess = c("center", "scale", "YeoJohnson"),
                             trControl = trainControl(method = "repeatedcv", repeats = 5))

LDA_train$results #same as logistics pretty much

#Form Random Forrest Model:
rf_train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
                             data = sample[sample$devt == 18, ],
                             method = "rf",
                             subset = createDataPartition(sample[sample$devt == 18, ]$status_act, p = .75, list = FALSE),
                             preProcess = c("center", "scale", "YeoJohnson"),
                             trControl = trainControl(method = "repeatedcv", repeats = 5))

rf_train$results # worse (barely)

#Form Bagged Tree Model:
baggedtree_train <- train(status_act ~ suit_lag_group + status + bi_pd + bi_rx + exp_pd + prop_pd + tot_rx + tot_pd_incr,
                             data = sample[sample$devt == 18, ],
                             method = "treebag",
                             subset = createDataPartition(sample[sample$devt == 18, ]$status_act, p = .75, list = FALSE),
                             preProcess = c("center", "scale", "YeoJohnson"),
                             trControl = trainControl(method = "repeatedcv", repeats = 5))


# Other classification models to try: C5.0, Boosted Tree, Robust LDA, Boosted Logistic, Neural Network,
# FDA, Sparse LDA, SVM (polynomial, robust, linbear, etc), glmnet, CARt, MDA, PLS, Nearest Shrunken Centroid,
# J48, PART, Naive Bayes, KNN, and more....

#ROC Curve plotting the performance function which checks how our model is performing:
test <- sample[sample$devt == 18, ]
test$probs <- predict(logmod_train, newdata = test, type = "prob")

test$preds <- sapply(test$probs$O, function(x) {
  rbinom(n = 1, size = 1, prob = x)
})

test$acts_num <- ifelse(test$status_act == "C", 0, 1)
logmod.scores <- ROCR::prediction(test$preds, test$acts_num)
plot(performance(logmod.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

