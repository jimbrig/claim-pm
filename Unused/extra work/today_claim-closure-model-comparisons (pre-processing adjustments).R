library(caret)
library(MASS)
library(subselect)
library(rms)
library(pROC)

#Load Data
sample <- read.csv("data/claims_z.csv", stringsAsFactors = FALSE)
sample <- select(sample, -tot_pd_incr)

str(sample)

#Easier for logistic regression if status is numeric instead of a factor:
sample$status <- ifelse(sample$status == "C", 0, 1)  
sample$status_1 <- ifelse(sample$status_1 == "C", 0, 1) 
sample$status_2_1 <- ifelse(sample$status_1 == 0, 0, sample$status_2_1)  #closed clms not included in contrasted dummy varaibles
sample$status_2_1 <- factor(sample$status_2_1)                           #will be included in the intercept 
sample <- select(sample, -status_1)
                                                                                           

# Purpose of this analysis is to determine if the predictors for the closure model, and other models should be transformed/
# PreProcessed due to their heavily skewed, zero-heavy, and outlier heavy distributions.  We will run three logistic regressions
# on status at age 30, one using data as is, one with centered, scaled, and YJ transformed predictors, and one with PCA (Principle
# Component Analysis).

# For the purposes of this model formation, we will limit total incurred values at age 18 to 1,000,000:

sample <- sample[!sample$tot_inc_1 > 1000000, ]

transform_yj <- caret::preProcess(sample[, c(3, 4, 6:15)], method = c("YeoJohnson", "center", "scale"))
transform_yj

transform_pca <- preProcess(sample[, c(3, 4, 6:15)], method = c("YeoJohnson", "center", "scale", "pca"))
transform_pca

sample_yj <- predict(transform_yj, sample)
head(sample_yj[,1:15])

sample_pca <- predict(transform_pca, sample)
head(sample_pca[, 1:12])


#Form train and test datasets:
set.seed(1506)

statuses <- sample$status

training_rows <- caret::createDataPartition(statuses, p = .75, list = FALSE )


training <- sample[training_rows, ]
training_yj <- sample_yj[training_rows, ]
training_pca <- sample_pca[training_rows, ]


testing <- sample[-training_rows, ]
testing_yj <- sample_yj[-training_rows, ]
testing_pca <- sample_pca[-training_rows, ]




#Fit models:


#First I will fit a logistic regression to the non-transformed original data values:

logmod1 <- glm(status ~., data = training, family = binomial)
summary(logmod1) 

logmod1 <- update(logmod1, .~.-exp_pd_1) #highest p-value
logmod1 <- update(logmod1, .~.-tot_pd_incr_1) 
logmod1 <- update(logmod1, .~.-prop_rx_1)
logmod1 <- update(logmod1, .~.-exp_rx_1)
logmod1 <- update(logmod1, .~.-tot_rx_incr_1)
logmod1 <- update(logmod1, .~.-rept_lag)
summary(logmod1)  #maybe remove bi_rx_1 (almost significant)


training$logits <- predict(logmod1)
training$logprobs <- predict(logmod1, type = "response")
testing$logits <- predict(logmod1, newdata = testing)
testing$logprobs <- predict(logmod1, newdata = testing, type = "response")

sum(testing$logprobs)
sum(testing$status)   #Off by 2


ggplot(training, aes(x = logits, y = as.numeric(as.character(status)))) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability Open at 30 Months") +
  xlab("Logit Odds") +
  ggtitle("Logit Model for Open Claims at 30 Months Development No Pre-Processing")



#Now lets fit a model to our Yeo-Johnson transformed data

logmod2 <- glm(status~., data = training_yj, family = binomial)
summary(logmod2) #lower AIC than logmod1

logmod2 <- update(logmod2,.~.-tot_rx_1) #highest p-value
logmod2 <- update(logmod2,.~.-exp_pd_1) 
logmod2 <- update(logmod2,.~.-bi_rx_1)
logmod2 <- update(logmod2,.~.-prop_rx_1)
logmod2 <- update(logmod2,.~.-suit_lag)
logmod2 <- update(logmod2,.~.-exp_rx_1)


training_yj$logits <- predict(logmod2)
training_yj$logprobs <- predict(logmod2, type = "response")
testing_yj$logits <- predict(logmod2, newdata = testing_yj)
testing_yj$logprobs <- predict(logmod2, newdata = testing_yj, type = "response")

sum(testing_yj$logprobs)
sum(testing_yj$status)    #off by 4

ggplot(training_yj, aes(x = logits, y = as.numeric(as.character(status)))) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability Open at 30 Months") +
  xlab("Logit Odds") +
  ggtitle("Logit Model for Open Claims at 30 Months Development - Yeo Johnson Transformations") +
  xlim(c(-7, 7))



#Lastly our Principle Component Model

logmod3 <- glm(status~., data = training_pca, family = binomial)
summary(logmod3) #lower AIC than logmod1

logmod3 <- update(logmod3,.~.-PC4)




training_pca$logits <- predict(logmod3)
training_pca$logprobs <- predict(logmod3, type = "response")
testing_pca$logits <- predict(logmod3, newdata = testing_pca)
testing_pca$logprobs <- predict(logmod3, newdata = testing_pca, type = "response")

sum(testing_pca$logprobs)
sum(testing_pca$status)  #almost exact  


ggplot(training_pca, aes(x = logits, y = as.numeric(as.character(status)))) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability Open at 30 Months") +
  xlab("Logit Odds") +
  ggtitle("Logit Model for Open Claims at 30 Months Development - Principle Component Analysis Transformation") +
  xlim(c(-7, 7))


#No pre-processing model had the best testing rate (barely),
#but the PCA/YJ models had the best AIC/Deviance (model fit)

logmod1$aic ; logmod2$aic ; logmod3$aic
logmod1$deviance ; logmod2$deviance ; logmod3$deviance

#Further sampling is needed for model selection:


training$status_asfactor <- factor(ifelse(training$status == 1, "Open", "Closed"))
names(training)

logmod1_train <- train(status_asfactor~., data = training[, -c(1, 16, 17)], method = "glm", 
                      trControl = trainControl(method = "repeatedcv", repeats = 15))
logmod1_train



training_yj$status_asfactor <- factor(ifelse(training_yj$status == 1, "Open", "Closed"))
names(training_yj)

logmod2_train <- train(status_asfactor~., data = training_yj[,-c(1,16,17)], method = "glm", 
                       trControl = trainControl(method = "repeatedcv", repeats = 15))
logmod2_train


training_pca$status_asfactor <- factor(ifelse(training_pca$status == 1, "Open", "Closed"))
names(training_pca)

logmod3_train <- train(status_asfactor~., data = training_pca[,-c(1,13,14)], method = "glm", 
                       trControl = trainControl(method = "repeatedcv", repeats = 15))
logmod3_train


#Looks like YJ model is best (best accuracy and highest kappa), 
#what about ROC curve, sensitivity, and specificity?

ctrl <- trainControl(method = "repeatedcv", repeats = 15, 
                     summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)

set.seed(548)
yjmod <- train(status_asfactor~., data = training_yj[, -c(1, 12, 13)], 
               method = "glm", metric = "ROC", trControl = ctrl)
yjmod

head(yjmod$pred)



#Other Classification models to try out:

# 1) Logistic Regression including non-linear components
# 2) LDA (linear discriminant analysis)
# 3) Partial Least Squares Analysis
# 4) Penelized Models (ridge/firth/etc.)
# 5) Nearest sunken centroids
# 6) SVM Model (Support Vector Machine w/ kernel density)
# 7) Random Forrest Model
# 8) Quadratic Discriminant Analysis
# 9) Sigmoidal Model w/ corrected probabilities



knn_fit <- knn3(status ~., data = training_part, k = 5)
knn_fit

test_predictions <- predict(knn_fit, newdata = testing_part, type = "class")
summary(test_predictions)
summary(testing_part$status)  #NOT BAD

set.seed(25)

svm_fit <- train(status ~., data = training_part, method = "svmRadial", )

