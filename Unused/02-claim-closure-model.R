
options(scipen=999)

# load packages
library(ggplot2) # for plotting
library(caret) # for modeling
library(corrplot)
library(dplyr)
library(tidyr)
library(ROCR)

#Assumptions of Logictic Regression model:
# a) Dependent variable is binary
# b) Model not over/ under fitted (only want meaningful predictors)
# c) Error terms need to be independent (independent observations)
# d) Little or no multicollinearity amongst predictors
# e) Linearity of independent variables w/ log odds of dependent variable

#Before thinking about the model we need to form our dataset (see data-prep for more details) 
#that will serve as our population (i.e. claims at 30 monthes devt):

# load data

sample <- as.data.frame(readRDS("data/bi-claims-30.RDS"))

sample$status_2_1 <- factor(ifelse(sample$status_1 == "C", "C", 
                            paste(sample$status_2, sample$status_1, sep = "_"))) #closed clm at 18 insignificant


sample <- select(sample, -status_1, - status_2, -status_1_0, -tot_pd_incr, -tot_rx_1, -tot_pd_1, -suit_1, -rept_lag) #unused here


#Restructure some factors:
levels(sample$suit_lag)
levels(sample$suit_lag) <- c("lag_12", "lag_18", "lag_24", "lag_30+", "lag_30+", 
                             "lag_30+", "lag_30+", "lag_30+", "lag_12", "lag_30+", 
                             "lag_30+", "NS")

sample$suit_lag <- relevel(sample$suit_lag, ref = "NS")

preds <- as.data.frame(sample[, -c(1:3, 10)])
transform <- preProcess(preds, method = c("YeoJohnson", "center", "scale"))
transform

sample <- predict(transform, sample)

#Form train and test datasets:
set.seed(1532)

statuses <- sample$status

training_rows <- caret::createDataPartition(statuses, p = .75, list = FALSE )

training <- sample[training_rows, ]

testing <- sample[-training_rows, ]

#Now lets fit a model:
names(training)

logmod_train <- train(status ~ ., data = training[, -c(6, 7, 10)], method = "glm", 
                      trControl = trainControl(method = "repeatedcv", repeats = 10))

logmod_train
summary(logmod_train)

VarImp <- varImp(logmod_train, scale = FALSE)
VarImp


training$logprobs <- predict(logmod_train, newdata = training, type = "prob")
testing$logprobs <- predict(logmod_train, newdata = testing, type = "prob")


training_cnts <- ifelse(training$status == "O", 1, 0)
testing_cnts <- ifelse(testing$status == "O", 1, 0)

sum(testing$logprobs$O)
sum(testing_cnts)  

sum(training$logprobs$O)
sum(training_cnts)


# Graphs

#Graph of Model in Training Set
training$logits <- log(training$logprobs$O / (1 - training$logprobs$O))
training$status_num <- ifelse(training$status == "O", 1, 0)

ggplot(training, aes(x = logits, y = status_num)) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability Open at 30 Months") +
  xlab("Logit Odds") +
  ggtitle("Logit Model for Open Claims at 30 Months Development") +
  xlim(c(-5, 5))



boxplot(logits ~ status_2_1, data = training)
boxplot(logits ~ suit_lag, data = training)

ggplot(training, aes(x = bi_rx_1, y = logits)) + geom_point() 


logmod.scores <- ROCR::prediction(testing$logprobs$O, testing$status)

#ROC Curve plotting the performance function which checks how our model is performing:

plot(performance(logmod.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

# TO DO:





