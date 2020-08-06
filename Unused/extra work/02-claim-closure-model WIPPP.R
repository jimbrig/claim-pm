#Set wking directory 1st
options(scipen=999)

# load packages
library(ggplot2) # for plotting
library(caret) # for modeling
library(dplyr)
library(knitr)
library(aod)


#Assumptions of Logictic Regression model:
# a) Dependent variable is binary
# b) Model not over/ under fitted (only want meaningful predictors)
# c) Error terms need to be independent (independent observations)
# d) Little or no multicollinearity amongst predictors
# e) Linearity of independent variables w/ log odds of dependent variable

#Before thinking about the model we need to form our dataset (see data-prep for more details) 
#that will serve as our population (i.e. claims at 30 monthes devt):

# load data
sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)

# only trying to predict the claims at 30 months or 2.5 years development

sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ]       #filters out claims that do not have Age 18 values. 
                    
# remove unecessary columns and current value columns (other than current status) 
names(sample)
sample <- sample[ , -c(1:4, 8:16, 28, 30, 32, 34, 36, 38)]


sample$status <- ifelse(sample$status == "C", 0, 1)  #setting Closed to to 0 and Open to 1
sample$status_1 <- ifelse(sample$status_1 == "C", 0, 1) #Status_1 represents clm status at 18 months devt
sample$status_2 <- ifelse(sample$status_2 == "C", 0, 1) #Status_2 represents clm status at 6 months devt

# Now we have our dataset: there are 20 total possible predictors that can be used to predict status at age 30


##no evidence that collections at age 18 (both subdivided among claim types and aggregated)
##have a statistically significant impact on status at age 30 by themselves or with status_1.
sample <- select(sample, -bi_coll_1, -exp_coll_1, -prop_coll_1 )


sample$status_2_1 <- paste(sample$status_2, sample$status_1, sep = "_") #difference between closed claims isnt significant
sample$status_2_1 <- factor(sample$status_2_1)
sample <- select(sample, -status_2)
contrasts(sample$status_2_1)  #see how r automatically dummy codes the contrast groups


# Form training and testing set
set.seed(17)

rows <- sample(1:nrow(sample), nrow(sample) *0.75)
training <- sample[rows, ]
testing <- sample[-rows,]


#Begin stepwise procedure:

#Form various models to be tested:

nothing <- glm(status ~ 1, data = training, family = binomial)   
initialmod <- glm(status ~ status_1 + suit_lag + bi_rx_1, data = training, family = binomial) 
fullmod <- glm(status ~ ., data = training, family = binomial)    

backwards = step(fullmod)   #wary of warnings!

summary(backwards)  #all but exp pd incr 1 significant

mymodel <- glm(status ~ status_1 + suit_lag + bi_rx_1 + prop_rx_1 + exp_rx_1 + bi_pd_incr_1 + prop_pd_incr_1 + exp_pd_incr_1, data = training, family = binomial)
mymodel <- backwards

#mymodel <- update(mymodel, .~.-bi_pd_incr_1)
mymodel <- update(mymodel, .~.-exp_pd_1)
#mymodel <- update(mymodel, .~.-exp_rx_1)
mymodel <- update(mymodel, .~.-bi_pd_1)
#mymodel <- update(mymodel, .~.-prop_pd_1)

model_variables <- select(training, status, suit_lag, prop_rx_1, exp_pd_1, 
                     prop_rx_1, prop_pd_1, bi_pd_incr_1, exp_rx_incr_1)

cor(model_variables)


summary(mymodel)
anova(mymodel, test = "Chisq")


predictors <- select(training, suit_lag, prop_rx_1, prop_pd_1, exp_rx_1, bi_pd_incr_1, status_2_1)

#Now we have our model!

# make predictions
training$prob <- predict(mymodel, type = "response") # probability
training$logit_scale <- predict(mymodel) # linear portion of logit


# display some predictions
head(training, 10)


# Plot with Standard Error Bars
ggplot(training, aes(x = logit_scale, y = as.numeric(as.character(status)))) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability Open at 30 Months") +
  xlab("Logit Score") +
  ggtitle("Logit Model for Open Claims at 30 Months Development") +
  xlim(c(-5, 5))

# Andy: why does this claim have the lowest logit score?
training[training$logit == min(training$logit), ]


testing$prob <- predict(mymodel, newdata = testing, type = "response")
sum(testing$prob)
sum(testing$status)
# only off by three claims this time

testing$prediction = ifelse(testing$prob > .55, 1, 0)  # .55 yields better accuracy than .5 threshold for predictions
mean(testing$prediction == testing$status) #81.42% accuracy
table(testing$prediction , testing$status) #confusion matrix

#Check for possible polynomial terms maybe? interaction terms? 

#Lets check for linearity b/w logits and continuous predictors:

ggplot(training, aes(x = suit_lag, y = logit_scale)) + geom_point() #Looks pretty linear
ggplot(training, aes(x = prop_rx_1, y = logit_scale)) + geom_point()  #decent
ggplot(training, aes(x = prop_pd_1, y = logit_scale)) + geom_point() #decent
ggplot(training, aes(x = exp_rx_1,y = logit_scale)) + geom_point()  #good
ggplot(training, aes(x = bi_pd_incr_1, y = logit_scale)) + geom_point() +xlim(c(0,1000))  #bad

#A logistic regression model assumes linearity between the logits of the response variable and the predictors..will have to work on 
#this more.

