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


sample$status_2_1 <- paste(sample$status_2, sample$status_1, sep = "_")
sample$status_2_1 <- factor(sample$status_2_1)
sample <- select(sample, -status_2)

set.seed(10)

rows <- sample(1:nrow(sample), nrow(sample) *0.75)
training <- sample[rows, ]
testing <- sample[-rows,]

summary(glm(status ~ status_2_1, data = training, family = binomial))

#Lets dummy code our own independent constrasts for status_2_1:  
numbers
form.contrasts <- function(dataset) {
  
  numbers <- table(dataset$status_2_1)
  
  dataset$c1 <- ifelse(dataset$status_2_1 == "NA_1", -2, 
                       ifelse(dataset$status_2_1 == "NA_0", -2, 1))
  
  dataset$c2 <- ifelse(dataset$status_2_1 == "NA_1", (1 / numbers[6]), 
                       ifelse(dataset$status_2_1 == "NA_0", -(1 / numbers[5]) , 0))
  
  dataset$c3 <- ifelse(dataset$status_2_1 == "0_1", (1 / (numbers[2] + numbers[4])), 
                       ifelse(dataset$status_2_1 == "1_1", (1 / (numbers[2] + numbers[4])), 
                       ifelse(dataset$status_2_1 == "1_0", -(1 / (numbers[1] + numbers[3])), 
                       ifelse(sample$status_2_1 == "0_0", -(1 / (numbers[1] + numbers[3])), 0))))
  
  dataset$c4 <- ifelse(dataset$status_2_1 == "0_1", (1 / numbers[2]) , ifelse(dataset$status_2_1 == "1_1", -(1 / numbers[4]), 0))
  dataset$c5 <- ifelse(dataset$status_2_1 == "1_0", (1 / numbers[3]), ifelse(dataset$status_2_1 == "0_0", -(1 / numbers[1]), 0))
  
  q <- dataset 
  return(q)
}


training <- form.contrasts(training)
testing <- form.contrasts(testing)
sample <- form.contrasts(sample)
contrasts <- select(training, c1, c2, c3, c4, c5)
cor(contrasts)
#Correlation matrix of each contrast group shows that they 
#are all completely uncorrelated

summary(glm(status ~ c1 + c2 + c3 + c5, data = training, family = "binomial"))
output <- glm(status ~ c1 + c2 + c3 , data = sample, family = "binomial")
summary(output)


#from this output, all three contrast groups are extremely significant
#For c1 this means that whether or not a claim is new as of age 18 (regardless of whether its open or closed)
#has an association with that claims status at age 30.
#For c2 this means that whether or not a claim is newly open at age 18 or newly closed at age 18 has
#an association with that claims status at age 30.
#For c3 this means that whether or not a non-new claim is open or closed at age 18 (regardless of its status at age 6)
#has an association with that claims status at age 30.
#For c4  this means that closed closed vs open closed makes a difference.
#now that we have our model set up with the contrast variables, we can interpret the coefficients/calculate odds ratios:

form.contrast.ors <- function(model, dataset) {
  
  numbers <- table(dataset$status_2_1)
  coefs <- coef(model)
  
  
  c1.new <- exp(coefs[2] * -2)
  c1.nn <- exp(coefs[2] * 1)
  c2.new.closed <- exp(coefs[3] * -(1 / numbers[5]))
  c2.new.open <- exp(coefs[3] * (1 / numbers[6]))
  c3.nn.open <- exp(coefs[4] * (1 / sum(numbers[c(2,4)])))
  c3.nn.closed <- exp(coefs[4] * -(1 / sum(numbers[c(1,3)])))
  c5.1_0 <- exp(coefs[5] * (1/ numbers[3]))
  c5.0_0 <- exp(coefs[5] * -(1/ numbers[1]))
  
  ors <- c(c1.new, c1.nn, c2.new.closed, c2.new.open,
           c3.nn.open, c3.nn.closed, c5.1_0, c5.0_0)
  names(ors) <- c("c1.new", "c1.nn", "c2.new.closed", "c2.new.open",
                  "c3.nn.open", "c3.nn.closed", "c5.1_0", "c5.0_0")
  return(ors)
}
  
ors <- form.contrast.ors(output, training)
ors


output <- glm(status ~ c1 + c2 + c3, data = sample, family ="binomial")
summary(output)
ors1 <- form.contrast.ors(output1, sample)
ors1

#All of these coefficients and odds ratios make sense.
#Lastly lets compare our model using the contrast variables with the model just using status_1 or just status_2_1:

output1 <- glm(status ~ status_1, data = sample, family = binomial)
exp(coef(output1))       #odds ratio of .0312 for closed claims at age 18 to be open at age 30.

output2 <- glm(status ~ status_2_1, data = sample, family = binomial)
exp(coef(output2))


output$deviance ; output1$deviance  ; output2$deviance 
#model with contrast variables has least deviance 

output$aic ; output1$aic ;output2$aic 
#model with contrast variables has smallest AIC

anova(output, output1, test = "Chisq")  
#Model with contrast variables is statistically significantly better at explaining
#the variance in status at age 30! (p-value of .00005138).

anova(output, output2, test = "Chisq")
#Model with contrast variables is statistically significantly better at explaining
#the variance in status at age 30! (p-value of 0).

#c1, c2, and c3 will be used instead of status_1 and/or status_2_1 in model:

sample <- select(sample, -status_2_1, -c4, -c5)   #will leave status_1 in model for reference purposes
predictors <- select(sample, -status, -status_1)

#Now for our last categorical variable, case weakening 
#(recall this is a 1 for claims that were open at age 18, 
#but had case rx decreases between age 6 and 18, and zeroes for all other claims)

summary(sample$case_weak)  #only 8.4% of total claims have this condition
table(sample$status, sample$case_weak) 
table(sample$status, sample$status_1)
#not very different (i.e. of all claims that were open at age 18, 46% stayed open at age 30,
#while of claims that were open at age 18 AND experienced case weakening b/w age 6-18, 
#44% were open at age 30, only 2% less)

#lets see if its significant alone:

output <- glm(status ~ case_weak, data = sample, family = binomial)
summary(output)  #very significant, but high AIC and deviance

exp(cbind(OR = coef(output), confint(output)))  
#although its significant, goes against intuition for claims that experienced case rx weakening
#to have increased odds (2.65) of being open at age 30.  Lets try analyzing it with c1, c2, and c3

n <- select(sample, case_weak, c1, c2, c3)
cor(n)
#correlated with c1 and c3 but not c2 (nothing too drastic however)

output <- glm(status ~ c1 + c2 + c3 + case_weak, data = sample, family = binomial)
summary(output)
#Now we get the negative coefficient we were looking for for case_weak

exp(cbind(OR = coef(output), confint(output)))
#odds ratios for c2 and c3 are indeterminate because they need to be multiplied by their weights (possible binary values) first:
#c1 odds ratio of .6972 given a claim is not new at age 18 (little higher than before, probabaly due to small
#amount of confounding w/ case_weak)
#case_weak odds ratio of .73 (case weakening has smaller effect on closures than non-new effect has)
#both CIs significant and less than one


#for c2 and c3:
coefs <- coef(output)
coefs <- c(-(1/2135), -(1/3152)) * c(coefs[3:4])
exp(coefs)  
#.1567 odds ratio for c2 given its a new closed claim (same as before, no evidence of confounding) 
#.25 odds ratio for c3 given its a non-new closed claim (same as before, no evidence of confounding)

coefs <- coef(output)
coefs <- c((1/3604), (1/1807)) * c(coefs[3:4]) 
exp(coefs)
#2.99 odds ratio for c2 given its a new open claim (same as before no evidence of confounding)
#11.64 odds ratio for c3 given its a non-new open claim (higher than before,
#was 10.54, evidence of some confounding w/ case_weak)

output1 <- glm(status ~ c1 + c2 + c3, data = sample, family = binomial)

output$deviance ; output1$deviance  #lower deviance
output$aic ; output1$aic  #lower AIC
anova(output, output1, test = "Chisq")

#Significant evidence that model is better with case_weak in it from anova (p-value of .00089)
#slight concern with its correlation with c1 and c3, and possible confounding, but will leave in for now.


#Of all our possible categorical/binary predictors, we will only use c1, c2, c3, and case_weak (possibly) 
#case weak should maybe be removed even though significant and improves model due to confounding issues and its small effect on 
#status at age 30 (will require furthur analysis).

sample <- select(sample, -bi_coll_1, -prop_coll_1, -exp_coll_1, -coll_1)  #remove collection variables

#Now onto our continuous predictors: There are 20 total possibilities (total net pd and total rx are just sums however)
#Instead of dealing with all the continuous predictors separately as we did for the categorical variables, to save time we will us a stepwise
#approach (forward, backward, and both) to quickly find just the significant predictors:


summary(predictors)




#Begin stepwise procedure:

#the warning for fitted probabilities numerically 0 or 1 that occurs for some of the models 
#is due to "serparation".
#in order to deal with separation issues, I will use a Firth logistic regression 
#which  penalizes likelihood methods due to separation (need R3.2.3 first to do this!).  
#For more information on how this model works reference the logisf webpage on CRAN:

library(logistf) #need R3.2.3

#Form various models to be tested:

nothing <- glm(status ~ 1, data=sample, family = "binomial")   
#regression of status against 1 (fitting just the inctercept), 
#this model is used as our "nothing model" in our stepwise analysis

initialmod <- glm(status ~ status_1 + suit_lag + tot_rx_1, data = sample, family ="binomial") 
#this is the model Andy initially used.

fullmod <- glm(status ~ c1 + c2 + c3 + case_weak + suit_lag + rept_lag + bi_rx_1 +
                 net_bi_pd_1 + prop_rx_1 + net_prop_pd_1 + exp_rx_1 +
                 net_exp_pd_1 + tot_net_pd_1 + tot_rx_1 + bi_rx_incr_1 +
                 net_bi_pd_incr_1 + prop_rx_incr_1 + net_prop_pd_incr_1 +
                 exp_rx_incr_1 + net_exp_pd_incr_1 + tot_net_pd_incr_1 + tot_rx_incr_1, data = sample, family = "binomial")    
#this model uses all possible predictors
# Got warning message here, so lets use the firth regression:

fullmod_firth <- logistf(sample$status ~ sample$status_1 + sample$status_2 + sample$suit_lag + sample$bi_rx_1 + sample$net_bi_pd_1 + sample$bi_inc_1 +
sample$prop_rx_1 + sample$net_prop_pd_1 + sample$exp_rx_1 + sample$net_exp_pd_1 + sample$tot_net_pd_1 +
sample$tot_rx_1 + sample$tot_net_inc_1 + sample$bi_rx_incr_1 + sample$net_bi_pd_incr_1 +
sample$bi_inc_incr_1 + sample$prop_rx_incr_1 + sample$net_prop_pd_incr_1 + sample$exp_rx_incr_1 +
sample$net_exp_pd_incr_1 + sample$tot_net_pd_incr_1 + sample$tot_rx_incr_1 + sample$tot_net_inc_incr_1)


backwards = step(fullmod) 

forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(fullmod)),
                direction="forward")

bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),
                direction="both",trace=0)

#Be wary of warnings!

formula(initialmod)
formula(fullmod)
formula(backwards)
formula(forwards)
formula(bothways)

initialmod$deviance; fullmod$deviance; backwards$deviance; forwards$deviance; bothways$deviance 
initialmod$aic; fullmod$aic; backwards$aic; forwards$aic; bothways$aic 

#fullmod has the lowest deviance (the best fit) obviously because it uses the most predictors, 
#but the step process penelizes models with excessive predictors.  From
#this analysis it looks like the backwards model is the best, has lowest AIC and
#only slightly higher deviance than forward/both

mymodel <- backwards

summary(mymodel)
#case_weak, bi_rx_1, and net_exp_pd_1 are all barely insignificant (p-values just over .05)

predictors <- select(sample, c1, c2, c3, case_weak, suit_lag, bi_rx_1, net_bi_pd_1, 
                     prop_rx_1, net_prop_pd_1, exp_rx_1, net_exp_pd_1, net_bi_pd_incr_1)

cor(predictors)

#from the correlation matrix we can see:
#   1) case_weak is still slightly correlated with c1 and c3.   (will drop case_weak if still insignificant after altering model)
#   2) BI (both rx and pd) is correllated with exp (both rx and pd)  (will have to pick between the two)
#   3) Net_bi_pd_incr_1 is perfectly correllated with net_bi_pd_1 (will have to pick between the two)

# Need to pick which is a better predictor between expense and BI variables b/c too highly correllated and causes confounding:

bis <- select(sample, status, bi_rx_1, net_bi_pd_1, net_bi_pd_incr_1)
exps <- select(sample, status, exp_rx_1, net_exp_pd_1)
cor(bis)
cor(exps)

#From Correllations, expense seems to be much more highly correllated w/ status than BI.  However, bi_rx_1 does have a decent correlation.
#Lets try removing net_bi_pd_1 and see what happens

mymodel <- update(mymodel, .~.-net_bi_pd_1)
summary(mymodel)

#now net_bi_pd_incr_1 is significant but its effect is minicule (small coefficient / odds ratios)

mymodel <- update(mymodel, .~.-bi_rx_1)
summary(mymodel)

mymodel <- update(mymodel, .~.-case_weak)
summary(mymodel)

#Everything looks good now significance wise, lets check the step procedure and see if it thinks anything else should be added in:

forwards1 = step(mymodel,
                scope=list(lower=formula(mymodel),upper=formula(fullmod)),
                direction="forward")
summary(forwards1)
formula(forwards1)
formula(mymodel)

mymodel <- glm(status ~ c1 + c2 + c3 + suit_lag + prop_rx_1 + net_prop_pd_1 + exp_rx_1 + net_bi_pd_1, data = sample, family = binomial)
# No Warning Message!

summary(mymodel)

predictors <- select(sample, c1, c2, c3, suit_lag, prop_rx_1, net_prop_pd_1, exp_rx_1, net_bi_pd_1)

#Now we have our model!
#Check for possible polynomial terms maybe? interaction terms? 

cor(predictors)


anova(mymodel, fullmod, test = "Chisq")

#Now for predictions:

# determine rows to be included in training data


set.seed(10)

rows <- sample(1:nrow(sample), nrow(sample) *0.75)
training <- sample[rows, ]
testing <- sample[-rows,]

training_mod <- glm(status ~ c1 + c2 + c3 + suit_lag + prop_rx_1 + prop_pd_1 + exp_rx_1 + bi_pd_1, data = training, family = binomial)
summary(training_mod)
anova(training_mod, test = "Chisq")   

# make predictions
training$prob <- round(predict(training_mod, type = "response"), 5) # probability
training$logit_scale <- round(predict(training_mod), 5) # linear portion of logit

#Lets check for linearity b/w logits and continuous predictors:

plot(training$suit_lag, training$logit_scale)  #Looks pretty linear
plot(training$prop_rx_1, training$logit_scale)  #decent
plot(training$net_prop_pd_1, training$logit_scale)   #decent
plot(training$exp_rx_1, training$logit_scale)  #good
plot(training$net_bi_pd_1, training$logit_scale)  #not very good

#A logistic regression model assumes linearity between the logits of the response variable and the predictors..will have to work on 
#this more.

# display some predictions
kable(head(training, 10), row.names = FALSE)

# plot the results
ggplot(training, aes(x = logit_scale, y = as.numeric(as.character(status)))) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_line(aes(y = prob), size = 2, colour = "#1177FF") +
  ylab("Probability Open at 30 Months") +
  xlab("Logit Score") +
  ggtitle("Logit Model for Open Claims at 30 Months Development") +
  xlim(c(-5, 5))


# Andy: automatically plot standard error bars
ggplot(training, aes(x = logit_scale, y = as.numeric(as.character(status)))) +
  geom_point(colour = "red", position = position_jitter(height = 0.1, width = 0.1)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1) + 
  ylab("Probability Open at 30 Months") +
  xlab("Logit Score") +
  ggtitle("Logit Model for Open Claims at 30 Months Development") +
  xlim(c(-5, 5))

# Andy: why does this claim have the lowest logit score?
training[training$logit == min(training$logit), ]


testing_mod <- glm(status ~ c1 + c2 + c3 + suit_lag + prop_rx_1 + prop_pd_1 + exp_rx_1 + bi_pd_1, data = training, family = binomial)
testing$prob <- predict(testing_mod, newdata = testing, type = "response")
testing$prediction <- ifelse(testing$prob > 0.5,1,0)

kable(head(testing, 10) , row.names = FALSE)
sum(testing$prob)
sum(testing$status)
#Only 11 open claims off out of 671 (over predicted which is better than under predicted)

683/671 -1
testing$error <- abs(testing$status - testing$prediction)
Error_rate <- sum(testing$error) / 2675
Error_rate
testing <- select(testing, -error)
#18% error rate much improved!

