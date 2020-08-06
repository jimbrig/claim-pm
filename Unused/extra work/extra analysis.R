#form our own contrasts instead of using automated R ones:

training <- form_contrasts(training, training)
testing <- form_contrasts(training, testing)  #want to use weights from training set when contrasting

#Begin stepwise procedure:

fullmod <- glm(status ~ ., data = training, family = binomial)    

backwards = step(fullmod)   #wary of warnings!

summary(backwards)  #all but exp pd incr 1 significant

mymodel <- glm(status ~ c1 + c2 + c3 + suit_lag + prop_rx_1 + exp_rx_1 + bi_pd_incr_1 + prop_pd_incr_1, data = training, family = binomial)
summary(mymodel)

initialmod <- glm(status ~ status_1 + suit_lag + bi_rx_1, data = training, family = binomial)

simulate(1000, fullmod, mymodel, backwards)

mymodel <- update(backwards, .~.-exp_pd_1)

model_variables <- select(training, status, c1, c2, c3, suit_lag, prop_rx_1, exp_rx_1, 
                          bi_pd_incr_1, prop_pd_incr_1, exp_pd_incr_1)

cor(model_variables)

mymodel <- update(mymodel, .~.-exp_pd_incr_1)  #correlation above .5 w/ both bi_pd_incr_1 and exp_rx_1
summary(mymodel)

anova(mymodel, test = "Chisq")

forwards1 = step(mymodel,
                 scope=list(lower=formula(mymodel),upper=formula(fullmod)),
                 direction="forward")
formula(forwards1)

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
# since using age 6 and 18 values to predict Status at age 30
# removed all "incr's" (changes between age 18 and 30), because these values will not be available when making predictions.
names(sample)
sample <- select(sample, -eval, -devt, -fiscal_year, -claim_number, -bi_rx, -bi_pd, -bi_coll, -prop_rx
                 -prop_pd, -prop_coll, #-exp_rx, 
                 -exp_pd, -exp_coll, -bi_rx_incr, 
                 -bi_pd_incr, -prop_rx_incr, -prop_pd_incr, -exp_rx_incr, -exp_pd_incr)

#remove incurred columns b/c they are just sums of pd and rx
sample <- sample[, setdiff(names(sample), c("bi_inc_1", "tot_net_inc_1", "bi_inc_incr_1", "tot_net_inc_incr_1"))]


sample$status <- ifelse(sample$status == "C", 0, 1)  #setting Closed to to 0 and Open to 1
sample$status_1 <- ifelse(sample$status_1 == "C", 0, 1) #Status_1 represents clm status at 18 months devt
sample$status_2 <- ifelse(sample$status_2 == "C", 0, 1) #Status_2 represents clm status at 6 months devt

# Now we have our dataset: there are 24 total possible predictors that can be used to predict status at age 30

summary(sample$status)  #only 25% of claims in sample are open
predictors <- select(sample, -status)

# STEP 1: DESCRIPTIVE ANALYSIS OF DATA

# Lets look at the binary/categorical predictors first
# They include bi_coll, prop_coll, exp_coll, status_1, status_2, and case_weak : 

summary(sample$bi_coll_1) #only .13% of claims in sample have BI collections at age 18
summary(sample$prop_coll_1) #5.42% of claims in sample have Prop collections
summary(sample$exp_coll_1)  #.9% of claims in sample have exp collections

table(sample$status, sample$bi_coll_1)  
#not much variablility (i.e. ~25% of claims w/o bi collections are open, ~21% of claims with bi collection are open, 
#both close to population percent open)

table(sample$status, sample$prop_coll_1)
#not much difference from norm, slight increase of number of open claims from norm : (~25% for no collection and ~27% for collection are open)

table(sample$status, sample$exp_coll_1)
#very similar to prop_coll_1 (~25% for no collection and ~27% for collection are open)

#lets try combining them into one variable:

sample$coll_1 <- ifelse( (sample$bi_coll_1 + sample$prop_coll_1 + sample$exp_coll_1) >= 1, 1, 0)

table(sample$status, sample$coll_1)
#better but still nothing significant (~25% for no collection and 27% for collection are open)

#Doesn't seem like collections are going to be a very indicative predictor of status at age 30.

#To ensure this, lets run a simple logistic regression for each of the categorical collection variables against Status:

output <- glm(status ~ bi_coll_1, data = sample, family = binomial)
summary(output)  #not significant

output <- glm(status ~ prop_coll_1, data = sample, family = binomial)
summary(output)  #not significant
exp(coef(output))
exp(cbind(OR = coef(output), confint(output)))  #wide confidence interval for odds ratio, nothing significant

output <- glm(status ~ exp_coll_1, data = sample, family = binomial)
summary(output)  #not significant
exp(cbind(OR = coef(output), confint(output)))  #nothing significant from OR CI

output <- glm(status ~ coll_1, data = sample, family = binomial)
summary(output)  #not significant

##no evidence that collections at age 18 (both subdivided among claim types and aggregated) have an impact on status at age 30 by themselves.


#Lets look at status_1 and status_2 now:

table(sample$status, sample$status_1)

#of the 5,287 claims that were closed at Age 18, 160 of them re-opened by Age 30 (3%).
#of the 5,411 claims that were open at Age 18, 2511 of them remained open (46%), while 2900 closed b/w
#Age 18 and 30 (54%).

table(sample$status, sample$status_2)

#of the 720 claims that were closed at age 6, 72 re-opened by age 30 (10%)
#of the 4239 claims that were open at age 6, 873 were still open at age 30 (20.6%)

#Obviously status at age 6 and age 18 are going to be highly correlated with status at age 30,
#especially for claims that are closed at age 18 or 30, we would expect them to stay closed.
#however, we would not want to include both status_1 and status_2 in the same model because 
#of confounding..I think it would be beneficial to create a new variable combining status_1 and status_2
#and set it as a factor:

sample$status_2_1 <- paste(sample$status_2, sample$status_1, sep = "_")
sample$status_2_1 <- factor(sample$status_2_1)
##combining statuses (i.e. 1_0 represents Open -> Closed)

summary(sample$status_2_1)  #most of the claims in our sample had NAs for status at age 6 meaning they were new claims as of age 18.
table(sample$status, sample$status_2_1)

#Of Closed-Closed claims, 2.6% reopened between age 18 and 30
#of closed-open claims, 48% are still open at age 30
#of open-closed claims, 2% reopened between 18 and 30
#of open-open claims, 48% are still open
#from this initial analysis it seems like claim status at age 6 doesn't have a strong association with status at age 30
#we can see this because there is little to no difference between the percentages of closed claims that re-open b/w
#ages 18 and 30 conditioned on their status at age 6 and little to no difference in the percentages of open claims
#at age 18 that remain open at age 30 conditioned on their status at age 6 (i.e whether or not the claim re-opened
#b/w age 6 and 18, etc.)

#Therefore status_2 (status at age 6) by itself can be removed:
sample <- select(sample, -status_2)

#However we can still test and see whether status_2_1 is a better predictor than status_1 
#(we would think so because it incorporates more information about the claim's status at age 18
#i.e whether it is new as of 18 or not, whether it reopened between age 6 and 18, etc.)

#Lets look at our created status factor now in a regression (status_2_1):

output <- glm(status ~ status_2_1, data = sample, family = binomial)
summary(output)
exp(cbind(OR = coef(output), confint(output)))
## Closed - closed (intercept), re-opened, open-open, and NA - open claims are all extremely significant, but open-closed and NA - closed
## are not significant.  Lower AIC of 8899.4.  Odds ratio <1 for closed - closed; odds ratio >1 for re-opened, open-open, and NA-open
## open - closed and NA - closed are inconclusive/insignificant from OR CIs, all makes logical sense..

#lets now look at status regressed using just status_1:

output1 <- glm(status ~ status_1, data = sample, family = binomial)
summary(output1)
exp(cbind(OR = coef(output1), confint(output1)))

##obviously status_1 is extremely significantly associated with status at age 30, question is is status_1 or status_2_1 a better predictor? 
#(for now its hard to interpret status_2_1's coefficients/odds ratios, but we can look at the models test statistics compared to status_1 and
#run an anova to see if it significantly helps the model (using chi squared test to determine p-value):

output$deviance - output1$deviance  #status_2_1 has lower deviance but could just be b/c has more variables at play
anova(output, test = "Chisq")
anova(output1, test = "Chisq")
anova(output, output1, test = "Chisq") 
#anova test very significant (p-value of .0004382) that status_2_1 improved the model instead of just using status_1!
#keep status 2_1

#problem with status_2_1 is that it is a categorical variable with multiple levels of factors (six to be exact)
#and its regressed coefficients/calculated odds ratios are difficult to interpret.
#in order to solve this problem, the categorical predictor (with 6 levels) can be transformed into five dummy coded
#contrast variables, each contrasting a different categorized factor level with another categorized factor level:
#Reference word document called Contrast Group Table for more info on this process:

#in order to avoid correlation between our created contrast variables, specific coding is necessary:

numbers
numbers <- table(training$status_2_1)  #to determine values to divide by for dummy coding

training$c1 <- ifelse(training$status_2_1 == "NA_1", -2, ifelse(training$status_2_1 == "NA_0", -2, 1))
training$c2 <- ifelse(training$status_2_1 == "NA_1", (1 / numbers[6]), ifelse(training$status_2_1 == "NA_0", -(1 / numbers[5]) , 0))
training$c3 <- ifelse(training$status_2_1 == "0_1", (1 / (numbers[2] + numbers[4])), ifelse(training$status_2_1 == "1_1", 1 / (numbers[2] + numbers[4]), 
                                                                                            ifelse(training$status_2_1 == "1_0", -(1 / (numbers[1] + numbers[3])), 
                                                                                                   ifelse(training$status_2_1 == "0_0", -(1 / (numbers[1] + numbers[3])), 0))))
training$c4 <- ifelse(training$status_2_1 == "0_1", (1 / numbers[2]) , ifelse(training$status_2_1 == "1_1", -(1 / numbers[4]), 0))
training$c5 <- ifelse(training$status_2_1 == "1_0", (1 / numbers[3]), ifelse(training$status_2_1 == "0_0", -(1 / numbers[1]), 0))

contrasts <- select(training, status, c1, c2, c3, c4, c5)
cor(contrasts)

#Correlation matrix of each contrast group shows that they are all completely uncorrelated
#and can now be used as independent variables in the model.
#we can also see that c1, c4, and c5 are negatively correlated with status at age 30, while c2 and c3 are positively correlated with status.
#c4 and c5 have extremely low correlations with status however so they will likely not be good predictor varibles for the model.
#Lets check by running a regression using our new contrast variables instead of just status_2_1:

output <- glm(status ~ c1 + c2 + c3 + c4 + c5, data = training, family = binomial)
summary(output)

#from this output, c4 and c5 are insignificant, as thought.
#For c4, its insignificance indicates that if a claim is open at age 18, whether or not it re-opened or remained open since
#age 6 is unassociated (or association cannot be proven that is) with that claims status at age 30.
#For c5's insignificance, it indicates that for claims that are closed at age 18, whether or not the claim was closed
#out between age 6 and 18 or if it was closed at both ages is unassociated with the claims status at Age 30.

#The good news is that c1, c2, and c3 were all significant.
#Lets look at the model without c4 and c5:

output <- glm(status ~ c1 + c2 + c3, data = sample, family = binomial)
summary(output)

#from this output, all three contrast groups are extremely significant
#For c1 this means that whether or not a claim is new as of age 18 (regardless of whether its open or closed)
#has an association with that claims status at age 30.
#For c2 this means that whether or not a claim is newly open at age 18 or newly closed at age 18 has
#an association with that claims status at age 30.
#For c3 this means that whether or not a non-new claim is open or closed at age 18 (regardless of its status at age 6)
#has an association with that claims status at age 30.

#now that we have our model set up with the contrast variables, we can interpret the coefficients/calculate odds ratios:

coefs <- coef(output)
coefs
#The intercept is meaningless here because we will never have a 0 for each of the contrast variables
#Lets look at c1's coefficient/odds ratio first:

exp(coefs[2] * -2) 
#The coefficient of -.37956 for c1 means that if a claim is new as of age 18 (regardless of whether it is open or not),
#the odds of that claim being open at age 30 increase by exp(-.37956 * -2) or 2.13.  

exp(coefs[2])
#Its odds of being open at age 30 given that it is not a new claim at age 18 is exp(-.37956 * 1) or .68 (<1). 

#Now c2:

exp(coefs[3] * (-1/2135))  
#Odds ratio of .1567 for newly closed claims as of age 18 

exp(coefs[3] * (1/3604))
#Odds ratio of 2.998 for newly open claims at age 18.

#And c3 :

exp(coefs[4]*(1/1807))
#Odds ratio of 10.54 for non-new claims that are open at age 18. 

exp(coefs[4]*(-1/3152))
#Odds ratio is .259 for non-new claims that are closed as of age 18.

#All of these coefficients and odds ratios make sense.
#c2 and c3 are simply testing whether newly open vs newly closed (for c2) or whether
#non-new open vs non-new closed (c3) have an effect on status at age 30.  It is quite obvious
#that whether the claim is open or closed at age 18 will be significantly associated with status
#at age 30.  Whats interesting is that whether the claim is new at age 18 or not is also associated 
#with status at age 30 (regardless of whether or not it is open or closed at time 18).  This is shown 
#by the fact that c1 is significant and that the odds ratio for c3's non-new closed claims (.259) is higher
#than the odds ratio of c2's newly closed claims (.1567).  Therefore non-new closed claims at age 18 are 
#more likely to be re-opened by age 30 than new closed claims at age 18.

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

nothing <- glm(status ~ 1, data=training, family = "binomial")   
#regression of status against 1 (fitting just the inctercept), 
#this model is used as our "nothing model" in our stepwise analysis

initialmod <- glm(status ~ status_1 + suit_lag + tot_rx_1, data = training, family ="binomial") 
#this is the model Andy initially used.

fullmod <- glm(status ~. , data = training, family = "binomial")    
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

simulate(1000,backwards,fullmod,initialmod)

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

training_mod <- glm(status ~ c1 + c2 + c3 + suit_lag + prop_rx_1 + net_prop_pd_1 + exp_rx_1 + net_bi_pd_1, data = training, family = binomial)
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
