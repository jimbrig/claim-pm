library(caret)
library(e1071)
library(corrplot)

#End goal is to project claim's incremental payments between age 18 and 30 using claim specific information up to age 18.
#Need to do an in depth analysis of the data first to get an idea of the relationships at play:

#Load Data
sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)
sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ]  #only want claims at 30 months devt

names(sample)
sample <- select(sample, status, status_1, status_2, status_1_0, status_2_1, 
                 rept_lag, suit_lag, suit_1, tot_rx_1, tot_pd_1, tot_rx_incr_1, tot_pd_incr_1, tot_pd_incr) #simplify sample


str(sample)
sample$status <- factor(sample$status)
sample$status_1 <- factor(sample$status_1)
sample$status_2 <- factor(sample$status_2)
sample$status_2_1 <- factor(sample$status_2_1)
sample$status_1_0 <- factor(sample$status_1_0)
sample$rept_lag <- as.numeric(sample$rept_lag)
sample$suit_lag <- as.numeric(sample$suit_lag)
sample$suit_1 <- as.numeric(sample$suit_1)

#For the purposes of this study, lets limit the incremental payments to only zero or positive values:

sample <- sample[sample$tot_pd_incr >= 0, ]


#First lets looks at the distribution of our final response variable we will be trying to predict: Total incremental payments

summary(sample$tot_pd_incr)  #large amount of zeroes have large effect on the distribution 
                             #(notice that median is zero, and third quartile is smaller than the mean)

#There is not only a heavy zero problem with our response, but also a large skewness in the distribution and 
#an outlier issue.  All of these issues will need to be considered before model formation.

#The large amount of skewness makes sense for our data, but lets see if we can detect whats causing the zero/outlier problems:

sample$zero <- ifelse(sample$tot_pd_incr == 0, 1, 0)
sum(sample$zero)  #Overall there are 5,853 incremental payments of zero!

sample_open_1 <- sample[sample$status_1 == "O", ]
sample_closed_1 <- sample[sample$status_1 == "C", ]

sum(sample_open_1$zero)
sum(sample_closed_1$zero) #most of the zeroes (82.8%) come from claims that are closed at age 18 (makes sense).


#Lets look at the non-zero values only:

tot_pd_incr_nz <- sample$tot_pd_incr[!sample$tot_pd_incr == 0]

hist(tot_pd_incr_nz)  #very skewed right
skewness(tot_pd_incr_nz) #large positive skewness of 19.5 (compared to almost 29 when zeroes are included)

hist(log(tot_pd_incr_nz)) #by taking the log we transformed the data and got rid of outliers!
skewness(log(tot_pd_incr_nz))


#Conclusion after univariate analysis of response variable:  
# 1) Distribution is heavily skewed right 
# 2) Distribution is zero heavy
# 3) Distribution is outlier heavy

#To deal with these problems we will have to do some data manipulation of our response. 
# 1) First, we will need to only form the model for non-zero values of the reponse.
# 2) Second, we will want to use the log transformed values of the reponse to deal with its skewness/outlier issues.


#Lets look at differences in non-zero incremental payments given status at age 30:

tot_pd_incr_nz <- sample[sample$tot_pd_incr > 0, c("status", "tot_pd_incr", "status_1_0")]

tot_pd_incr_nz$logpmnt <- log(tot_pd_incr_nz$tot_pd_incr)

boxplot(logpmnt ~ status, data = tot_pd_incr_nz, main = 
          "Log of Incremental Payments vs. Status at Age 30\n (non-zero pmnts only)", 
        xlab = "Status at Age 30", ylab = "Log Incremental Payments (18 - 30)")

# Distributions arent too different, what about with status_1_0:

boxplot(logpmnt ~ status_1_0, data = tot_pd_incr_nz, main =
          "Log of Incremental Payments vs. Status at Age 18 and 30\n (non-zero pmnts only)",
        xlab = "Status at Age 18 _ Status at Age 30", ylab = "Log Incremental Payments (18 - 30)")

#Notice that Open -> Closed and Open -> Open have outliers (mostly large ones), lets look at the histograms:

hist(tot_pd_incr_nz[tot_pd_incr_nz$status_1_0 == "C_C", ]$logpmnt, main =
       "Histogram of Log Pmnts for Closed -> Closed Claims b/w Age 18 and 30\n (non-zero pmnts only)",
     xlab = "Log Pmnts", ylab = "Occurences")

hist(tot_pd_incr_nz[tot_pd_incr_nz$status_1_0 == "C_O", ]$logpmnt, main =
       "Histogram of Log Pmnts for Re-Opened Claims b/w Age 18 and 30\n (non-zero pmnts only)",
     xlab = "Log Pmnts", ylab = "Occurences")

hist(tot_pd_incr_nz[tot_pd_incr_nz$status_1_0 == "O_C", ]$logpmnt, main =
       "Histogram of Log Pmnts for Open -> Closed Claims b/w Age 18 and 30\n (non-zero pmnts only)",
     xlab = "Log Pmnts", ylab = "Occurences")

hist(tot_pd_incr_nz[tot_pd_incr_nz$status_1_0 == "O_O", ]$logpmnt, main =
       "Histogram of Log Pmnts for Open -> Open Claims b/w Age 18 and 30\n (non-zero pmnts only)",
     xlab = "Log Pmnts", ylab = "Occurences")


#Slight differences in distributions base on statuses at ages 18 and 30 are noticeable in these histograms.


#CONCLUSION:  SEEMS LIKE THERES LARGE DIFFERENCE IN INCREMENTAL PAYMENTS BASED ON A NUMBER OF DIFFERENT SCENARIOES.  

#FIRST NEED TO PREDICT WHAT A CLAIMS STATUS IS AT AGE 30 GIVEN ITS STATUS/RX/ETC. AT AGE 18. #FORM NEW CATEGORICAL 
#PREDICTOR STATUS_1_0* (FOR PREDICTED STATUS AT AGES 18 AND 30)

#SECOND FORM A MODEL PREDICTING WHETHER A SPECIFIC CLAIM GIVEN ITS PROJECTED STATUS_1_0 WILL HAVE AN INCREMENTAL PMNT OF ZERO.
#USE BINOMIAL TO SIMULATE ZEROES VS NON-ZEROES.

#THIRD: FOR PROJECTED NON-ZEROES, PREDICT NON-ZERO PMNTS BETWEEN AGES 18 AND 30 USING FOUR DIFFERENT MODELS (ONE FOR EACH TYPE 
#OF STATUS CHANGE BETWEEN AGE 18 AND 30).  MODELS WILL BE FORMED USING LOG TRANSFORMED VALUES OF NON-ZERO PAYMENTS IN ORDER
#TO HELP WITH SKEWNESS AND OUTLIERS OF DISTRIBUITONS.


