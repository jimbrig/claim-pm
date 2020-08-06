library(caret)
library(e1071)

#End goal is to project claim's incremental payments between age 18 and 30 using claim specific information up to age 18.
#Need to do an in depth analysis of the data first to get an idea of the relationships at play:

#Load Data
sample <- read.csv("data/claims.csv", stringsAsFactors = FALSE)
sample <- sample[sample$devt == 30 & !is.na(sample$status_1), ] 

str(sample)
sample$status <- factor(sample$status)
sample$status_1 <- factor(sample$status_1)
sample$status_2 <- factor(sample$status_2)
sample$status_2_1 <- factor(sample$status_2_1)
sample$status_1_0 <- factor(sample$status_1_0)
sample$rept_lag <- as.numeric(sample$rept_lag)
sample$suit_lag <- as.numeric(sample$suit_lag)
sample$suit <- as.numeric(sample$suit)

#For the purposes of this study, lets limit the incremental payments to only zero or positive values:

sample <- sample[sample$tot_pd_incr >= 0, ]


incr_pmnt_data <- sample[, c("bi_pd_incr", "prop_pd_incr", "exp_pd_incr", "tot_pd_incr")]

summary(incr_pmnt_data)

sample <- sample[, -c(1:4, 13:21, 31, 33, 35, 37, 39, 41 )]  #remove info past age 18 (except status and tot_pd_incr)
names(sample)

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


#Lets deal with the outliers now:

quartiles <- summary(tot_pd_incr_nz)

IQR <- quartiles[5] - quartiles [2] 

mild_outlier_cutoff <- 1.5*IQR

mild_outliers <- tot_pd_incr_nz[tot_pd_incr_nz >= mild_outlier_cutoff]  #889 mild outliers (18.4% of nonzeroes)

extreme_outlier_cutoff <- 3*IQR

extreme_outliers <- tot_pd_incr_nz[tot_pd_incr_nz >= extreme_outlier_cutoff] #442 extreme outliers (9.15% of nonzeroes)

#Lets look at the distribution w/o the outliers:

tot_pd_incr_nz_lmtd_mild <- tot_pd_incr_nz[!tot_pd_incr_nz > mild_outlier_cutoff]

hist(tot_pd_incr_nz_lmtd_mild)  #Still heavily skewed, but much better!

#Lets look at the distribution of the outliers:

hist(mild_outliers)
hist(mild_outliers, ylim = c(0,200))  #looks like 400,000 would be a good cutoff point for our model from this histogram
                                      #only two total claims over 400k (both around 750-800k) out of the 10,681 total claims.

hist(mild_outliers[!mild_outliers > 400000])  #much better!

#Conclusion after univariate analysis of response variable:  
# 1) Distribution is heavily skewed right 
# 2) Distribution is zero heavy
# 3) Distribution is outlier heavy

#To deal with these problems we will have to do some data manipulation of our response.  
# 1) First, for the purposes of this study the two claims w/ pmnts above 500k will have to be excluded (can introduce a shock claim
#    part of the model later to deal with these two claims if deem necessary.)
# 2) Second, to deal with the outliers, two separate models will have to be formed, one for claims below
#    a certain threshold and the other for claims above (i.e. we will have to run a logistic regression
#    to determine probabilities for whether claims will be more or less likely to be above a certain threshold given claim specific 
#    information at age 18.)
# 3) The same approach will be taken for zero payments as mentioned for outliers.  A Logistic regression will be performed to determine
#    the liklihood of a claim having a pmnt of zero or non-zero and separate models will be formed respectively.
# 4) Different model for closed claims at age 18 and open claims at 18?

sample <- sample[!sample$tot_pd_incr > 400000, ]  #get rid of two "shocker claims" from data alltogether

                 

boxplot(tot_pd_incr ~ status, data = sample, main = "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30", 
                          xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

#Inconclusive b/c of distributional problems with response, lets look at outlier vs non-outliers and non-zeroes

tot_pd_incr_nz <- sample[sample$tot_pd_incr > 0, c("status", "tot_pd_incr")]
tot_pd_incr_nz_lmtd_mild <- tot_pd_incr_nz[!tot_pd_incr_nz$tot_pd_incr > mild_outlier_cutoff, ]
tot_pd_incr_nz_lmtd_extr <- tot_pd_incr_nz[!tot_pd_incr_nz$tot_pd_incr > extreme_outlier_cutoff, ]

hist(tot_pd_incr_nz$tot_pd_incr[tot_pd_incr_nz$tot_pd_incr < mild_outlier_cutoff])
hist(tot_pd_incr_nz$tot_pd_incr[tot_pd_incr_nz$tot_pd_incr < extreme_outlier_cutoff])


boxplot(tot_pd_incr ~ status, data = tot_pd_incr_nz, main = 
          "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30\n (non-zero pmnts only)", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

boxplot(tot_pd_incr ~ status, data = tot_pd_incr_nz_lmtd_mild, main = 
          "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30\n (non-zero pmnts lmtd to 1.5 X IQR only)", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

boxplot(tot_pd_incr ~ status, data = tot_pd_incr_nz_lmtd_extr, main = 
          "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30\n (non-zero pmnts lmtd to 3.0 X IQR only)", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

boxplot(tot_pd_incr ~ status_1_0, data = sample)


#For a more indepth classification analysis, let's make some new categorical variables: 
#One for zero vs. non-zero, and another for outlier vs. non-outlier (both mild and extreme).


hist(tot_pd_incr_nz$tot_pd_incr[tot_pd_incr_nz$tot_pd_incr < mild_outlier_cutoff])
hist(tot_pd_incr_nz$tot_pd_incr[tot_pd_incr_nz$tot_pd_incr < extreme_outlier_cutoff])
#not much difference b/w non-mild and non-extreme outliers distributions

hist(tot_pd_incr_nz$tot_pd_incr[tot_pd_incr_nz$tot_pd_incr > mild_outlier_cutoff])
hist(tot_pd_incr_nz$tot_pd_incr[tot_pd_incr_nz$tot_pd_incr > extreme_outlier_cutoff])
#not much difference b/w mild and extreme outliers distributions

#therefore safe to assume we can treat JUST extreme outliers as outliers and mild outliers as non-outliers.

sample$outliers <- factor(ifelse(sample$tot_pd_incr < extreme_outlier_cutoff, "NO", "YES"))
sample$zero_pmnt <- factor(ifelse(sample$tot_pd_incr == 0, "YES", "NO"))        

classes <- sample[, c("status", "outliers", "zero_pmnt", "tot_pd_incr")]

plot(status ~ outliers, data = classes)  #much more non-outliers than outliers, also larger percentage of open outliers
                                         #compared to open non-outliers

plot(status ~ zero_pmnt, data = classes)  #pretty evenly split between closed and open for non-zero pmnts
                                          #almost ALL of zero payments are coming from closed claims (~90%)
                                          #we are more worried about the 10% of open claims that have payments of zero

plot(zero_pmnt ~ outliers, data = classes) #obviously all zero-payments are non-outliers

#Lets look at a dataset where closed claims at age 18 are removed.

sample_open <- sample[sample$status_1 == "O", ]

hist(sample_open$tot_pd_incr)
summary(sample_open$tot_pd_incr)

#Lets look at the non-zero values only:

tot_pd_incr_nz_open_1 <- sample_open$tot_pd_incr[!sample_open$tot_pd_incr == 0]  
#of the 5,342 claims in our open dataset, 1,007 of them had incremental pmnts of zero (18.85%).

hist(tot_pd_incr_nz_open_1)  #skewed right
skewness(tot_pd_incr_nz_open_1) #still skewed (4.2)

#Still have a zero problem with our response, and some skewness but much better than full dataset including closed claims.
#Lets check for outliers:

quartiles <- summary(tot_pd_incr_nz_open_1)

IQR <- quartiles[5] - quartiles [2] 

mild_outlier_cutoff <- 1.5*IQR

mild_outliers <- tot_pd_incr_nz_open_1[tot_pd_incr_nz_open_1 >= mild_outlier_cutoff]  #758 mild outliers (17.5% of open nonzeroes)

extreme_outlier_cutoff <- 3*IQR

extreme_outliers <- tot_pd_incr_nz_open_1[tot_pd_incr_nz_open_1 >= extreme_outlier_cutoff] #357 extreme outliers (8.24% of open nonzeroes)

#Lets look at the distribution w/o the outliers:

tot_pd_incr_nz_open_lmtd_mild <- tot_pd_incr_nz_open_1[!tot_pd_incr_nz_open_1 > mild_outlier_cutoff]

hist(tot_pd_incr_nz_open_lmtd_mild)  #Still heavily skewed, but much better!

#Lets look at the distribution of the outliers:

hist(mild_outliers)

hist(extreme_outliers)


#Now lets see if theres a strong difference between incremental payments given the claims status at age 30:

boxplot(tot_pd_incr ~ status, data = sample_open, main = "Incremental Payments b/w Age 18 and 30 vs. 
        Status at Age 30 /n Open Claims at Age 18 Only", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

#Inconclusive b/c of distributional problems with response, lets look at outlier vs non-outliers and non-zeroes

tot_pd_incr_nz_open <- sample_open[sample_open$tot_pd_incr > 0, c("status", "tot_pd_incr")]
tot_pd_incr_nz_lmtd_mild_open <- tot_pd_incr_nz_open[!tot_pd_incr_nz_open$tot_pd_incr > mild_outlier_cutoff, ]
tot_pd_incr_nz_lmtd_extr_open <- tot_pd_incr_nz_open[!tot_pd_incr_nz_open$tot_pd_incr > extreme_outlier_cutoff, ]

hist(tot_pd_incr_nz_open$tot_pd_incr[tot_pd_incr_nz_open$tot_pd_incr < mild_outlier_cutoff])
hist(tot_pd_incr_nz_open$tot_pd_incr[tot_pd_incr_nz_open$tot_pd_incr < extreme_outlier_cutoff])


boxplot(tot_pd_incr ~ status, data = tot_pd_incr_nz_open, main = 
          "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30\n (non-zero pmnts and open claims at age 18 only)", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

boxplot(tot_pd_incr ~ status, data = tot_pd_incr_nz_lmtd_mild_open, main = 
          "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30\n (non-zero open age 18 pmnts lmtd to 1.5 X IQR only)", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

#Notice that for claims that close out, the average pmnt is higher, but there are no outliers!

boxplot(tot_pd_incr ~ status, data = tot_pd_incr_nz_lmtd_extr_open, main = 
          "Incremental Payments b/w Age 18 and 30 vs. Status at Age 30\n (non-zero open age 18 pmnts lmtd to 3.0 X IQR only)", 
        xlab = "Status at Age 30", ylab = "Total Gross Incremental Payment (18 - 30)")

#CONCLUSION:  SEEMS LIKE THERES LARGE DIFFERENCE IN INCREMENTAL PAYMENTS BASED ON A NUMBER OF DIFFERENT SCENARIOES.  

#FIRST NEED TO PREDICT WHAT A CLAIMS STATUS IS AT AGE 30 GIVEN ITS STATUS/RX/ETC. AT AGE 18. #FORM NEW CATEGORICAL 
#PREDICTOR STATUS_1_0* (FOR PREDICTED STATUS AT AGES 18 AND 30)

#SECOND FORM A MODEL PREDICTING WHETHER A SPECIFIC CLAIM GIVEN ITS PROJECTED STATUS_1_0 WILL HAVE AN INCREMENTAL PMNT OF ZERO.
#USE BINOMIAL TO SIMULATE ZEROES VS NON-ZEROES.

#THIRD: FOR PROJECTED NON-ZEROES, CALUCULATE PROBABILITIES OF OUTLIER VS NON-OUTLIER AND MULTIPLY THOSE PROBABILITIES BY THE
#PROJECTED PMNTS FROM EACH MODEL TO GET PROJECTED NON-ZERO PMNTS.




