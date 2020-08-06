simulate_zeroes <- function(n, p) {
  
  set.seed(n) #use different seed than logistic regression so not overfitting data
  
  rows <- sample(1:nrow(sample), nrow(sample) *p)
  training <- sample[rows, ]
  
  
  training_open <- training[training$status == 1, ]
  #training_closed <- training[training$status == 0, ]
  
  training_open$zeroes <- as.numeric(ifelse(training_open$tot_pd_incr == 0, 1, 0))
  #training_closed$zeroes <- as.numeric(ifelse(training_closed$tot_pd_incr == 0, 1, 0))
  
  count_open <- sum(training_open$zeroes)
  #count_closed <- sum(training_closed$zeroes)
  
  output <- as.numeric(count_open)
  output
  
}

x <- runif(1000, 1, 10000)
x <- round(x[1:1000], 0)


simulation <- lapply(x, simulate_zeroes, .75)
hold <- lapply(simulation, as.data.frame)
m <- as.matrix(dplyr::bind_rows(hold))
dim(m) <- c(1, 1000)

sums <- apply(m, 1, sum)
mean <- apply(m, 1, mean)
vars <- apply(m, 1, var)

sums
mean
vars
