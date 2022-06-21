# result$r.squared <- 1 - result$prediction.error / var(y)
# ranger() uses this definition, this corresponds to the recommended def in Kvalseth
bootstrap_r_squared <- function(obs, pred, n = 1000){
  if(length(obs) != length(pred)) stop("obs and pred differ in length")
  n_obs <- length(obs)
  r_squared <- c()
  for(i in 1:n) {
    indices <- sample(1:n_obs, n_obs, replace = TRUE)
    prediction.error <- mean((obs[indices] - pred[indices])^2)
    r_squared[i] <- 1 - prediction.error / var(obs[indices])
  }
  
  r_squared
} 

# caret uses a correlation based R2
bootstrap_r_squared_corr <- function(obs, pred, n = 1000){
  if(length(obs) != length(pred)) stop("obs and pred differ in length")
  n_obs <- length(obs)
  r_squared <- c()
  for(i in 1:n) {
    indices <- sample(1:n_obs, n_obs, replace = TRUE)
    r_squared[i] <- cor(obs[indices],pred[indices])^2
  }
  
  r_squared
}

# Max Kuhn refers to this paper when defending the R2 choice of caret
# in the APM book it is made explicit that R2 (in caret) is a measure of correlation, not accuracy

# Cautionary Note about R 2
# Tarald O. Kvalseth
# 
# The coefficient of determination (R 2) is perhaps the single most extensively used measure of goodness of fit for regression models.
# It is also widely misused. The primary source of the problem is that except for linear models with an intercept term, 
# the several alternative R 2 statistics are not generally equivalent. This article discusses various considerations and potential pitfalls 
# in using the R 2's. Specific points are exemplified by means of empirical data. A new resistant statistic is also introduced.


bootstrap_accuracy <- function(obs, pred, n = 1000){
  if(length(obs) != length(pred)) stop("obs and pred differ in length")
  n_obs <- length(obs)
  accuracy <- c()
  for(i in 1:n) {
    indices <- sample(1:n_obs, n_obs, replace = TRUE)
    accuracy[i] <- mean(obs[indices] == pred[indices])
  }
  accuracy
}

