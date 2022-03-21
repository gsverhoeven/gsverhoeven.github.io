add_spurious_predictors <- function(df, n_spur){
  n_obs <- nrow(df)
  
  mat_spur <- matrix(data = rnorm(n_obs * n_spur, mean = 0, sd = 1), nrow = n_obs, ncol = n_spur) 
  
  mat_spur <- data.frame(mat_spur)
  colnames(mat_spur) <- paste0("SPUR", 1:n_spur)
  
  return(cbind(df, mat_spur))
}