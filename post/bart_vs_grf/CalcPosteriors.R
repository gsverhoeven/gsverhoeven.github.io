# THis requires a bart fit with a test set that has treatment reversed

CalcPosteriorsBART <- function(sim, bartFit, treatname = "z"){
  
  # calc ITE
  sim$ite <- bartFit$yhat.train.mean - bartFit$yhat.test.mean
  sim$ite <- ifelse(sim[[treatname]] == 0, -1*sim$ite, sim$ite)
  
  # calc posterior on ITE
  sim$ql <- apply(bartFit$yhat.train - bartFit$yhat.test, length(dim(bartFit$yhat.test)), quantile,probs=0.05)
  sim$qm <- apply(bartFit$yhat.train - bartFit$yhat.test, length(dim(bartFit$yhat.test)), quantile,probs=.5)
  sim$qu <- apply(bartFit$yhat.train - bartFit$yhat.test, length(dim(bartFit$yhat.test)), quantile,probs=0.95)
  
  # fix sign for non treated obs
  sim$ql <- ifelse(sim[[treatname]] == 0, -1*sim$ql, sim$ql)
  sim$qm <- ifelse(sim[[treatname]] == 0, -1*sim$qm, sim$qm)
  sim$qu <- ifelse(sim[[treatname]] == 0, -1*sim$qu, sim$qu)

  sim
}

CalcPredictionsGRF <- function(X.test, grf.fit){
  tau.hat <- predict(grf.fit, X.test, estimate.variance = TRUE)
  
  sigma.hat <- sqrt(tau.hat$variance.estimates)
  
  X.test <- data.frame(X.test, 
                       qm = tau.hat$predictions)
  
  
  X.test <- X.test %>% mutate(ql =  qm - 1.96 * sigma.hat, 
                             qu = qm + 1.96 * sigma.hat)
  X.test
}