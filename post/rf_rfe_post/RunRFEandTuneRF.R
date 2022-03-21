# Complete procedure: RFE + tuning

RunRFEandTuneRF <- function(X, Y, rfe_ctrl, train_ctrl, seed = 123){

  set.seed(seed)
  
  # RFE subsets to use during backward feature selection
  if(ncol(X) >= 10){
    subsets <- round(exp(seq(log(1), log(ncol(X)), length.out=sqrt(ncol(X))))) # can also use 2^(1:5)
  } else{ 
    subsets <- 1:(ncol(X) - 1)
  }
  
  print("starting the rfe procedure ..")
  
  rfeObject <- rfe(X, Y,
                sizes = subsets,
                rfeControl = rfe_ctrl)
  
  # step two: tune mtry for optimal set of predictors
  print("Tuning the model after feature elimination ...")
  
  # subset on selected features
  preds <- predictors(rfeObject)
  Xrfe <- X %>% select(all_of(preds))
  
  set.seed(seed)
  
  # check mtry at different orders of magnitude
  tune_grid <- expand.grid(mtry = round(exp(seq(log(1), log(length(preds)), length.out=sqrt(length(preds))))),
                           splitrule = "variance",
                           min.node.size = 5)
  
  trainObject <- train(x = Xrfe,
               y = Y,
               method = "ranger",
               trControl = train_ctrl,
               tuneGrid = tune_grid)
  
  return(list(rfeObject, trainObject))
}

RunRFEwithTuning <- function(X, Y, rfe_ctrl, train_ctrl, seed = 123){
  
  set.seed(seed)
  
  # RFE subsets to use during backward feature selection
  if(ncol(X) >= 10){
    subsets <- round(exp(seq(log(1), log(ncol(X)), length.out=sqrt(ncol(X))))) # can also use 2^(1:5)
  } else{ 
    subsets <- 1:(ncol(X) - 1)
  }
  
  print("starting the rfe procedure ..")

  rfeObject <- rfe(X, Y,
                   sizes = subsets,
                   rfeControl = rfe_ctrl,
                   trControl = train_ctrl
                   )

  return(list(rfeObject))
}

