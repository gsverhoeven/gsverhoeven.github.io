# Backwards Feature Selection Helper Functions (aka recursive feature elimination)

# this version only calculates feature importance for the model that includes all the features (the first)
rangerTuneFuncs <-  list(summary = defaultSummary,
                     fit = function(x, y, first, last, ...){
                       if(first){ 
                         n_preds <- ncol(x)
                         # check mtry at different orders of magnitude
                         tune_grid <- expand.grid(mtry = round(exp(seq(log(1), log(n_preds), length.out= 2))), #length.out=sqrt(n_preds)))),
                                                  splitrule = "variance",
                                                  min.node.size = 10)
                         
                         train(x, y, 
                               method = "ranger", 
                               tuneGrid = tune_grid,
                               metric = "Rsquared",
                               importance = "permutation",
                               ...)
                       } else {
                         n_preds <- ncol(x)
                         # check mtry at different orders of magnitude
                         tune_grid <- expand.grid(mtry = round(exp(seq(log(1), log(n_preds), length.out= 2))), #length.out=sqrt(n_preds)))),
                                                  splitrule = "variance",
                                                  min.node.size = 10)
                         train(x, y, 
                               method = "ranger", 
                               tuneGrid = tune_grid, 
                               metric = "Rsquared",
                               importance = "none", ...)
                       }
                     },
                     pred = function(object, x)  {
                       if(!is.data.frame(x)) x <- as.data.frame(x)
                       out <- predict(object, x)
                       # Only works for Regression
                       # if(object$treetype == "Probability estimation") {
                       #   out <- cbind(pred = colnames(out)[apply(out, 1, which.max)],
                       #                out)
                       # }
                       out
                     },
                     rank = function(object, x, y) { # stolen from caretFuncs
                       vimp <- varImp(object, scale = FALSE)$importance
                       if(!is.data.frame(vimp)) vimp <- as.data.frame(vimp, stringsAsFactors = TRUE)
                       if(object$modelType == "Regression") {
                         vimp <- vimp[order(vimp[,1], decreasing = TRUE),,drop = FALSE]
                       } else {
                         if(all(levels(y) %in% colnames(vimp)) & !("Overall" %in% colnames(vimp))) {
                           avImp <- apply(vimp[, levels(y), drop = TRUE], 1, mean)
                           vimp$Overall <- avImp
                         }
                       }
                       vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                       vimp$var <- rownames(vimp)
                       vimp
                     },
                     selectSize = pickSizeTolerance, # instead of pickSizeBest
                     selectVar = pickVars)

