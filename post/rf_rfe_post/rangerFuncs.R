# Backwards Feature Selection Helper Functions (aka RFE: recursive feature elimination)

# this version only calculates feature importance for the model that includes all the features (the first)
rangerFuncs <-  list(summary = defaultSummary,
                     fit = function(x, y, first, last, ...) {
                       loadNamespace("ranger")
                       dat <- if(is.data.frame(x)) 
                         x else as.data.frame(x)
                       dat$.outcome <- y
                       # return a model object to generate preds
                       ranger::ranger(.outcome ~ ., 
                                      data = dat, 
                                      importance = if(first) "permutation" else "none", # we use permutation importance 
                                      probability = FALSE, # for classification do majority vote instead of probability forest
                                      write.forest = TRUE, # Save ranger.forest object, required for prediction
                                      ...)
                     },
                     pred = function(object, x)  {
                       if(!is.data.frame(x)) x <- as.data.frame(x)
                       # remove as.data.frame(), needed for regression rfe
                       out <- predict(object, data = x, type = "response")$predictions
                       
                       if(object$treetype == "Probability estimation") {
                         # added as.data.frame(), needed for (multi?) classification
                         out <- as.data.frame(out)
                         #out <- cbind(pred = colnames(out)[apply(out, 1, which.max)], out) # https://github.com/topepo/caret/issues/1137
                         out$pred <- factor(colnames(out)[apply(out,1,which.max)], levels=sort(colnames(out)))
                         rownames(out) <- rownames(x)
                       }
                       return(out)

                     },
                     rank = function(object, x, y) {
                       if(length(object$variable.importance) == 0)
                         stop("No importance values available")
                       imps <- ranger:::importance(object)
                       vimp <- data.frame(Overall = as.vector(imps),
                                          var = names(imps))
                       rownames(vimp) <- names(imps)
                       
                       vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                       vimp
                     },
                     selectSize = pickSizeTolerance, # instead of pickSizeBest
                     selectVar = pickVars)

