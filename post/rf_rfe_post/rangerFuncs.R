# Backwards Feature Selection Helper Functions (aka recursive feature elimination)

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
                                      probability = is.factor(y), # TRUE if the outcome is a factor
                                      write.forest = TRUE, # Save ranger.forest object, required for prediction
                                      ...)
                     },
                     pred = function(object, x)  {
                       #if(!is.data.frame(x)) x <- as.data.frame(x)
                       #out <- predict(object, x)$predictions
                       out <- as.data.frame(predict(object, data = x, type = "response")$predictions)
                       
                       if(object$treetype == "Probability estimation") {
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

