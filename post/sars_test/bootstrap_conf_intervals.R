fullrun <- 0
set.seed(123)
options(yardstick.event_first = FALSE)

spec_vec <- c()
sens_vec <- c()

n <- 1e4 # 100s

#  sample with replacement, and calculate the sensitivity and specificity on the samples
if(fullrun){
  for (i in 1:n){
    sel_vec <- sample(1:nrow(df_leaflet), size = nrow(df_leaflet), replace = TRUE)
    df_sample <- df_leaflet[sel_vec,]
    spec_vec[i] <- yardstick::spec(df_sample, pcr_result, ag_result) %>% 
      pull(.estimate)
    sens_vec[i] <- yardstick::sens(df_sample, pcr_result, ag_result) %>% 
      pull(.estimate)
  }
  saveRDS(spec_vec, "spec_vec.rds")
  saveRDS(sens_vec, "sens_vec.rds")
} else {
  spec_vec <- readRDS("spec_vec.rds")
  sens_vec <- readRDS("sens_vec.rds")
}
