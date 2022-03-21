# Dataprep roche leaflet

df_leaflet <- tibble(patient_id = c(1:537), 
              pcr_result = factor(c(rep(1, 85), rep(1, 17), rep(0, 4), rep(0, 431))), 
              ag_result = factor(c(rep(1, 85), rep(0, 17), rep(1, 4), rep(0, 431))))

#For binary classification, the first factor level is assumed to be the event.