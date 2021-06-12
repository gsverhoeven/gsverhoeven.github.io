# dataprep

library(tidyverse)

# ## Study 1
# 
# 1. https://erj.ersjournals.com/content/57/4/2003961 (289 patients, 13.5% infected, N* = 244)
# 
# *Head-to-head comparison of SARS-CoV-2 antigen-detecting rapid test with self-collected nasal swab versus professional-collected nasopharyngeal swab*
#   
#   The study took place at the ambulatory SARS-CoV-2 testing facility of Charité University Hospital (Charité-Universitätsmedizin Berlin, Germany) from 23 September to 14 October 2020. 
# 

# 
df1 <- read.csv("sars_test/dataset_n287.csv", sep = " ", header = FALSE, na.strings = "X")

df1_names <- c("id", "ag_self_nmt", "ag_self_nmt_signal", "ag_prof_np", "ag_prof_np_signal", "ct_value",  "viral_load", "days_of_symptoms")
colnames(df1) <- df1_names

df1 <- df1 %>% pivot_longer(!c("id", "ct_value",  "viral_load", "days_of_symptoms"), 
                            names_to ="mm_type", 
                            values_to = "mm_value")
df1$study <- 1
df1$pcr_assay_type <- NA


#For study one, we want the `ag_self_nmt` variable. nmt stands for nasal measurement.
# PCR was on OP/NP swab.

## Study 2

# 2. https://erj.ersjournals.com/content/57/5/2004430 (179 patients, 22.9% infected, N* = 155)
# 
# *Head-to-head comparison of SARS-CoV-2 antigen-detecting rapid test with professional-collected nasal versus nasopharyngeal swab*
#   
#   Adults at high risk for SARS-CoV-2 infection according to clinical suspicion who attended the ambulatory SARS-CoV-2 testing facility of Charité University Hospital Berlin, Germany, were enrolled from 11 to 18 November 2020. Of 181 patients invited, 180 (99.4%) consented to participate. One patient was excluded as both swabs for the Ag-RDT could not be obtained. 



df2 <- read.csv("sars_test/dataset_n179.csv", sep = " ", header = FALSE, na.strings = "X")
df2_names <- c("id", "ag_prof_nmt", "ag_prof_nmt_signal", "ag_prof_np", "ag_prof_np_signal", "ct_value",  "viral_load", "days_of_symptoms")
colnames(df2) <- df2_names

df2 <- df2 %>% pivot_longer(!c("id", "ct_value",  "viral_load", "days_of_symptoms"), names_to ="mm_type", 
                            values_to = "mm_value")
df2$study <- 2
df2$pcr_assay_type <- NA


#For study two, we want the `ag_prof_nmt` variable. nmt stands for nasal measurement.
# PCR was on OP/NP swab.

## study 3

# 
# 3. https://www.medrxiv.org/content/10.1101/2021.01.06.20249009v1 (146 patients, N*=138, 27.4% infected), still under Review
# 
# *SARS-CoV-2 patient self-testing with an antigen-detecting rapid test: a head-to-head comparison with professional testing*
#   
#   The   study   took   place at the ambulatory SARS-CoV-2 testing   facility   of Charité-Universitätsmedizin Berlin, Germany,from 30 November to 11 December 2020. 

df3 <- read.csv("sars_test/dataset_n146.csv", sep = " ", header = FALSE, na.strings = "X")
df3 <- df3[,1:7]
df3$V0 <- as.integer(gsub('^*([0-9]+).*','\\1',df3$V1))
df3$V1 <- gsub('^*([0-9]+)(.*)','\\2',df3$V1)
df3 <- df3[, c(8, 1:3,5,4,6,7)]
df3_names <- c("id", "ag_self_nmt", "ag_prof_nmt", "ag_prof_np","pcr_assay_type", "ct_value",  "viral_load", "days_of_symptoms")
colnames(df3) <- df3_names
df3$ag_prof_nmt_signal <- gsub('^*([a-z.]+)(.*)','\\2',df3$ag_prof_nmt)
df3$ag_prof_nmt <- gsub('^*([a-z.]+)(.*)','\\1',df3$ag_prof_nmt)
df3$ag_prof_np_signal <- gsub('^*([a-z.]+)(.*)','\\2',df3$ag_prof_np)
df3$ag_prof_np <- gsub('^*([a-z.]+)(.*)','\\1',df3$ag_prof_np)

df3 <- df3 %>% pivot_longer(!c("id", "pcr_assay_type", "ct_value",  "viral_load", "days_of_symptoms"), names_to ="mm_type", 
                            values_to = "mm_value")
df3$study <- 3


#For study three, we want the `ag_self_nmt` variable. nmt stands for nasal measurement.
# PCR was on OP/NP swab.

# Tech note:
# The results for the Antigen test in the three studies are not entirely comparable:
# For two of the studies, patients themselves took the nasal samples, in a third study professionals took the samples.
# One of the studies (Study 3) has results that are consistent with the assumption that for anterior nose samples, the procedure is so simple that health workers can only improve very little on the results that patients themselves obtain.
# (33/40 match with PCR vs 34/40 match with PCR for the professionals)
# So for this blog post we simply pool the three studies and think of them as results that apply to self-testing by patients.

# Merged dataset

df_pcr_pos <- rbind(df1 %>% filter(mm_type == "ag_self_nmt"), 
            df2 %>% filter(mm_type == "ag_prof_nmt"), 
            df3 %>% filter(mm_type == "ag_self_nmt"))

df_pcr_pos$mm_value <- as.integer(as.factor(df_pcr_pos$mm_value ))-1

rm(df1, df2, df3, df1_names, df2_names, df3_names)


