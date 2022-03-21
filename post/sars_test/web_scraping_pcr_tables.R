library(tidyverse)
library(magrittr)
library(rvest)

# Web scraping supplementary tables for Gertjan Verhoeven's blog post
# https://gsverhoeven.github.io/post/covid_antigen_test_reliability/

parse_ers_table = function(url) {
  read_html(url) %>%
    html_element("#table-1") %>%
    html_table() %>%
    head(-2) %>%
    tail(-2) %>%
    set_colnames(c("id", "ag_self_nmt", "ag_prof_np", "ct_value", "viral_load", "days_of_symptoms")) %>%
    separate(ag_self_nmt, into=c("ag_self_nmt", "ag_self_nmt_signal"), sep=" ") %>%
    separate(ag_prof_np,  into=c("ag_prof_np", "ag_prof_np_signal"  ), sep=" ") %>%
    mutate(ct_value = str_extract(ct_value, "\\d+\\.\\d+"))
}

parse_ers_table("https://erj.ersjournals.com/content/57/4/2003961.figures-only") %>% write_delim("df_287.csv")
parse_ers_table("https://erj.ersjournals.com/content/57/5/2004430.figures-only") %>% write_delim("df_179.csv")