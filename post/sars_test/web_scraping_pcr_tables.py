# Web scraping supplementary tables for Gertjan Verhoeven's blog post
# https://gsverhoeven.github.io/post/covid_antigen_test_reliability/

import pandas as pd

def parse_ers_table(url):
  return (pd
          .read_html(url)[0]
          .head(-2)
          .set_axis(["id", "ag_self_nmt", "ag_prof_np", "ct_value", "viral_load", "days_of_symptoms"], axis=1)
          .assign(
            ag_self_nmt_signal = lambda row: row.ag_self_nmt.str.split(' ').str[1],
            ag_prof_np_signal = lambda row: row.ag_prof_np.str.split(' ').str[1],
            ag_self_nmt = lambda row: row.ag_self_nmt.str.split(' ').str[0],
            ag_prof_np = lambda row: row.ag_prof_np.str.split(' ').str[0],
            ct_value = lambda row: row.ct_value.str.split(r'\d+.\d+').str[0]
          )
          .astype({'days_of_symptoms': float})
          .astype({'days_of_symptoms': 'Int64'})
          .loc[:, ["id", "ag_self_nmt", "ag_self_nmt_signal", "ag_prof_np", "ag_prof_np_signal", "ct_value", "viral_load", "days_of_symptoms"]]
  )

parse_ers_table("https://erj.ersjournals.com/content/57/4/2003961.figures-only").to_csv("df_287.csv", sep=" ", index=False)
parse_ers_table("https://erj.ersjournals.com/content/57/5/2004430.figures-only").to_csv("df_179.csv", sep=" ", index=False)
