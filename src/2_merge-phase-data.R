library(here)
library(dplyr)
library(readr)

source(here::here('src/0_exp-1-fxns.R'))
setwd(here::here('results'))


file_list <- c(
    'all_clean_p1_unswitched.csv', 
    'all_clean_p2_unswitched.csv',
    'all_clean_p3_unswitched.csv'
)
all_master <- lapply(file_list, read.csv)
all_samp <- bind_rows(all_master)

write_csv(all_samp, here::here('results/all_phases_clean_data.csv'))
