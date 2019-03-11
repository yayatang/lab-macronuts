library(here)
library(dplyr)
library(readr)

source(here::here('src/exp 1 fxns.R'))
setwd(here::here('results'))


file_list <- c(
    'all_clean_p1_unswitched.csv', 
    'all_clean_p2_unswitched.csv',
    'all_clean_p3_unswitched.csv'
)
all_master <- lapply(file_list, read.csv)
all_samp <- bind_rows(all_master)

write.csv(all_samp, file=here::here('results/all_samp.csv'))
