library(here)
library(tidyverse)

source(here::here('src/0_exp-1-fxns.R'))

file_list <- c(
    here::here('results/all_clean_p1_unswitched.csv'),
    here::here('results/all_clean_p2_unswitched.csv'),
    here::here('results/all_clean_p3_unswitched.csv'))

import_triphase <- lapply(file_list, read_csv)
all_triphase <- bind_rows(import_triphase) %>% 
    select(-sampleID) # to avoid future treatment confusion

# === import tube actual soil values + merge ===
dsoil_raw <- read_csv(here::here('data/dsoil_actual_phase1.csv')) %>% 
    rename(tube_num = tube.num)
dsoil_raw$treatment <- as.factor(dsoil_raw$treatment)

if (switch_switch == 0){
    switch_file <- 'unswitched'
    dsoil_table <- dsoil_raw
}  else {
    switch_file <- 'switched'
    dsoil_table <- switch48(dsoil_raw)
}

#### === merge IRGA data with dry soil data === ####
# and order all the data to show the tubes in order by sampleID, 
# and have the factors in the order I want to see them

table_merged <- left_join(dsoil_table, all_triphase, by=c('tube_num')) %>% 
    mutate(trt_ID = substr(sampleID, 1,3)) %>% 
    select(tube_num, sampleID, trt_ID, MC, treatment, rep,
           phase, phase_count, exp_count, everything())
table_merged$treatment <- fct_rev(table_merged$treatment)
table_merged$MC <- fct_relevel(table_merged$MC, c('BU', 'BG', 'GU', 'GG'))
table_merged <- table_merged %>% 
    arrange(MC, treatment, rep, exp_count)
table_merged$trt_ID <- fct_inorder(table_merged$trt_ID)
table_merged$sampleID <- fct_inorder(table_merged$sampleID)
table_merged <- table_merged %>% 
    arrange(sampleID, exp_count)

data_triphase_complete <- table_merged[complete.cases(table_merged),]

##### write file 
write_rds(data_triphase_complete, 
          here::here(paste0('results/2_clean_all_phases_',switch_file,'.rds')))
