library(here)
library(dplyr)
library(readr)

source(here::here('src/0_exp-1-fxns.R'))

file_list <- c(
    here::here('results/all_clean_p1_unswitched.csv'),
    here::here('results/all_clean_p2_unswitched.csv'),
    here::here('results/all_clean_p3_unswitched.csv'))

import_triphase <- lapply(file_list, read.csv)
all_triphase <- bind_rows(import_triphase)

# === import tube actual soil values + merge ===
dsoil_raw <- read.csv(here::here('data/dsoil_actual_phase1.csv'), header=T)

if (switch_switch == 0){
    switch_file <- 'unswitched.csv'
    dsoil_table <- dsoil_raw
}  else {
    switch_file <- 'switched.csv'
    dsoil_table <- switch48(dsoil_raw)
}

# === merge IRGA data with dry soil data ===
table_merged <- merge(all_triphase, dsoil_table, by=c('sampleID'))
data_triphase <- arrange(table_merged, sampleID, exp_count)
data_triphase_complete <- data_triphase[complete.cases(data_triphase),]

write_csv(data_triphase_complete, here::here(paste0('results/all_phases_clean_',switch_file)))