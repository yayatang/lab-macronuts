library(here)
library(dplyr)
library(readr)

source(here::here('src/0_exp-1-fxns.R'))
setwd(here::here('results'))

file_list <- c(
    here::here('results/all_clean_p1_switched.csv'),
    here::here('results/all_clean_p2_switched.csv'),
    here::here('results/all_clean_p3_switched.csv'))

import_triphase <- lapply(file_list, read.csv)
all_triphase <- bind_rows(all_master)

# === import tube actual soil values + merge ===
dsoil_raw <- read.csv(here::here('data/dsoil_actual_phase1.csv'), header=T)
dsoil_table <- switch48(dsoil_raw)

# === merge IRGA data with dry soil data ===
table_merged <- merge(all_triphase, dsoil_table, by=c('sampleID'))
data_triphase <- arrange(table_merged, sampleID, exp_count)

write_csv(data_triphase, here::here('results/all_phases_clean_switched.csv'))