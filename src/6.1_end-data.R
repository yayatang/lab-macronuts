# This script is for generating plots for end-of-experiment data
# includes all tube data, aka "by_tube"
# Question: How do each of my explanatory variables affect the overall fxn / 
# rates of respiration?


#====== Code starts here

# when running this script alone:
# switch_switch <- 1 # switch_switch = 1 means switched
library(lubridate)
# folder_date <- gsub("-", ".",today())

#### library, packages ####
list.of.packages <- c('here', 'tidyverse', 'ggpubr', 'ggpmisc', 
                      'plotly', 'viridis') 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source(here::here('src/0_exp-1-fxns.R'))

#### import data ####
if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

imported_data <- read_rds(paste0(here::here('results/4_tubes_to_plot_'), 
                                 outlier_name, switch_file,'.rds'))
levels(imported_data$treatment)

# import amendment protein proportions
amend_trts <- read_csv(here::here('data/protein_treatments.csv')) %>% 
    mutate(treatment = fct_rev(treatment))
# levels(amend_trts$treatment)

#### CN data processing ####
CN_raw <- read_csv(here::here('data/elemental analysis/exp1_CN_data.csv'))
colnames(CN_raw) <- c('sample_name', 'sample_source', 'pH', 'sample_mass',
                      'C_percent', 'N_percent', 
                      'N_total_mg', 'C_total_mg')

# is this necessary at this point?
# generate C:N ratios
data_CN <- CN_raw %>%
    select(sample_name, sample_source, pH, sample_mass, C_percent, N_percent) %>%
    mutate(CN_ratio = C_percent / N_percent,
           C_total_mg = C_percent /100 * sample_mass,
           N_total_mg = N_percent /100 * sample_mass)
data_CN[which(data_CN$pH == 'na'),]$pH <- NA


#split up the table into three types of samples: litter, soil, treatments
site_use <- tibble(MC = as_factor(c('BU', 'BG', 'GU', 'GG')),
                   location = as_factor(c(rep('Beit Guvrin', 2), 
                                          rep('Golan Heights', 2))),
                   grazing = as_factor(rep(c('ungrazed', 'grazed'), 2)))

CN_lit <- data_CN[which(data_CN$sample_name == 'litter_1g'),]
CN_soils <- data_CN[which(data_CN$sample_name %in% c('BU', 'BG', 'GU', 'GG')),] %>% 
    rename(MC = sample_name) %>%
    mutate(MC = as_factor(MC)) %>% 
    left_join(site_use)
CN_trt <- data_CN[6:14,] %>% 
    rename(treatment = sample_name,
           trt_CN = CN_ratio,
           trt_C_total_mg = C_total_mg,
           protein_prop = N_total_mg)

CN_soils_long <- CN_soils %>% 
    select(-pH, -C_percent, -N_percent, -CN_ratio) %>% 
    gather(key = "element",
           value = "element_mg", C_total_mg, N_total_mg) %>% 
    mutate(element = as_factor(substr(element, 1, 1)),
           source = 'soil',
           percent_element = element_mg / sample_mass * 100)

#### merge imported data with elemental analysis ####
all_data <- imported_data %>% 
    left_join(amend_trts) %>% 
    left_join(CN_soils)

#### determine end of phase variables ####
# filter data to only include final cumulative differences
max_p1 <- max(filter(all_data, phase == 1)$exp_count)
max_p2 <- max(filter(all_data, phase == 2)$exp_count)
max_p3 <- max(filter(all_data, phase == 3)$exp_count)

end_p1 <- filter(all_data, exp_count == max_p1)
end_p2 <- filter(all_data, exp_count == max_p2)
end_p3 <- filter(all_data, exp_count == max_p3)

phase_ends_list <- list(end_p1, end_p2, end_p3)
phase_ends_rows <- rbind(end_p1, end_p2, end_p3)

#### data export for respiration analysis ####
# data from the ends of each phase, in one table
write_rds(phase_ends_rows, paste0(here::here('results/6_end_data_rows_'),  outlier_name, switch_file, '.rds'))    
# data from the ends of each phase, in list format
write_rds(phase_ends_list, paste0(here::here('results/6_end_data_by_list_'), outlier_name, switch_file, '.rds'))

#### data export for C and N properties visualization/analysis ####
write_rds(data_CN, paste0(here::here('results/6_CN_data_'), outlier_name, switch_file, '.rds'))
