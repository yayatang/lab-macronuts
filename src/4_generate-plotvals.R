# import data that has all daily values interpolated (but no cumulative, not by trt)
library(tidyverse)
source(here::here('src/0_exp-1-fxns.R'))

# switch_switch <- 1 # 1 is switched
if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_')

data_calculated <- read_rds(paste0(here::here('results/3_calculated_'), outlier_name, switch_file, '.rds'))
data_ID <- unique(data_calculated[, c('trt_ID', 'MC', 'treatment','exp_count', 'phase','interped')])
# irga_days <- read_rds(here::here('results/irga_days.rds'))

# prepping data into two tables for graphing
# table by individual tubes
# 0) daily values ready 1) calculate cumulative vals, 2) calculate phase cumulative vals
by_tube <- data_calculated %>% 
    group_by(sampleID) %>%
    arrange(exp_count) %>%
    mutate(cumul_gross = order_by(exp_count, cumsum(infer_samp_perday)),
           cumul_diff = order_by(exp_count, cumsum(infer_diff_perday))) %>% 
    rename(tube_se = MC.C_day_se) %>% 
    group_by(sampleID, phase) %>%
    mutate(cumul_phase_gross = order_by(exp_count, cumsum(infer_samp_perday)),
           cumul_phase_diff = order_by(exp_count, cumsum(infer_diff_perday))) %>% 
    select(sampleID, trt_ID, MC, treatment, 
           exp_count, phase, phase_count, interped,
           total_time_incub, infer_samp_perday, infer_diff_perday, 
           cumul_gross, cumul_diff, cumul_phase_gross, cumul_phase_diff, tube_se)

# write_rds(by_tube, paste0(here::here('results/4_tubes_to_plot_'), switch_file,'.rds'))
write_rds(by_tube, paste0(here::here('results/4_tubes_to_plot_'), outlier_name, switch_file,'.rds'))


#------------------------------------

# treatment table
# 1) generate mean daily gross values, and se by treatment
# 2) sum up cumulative values by phase and by exp_count

c_tube_daily <- by_tube %>% 
    filter(treatment == 'C') %>% 
    rename(c_daily_gross = infer_samp_perday,
           c_cumul_gross = cumul_gross,
           c_cumul_phase = cumul_phase_gross,
           c_daily_se = tube_se) %>% 
    select(sampleID, trt_ID, MC, treatment, exp_count, phase, 
           c_daily_gross, c_cumul_gross, c_cumul_phase, c_daily_se)

c_summ_daily <- c_tube_daily %>% 
    group_by(trt_ID, exp_count) %>% 
    summarise(c_daily_mean = mean(c_daily_gross, na.rm=TRUE))

c_summ_cumul <- c_tube_daily%>% 
    group_by(trt_ID, exp_count) %>% 
    summarise(c_cumul_mean = mean(c_cumul_gross, na.rm=TRUE),
              c_cumul_se = se(c_cumul_gross))  %>%
    left_join(data_ID[,c('trt_ID', 'MC', 'exp_count')], by=c('trt_ID', 'exp_count')) %>% 
    left_join(c_summ_daily, by = c('trt_ID', 'exp_count')) %>% 
    select(trt_ID, MC, exp_count, c_daily_mean, everything())

by_trt_daily <- by_tube %>% 
    group_by(trt_ID, exp_count) %>%
    summarise(trt_gross_daily = mean(infer_samp_perday, na.rm=TRUE),
              trt_se_daily = se(infer_samp_perday))

by_trt_cumul <- by_tube %>% 
    group_by(trt_ID, exp_count) %>% 
    summarise(trt_gross_cumul = mean(cumul_gross, na.rm=TRUE),
              trt_se_cumul = se(cumul_gross),
              trt_phase_cumul = mean(cumul_phase_gross, na.rm=TRUE),
              trt_phase_se = se(cumul_phase_gross))

trt_summ <- full_join(by_trt_daily, by_trt_cumul, by=c('trt_ID', 'exp_count')) %>% 
    left_join(unique(data_ID[,c('trt_ID', 'MC', 'treatment','exp_count','interped','phase')]), by=c('trt_ID','exp_count')) %>% # merge with MC/treatment identifying data
    left_join(c_summ_cumul[,c('MC', 'exp_count', 'c_daily_mean', 'c_cumul_mean', 'c_cumul_se')], by=c('MC', 'exp_count')) %>%  # merge with summarized 
    mutate(trt_diff_daily = trt_gross_daily - c_daily_mean,
           trt_diff_cumul = trt_gross_cumul - c_cumul_mean) %>% 
    select(trt_ID, MC, exp_count, everything())

trt_summ[trt_summ$interped == TRUE,]$c_cumul_se <- NA
trt_summ[trt_summ$interped == TRUE,]$trt_se_daily <- NA
trt_summ[trt_summ$interped == TRUE,]$trt_se_cumul <- NA
trt_summ[trt_summ$interped == TRUE,]$trt_phase_se <- NA


# fix treatment factor levels
# trt_levels <- factor(c('R', 'C', '8', '7', '6', '5', '4', '3', '2', '1'))
# trt_summ$treatment <- factor(trt_summ$treatment, levels = trt_levels)

write_rds(trt_summ, paste0(here::here('results/4_trts_to_plot_'), outlier_name, switch_file,'.rds'))