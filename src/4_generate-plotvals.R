# import data that has all daily values interpolated (but no cumulative, not by trt)

library(tidyverse)

source(here::here('src/0_exp-1-fxns.R'))

data_calculated <- read.csv(here::here('results/calculated_unswitched.csv'))
data_ID <- unique(data_calculated[, c('MC', 'treatment','exp_count')])
# irga_days <- read.csv(here::here('results/irga_days.csv'))

# prepping data into two tables for graphing
# tube table
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
    select(sampleID, trt_ID, MC, treatment, exp_count, phase, phase_count, total_time_incub, infer_samp_perday, 
           infer_diff_perday, cumul_gross, cumul_diff, cumul_phase_gross, cumul_phase_diff, tube_se)

# write_csv(by_tube, here::here('results/tubes_to_plot.csv'))

#------------------------------------

# treatment table
# 1) generate mean daily gross values, and se by treatment
# 2) add up cumulative values by phase and by exp_count

cumul_c <- by_tube %>% 
    filter(treatment == 'C') %>% 
    rename(daily_C_gross = infer_samp_perday,
           cumul_C_gross = cumul_gross,
           cumul_C_phase = cumul_phase_gross) %>% 
    select(sampleID, trt_ID, MC, treatment, exp_count, phase, daily_C_gross, cumul_C_gross, cumul_C_phase)


by_trt_gross <- by_tube %>% 
    group_by(trt_ID, exp_count) %>% 
    summarise_each(list(~mean(., na.rm=TRUE), ~se), infer_samp_perday) %>% 
    rename(trt_gross_daily = mean,
           trt_se_daily = se) %>% 
    # left_join(cumul_c, by=c(""))
    mutate(trt_cumul_gross = order_by(exp_count, cumsum(trt_gross_daily)))
    
se_by_trt <- unique(by_trt_gross[, c('trt_ID', 'exp_count','trt_se_daily')])

by_trt_diff <- by_tube %>% 
    group_by(trt_ID, exp_count) %>% 
    summarise(trt_daily_diff = mean(infer_diff_perday))

by_trt_diff <- left_join(by_trt_diff, se_by_trt, by=c('trt_ID', 'exp_count'))









# 
# 
# by_trt <- by_tube %>% 
#     group_by(MC, treatment, exp_count) %>%
#     summarize_at(vars(infer_samp_perday, infer_diff_perday), mean, na.rm=TRUE) %>% 
#     rename(mean_daily_samp = infer_samp_perday,
#            mean_daily_diff = infer_diff_perday)
# by_trt <- by_trt %>% 
#     group_by(MC, treatment, exp_count) %>% 
#     mutate(mean_reps = mean)
# 
# 
# 
# 
# # Interpolate daily CO2 + running cumulative values [cumul_data]---------------------------
# daily_summ <- data8_gapped_interpolated %>% 
#     group_by(MC, treatment, exp_count) %>%
#     summarise_each(list(~mean(., na.rm=TRUE), ~se), diff_fromC_perday) %>%
#     rename(mean_reps = mean, se_reps = se)
# 
# # calculate cumulative CO2 respiration for each tube + error
# 
# cumul_all_summ <- cumul_all_indiv %>%
#     group_by(MC, treatment, exp_count) %>%
#     summarise_each(list(~mean(., na.rm=TRUE), ~se), infer_cumul) %>%
#     rename(mean_reps = mean, se_reps = se)
# 
# 
# # Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----
# 
# # these are the only days that get real bars, since these were true data days
# errorbar_daily_C <- merge(irga_days, daily_summ, all.x=TRUE)
# errorbar_cumul_C <- merge(irga_days, cumul_all_summ, all.x=TRUE)
