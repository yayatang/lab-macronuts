







# Interpolate daily CO2 + running cumulative values [cumul_data]---------------------------
daily_summ <- data8_gapped_interpolated %>% 
    group_by(MC, treatment, exp_count) %>%
    summarise_each(list(~mean(., na.rm=TRUE), ~se), diff_fromC_perday) %>%
    rename(mean_reps = mean, se_reps = se)

# adds in-phase cumulative value in addition to pan-experiment
cumul_phase_indiv <- data8_gapped_interpolated %>%
    group_by(trt_ID, phase) %>%
    mutate(phase_infer_cumul = order_by(exp_count, cumsum(infer_perday)))

cumul_phase_summ <- cumul_phase_indiv %>%
    group_by(MC, treatment, exp_count) %>%
    summarise_each(list(~mean(., na.rm=TRUE), ~se), phase_infer_cumul) %>%
    rename(phase_mean_reps = mean, phase_se_reps = se)

# calculate cumulative CO2 respiration for each tube + error
cumul_all_indiv <- data8_gapped_interpolated %>%
    group_by(sampleID) %>%
    arrange(exp_count) %>%
    mutate(infer_cumul = order_by(exp_count, cumsum(infer_perday)))

cumul_all_summ <- cumul_all_indiv %>%
    group_by(MC, treatment, exp_count) %>%
    summarise_each(list(~mean(., na.rm=TRUE), ~se), infer_cumul) %>%
    rename(mean_reps = mean, se_reps = se)


# Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----

# these are the only days that get real bars, since these were true data days
errorbar_daily_C <- merge(irga_days, daily_summ, all.x=TRUE)
errorbar_cumul_C <- merge(irga_days, cumul_all_summ, all.x=TRUE)
