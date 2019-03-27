library(ggplot2)
library(zoo)
library(dplyr)

source(here::here('src/0_exp-1-fxns.R'))
switch_switch <- 1 # 1 is switched
if (switch_switch == 0) switch_file <- 'unswitched.csv' else switch_file <- 'switched.csv'
data0_raw <- read.csv(paste0(here::here('results/all_phases_clean_'), switch_file))

# Calculate gross daily ppm values [data1_orig]------

# known standard gas CO2 ppm value
known_std <- 1997

# scaling the sample integral value to the calibration + injection number
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096

# sub-value of true ppm
data1_orig <- data0_raw
data1_orig$samp_co2_sub <- (known_std * (data1_orig$integral/data1_orig$std_vector))*(inj_constant^data1_orig$inject_num)

# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
data1_orig$samp_co2_tot <- data1_orig$samp_co2_sub * co2c_const / data1_orig$actual_dsoil
data1_orig$samp_co2_rate <- data1_orig$samp_co2_tot / data1_orig$total_time_incub
data1_orig <- mutate(data1_orig, samp_co2_perday = samp_co2_rate*24)

# Calculate ctrl vals + merge [ref_data, ctrl_data, data_with_C]-----------------------

# this is just for knowing when to cut off the control tubes
# summarize reference replicates by tube and day
ref_data <- data1_orig %>%
   filter(treatment == 'R') %>%
   select(exp_count, MC, rep, samp_co2_perday) %>%
   group_by(MC, exp_count) %>%
   summarise(MC.R_day_avg = mean(samp_co2_perday, na.rm = T))

# this is for differences between tubes
ctrl_data <- data1_orig %>%
   filter(treatment == 'C') %>%
   select(exp_count, MC, rep, samp_co2_perday) %>%
   group_by(MC, exp_count) %>%
   summarise(MC.C_day_avg = mean(samp_co2_perday, na.rm = T))

# # ===FOR DEBUGGING===
# # plot the reference average values over time
# ggplot(ctrl_data, aes(x = exp_count, y = MC.C_day_avg, color = factor(MC))) +
#    geom_point(shape = 20, size = 4) +
#    geom_line(size = 1, aes(factor = (MC))) +
#    ggtitle("Control tubes over time")

# to find which observations have NA!
# all_clean[!complete.cases(all_clean),]

# Combine data + calculate differences from Ctrl [all_diff]-----------------

# Merge C tube data (grouped) into the rest of the data
data_with_C <- merge(data1_orig, ctrl_data, by = c("MC", "exp_count"))

all_diff <- data_with_C %>%
   mutate(diff_fromC_perday = samp_co2_perday - MC.C_day_avg)

#Declare variables for unique treatments, labels, tubes, simplified data table [basic_diff]-----

# irga_days is a data frame bc it needs to merge later
irga_days <- data.frame(exp_count = unique(all_diff$exp_count)) %>% arrange(exp_count)

# unique values for each variable
trt_vec <- sort(as.character(unique(all_diff$treatment)))
MC_vec <- as.character(unique(all_diff$MC))
tube_IDs <- unique(all_diff$sampleID)
extra_ID <- unique(all_diff[c('sampleID', 'exp_count','phase', 'phase_count','MC','treatment', 'rep')])
phase_lookup <- unique(all_diff[c('exp_count', 'phase')])

# making empty grid table
max_days <- max(all_diff$exp_count)
days_all <- seq(1, max_days)
grid_vals <- expand.grid(tube_IDs, days_all)
colnames(grid_vals) <- c('sampleID', 'exp_count')

# basic_diff is the last table with non-inferred data, trimmed
basic_diff <- all_diff %>%
   select(sampleID, exp_count, diff_fromC_perday)

# Create blank grid and interpolate [daily_summ]------------------------------------
# outer join blank table with CO2 data
gapped_data <- merge(grid_vals, basic_diff, all.x=TRUE, by=c('sampleID', 'exp_count')) %>%
   mutate(trt_ID = substr(sampleID, 1,3))

# include MC, treatment, and rep columns
gapped_full <- merge(extra_ID, gapped_data, all.x=TRUE, by=c('sampleID', 'exp_count'))

filled_data <- gapped_data %>%
   group_by(trt_ID) %>%
   arrange(exp_count) %>%
   mutate(infer_perday = na.approx(diff_fromC_perday))

# include MC, treatment, and rep columns
filled_full <- merge(extra_ID, filled_data, all.x=TRUE, by=c('sampleID', 'exp_count'))

daily_summ <- gapped_full %>%
   group_by(MC, treatment, exp_count) %>%
   summarise_each(list(~mean(., na.rm=TRUE), ~se), diff_fromC_perday) %>%
   rename(mean_reps = mean, se_reps = se)


# Interpolate daily CO2 + running cumulative values [cumul_data]---------------------------

# adds in-phase cumulative value in addition to pan-experiment
cumul_phase_indiv <- filled_full %>%
   group_by(trt_ID, phase) %>%
   mutate(phase_infer_cumul = order_by(exp_count, cumsum(infer_perday)))

cumul_phase_summ <- cumul_phase_indiv %>%
   group_by(MC, treatment, exp_count) %>%
   summarise_each(list(~mean(., na.rm=TRUE), ~se), phase_infer_cumul) %>%
   rename(phase_mean_reps = mean, phase_se_reps = se) %>%
   merge(phase_lookup, by='exp_count')

# calculate cumulative CO2 respiration for each tube + error
cumul_all_indiv <- filled_full %>%
   group_by(sampleID) %>%
   arrange(exp_count) %>%
   mutate(infer_cumul = order_by(exp_count, cumsum(infer_perday)))

cumul_all_summ <- cumul_all_indiv %>%
   group_by(MC, treatment, exp_count) %>%
   summarise_each(list(~mean(., na.rm=TRUE), ~se), infer_cumul) %>%
   rename(mean_reps = mean, se_reps = se) %>%
   merge(phase_lookup, by='exp_count')

# ===use this to check values for each tube are OK
# temptube <- cumul_all_indiv %>% filter(sampleID=='GG6.6')
# ggplot(temptube, aes(x=exp_count, y=infer_cumul)) +
#    geom_point(size=4, shape=20)
# ===end use here

# Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----

# these are the only days that get real bars, since these were true data days
errorbar_daily_C <- merge(irga_days, daily_summ, all.x=TRUE)
errorbar_cumul_C <- merge(irga_days, cumul_all_summ, all.x=TRUE)