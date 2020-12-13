## this script calculates all the converted CO2 values, interpolated points,
## and differences from the control/reference tubesk

library(here)
library(tidyverse)
library(zoo)

source(here::here('src/0_exp-1-fxns.R'))

# switch_switch <- 1 # 1 is switched
if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
data0_raw <- read_rds(paste0(here::here('results/2_clean_all_phases_'), switch_file, '.rds'))

# remove outliers
if(outlier_bool == FALSE) data0_raw <- remove_outliers(data0_raw)

#### Calculate gross daily ppm values [data1_orig]------
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

# Calculate ctrl vals + merge [data2_ref, data3_ctrl, data3_with_C]-----------------------

# this is just for knowing when to cut off the control tubes
# average daily reference tube values
data2_ref <- data1_orig %>%
    filter(treatment == 'R') %>%
    select(exp_count, MC, rep, samp_co2_perday) %>%
    group_by(MC, exp_count) %>%
    summarise(MC.R_day_avg = mean(samp_co2_perday),
              MC.R_day_se = se(samp_co2_perday))

# this is for differences between tubes
# average daily control tube values
data2_ctrl <- data1_orig %>%
    filter(treatment == 'C') %>%
    select(exp_count, MC, rep, samp_co2_perday) %>%
    group_by(MC, exp_count) %>%
    summarise(MC.C_day_avg = mean(samp_co2_perday),
              MC.C_day_se = se(samp_co2_perday))

# Combine data + calculate differences from control-----------------

# Merge summarized control tube data with all tubes
data3_with_C <- left_join(data1_orig, data2_ctrl, by = c("MC", "exp_count"))

# this is for data that is substracted from the CONTROL tubes, not RAW
data4_diff <- data3_with_C %>%
    mutate(diff_fromC_perday = samp_co2_perday - MC.C_day_avg)

# Declare variables for unique treatments, labels, tubes, simplified data table-----

# # list of days with measurements for later error bars (possibly deprecated)
irga_days <- data.frame(exp_count = unique(data4_diff$exp_count)) %>%
    arrange(exp_count)

# unique values for each variable
extra_ID <- unique(data4_diff[c('sampleID', 'trt_ID', 'MC','treatment')])

# making empty grid table
max_days <- max(data4_diff$exp_count)
days_all <- seq(1, max_days)
grid_empty <- expand.grid(extra_ID$sampleID, days_all) 
colnames(grid_empty) <- c('sampleID', 'exp_count')

phase_looks <- map_dbl(grid_empty$exp_count, ~phase_lookup(.x))
tube_meta <- data4_diff %>% 
    select(tube_num:rep, actual_dsoil) %>% 
    unique()
# to make all 72k rows
grid_vals <- grid_empty %>% 
    mutate(phase = phase_looks) %>% 
    left_join(extra_ID) %>% 
    left_join(tube_meta)
grid_vals$phase_count <- map_dbl(grid_empty$exp_count, ~phase_count_lookup(.x))

# basic_diff is the last table with non-inferred data, trimmed
data5_diff_trim <- data4_diff %>%
    # select(sampleID, exp_count, diff_fromC_perday)
    select(-phase, -MC, -treatment, -rep)

# Create blank grid and interpolate [daily_summ]------------------------------------
# outer join blank table with CO2 data
data6_gapped <- left_join(grid_vals, data5_diff_trim)

# include MC, treatment, and rep columns
data7_gapped_full <- left_join(data6_gapped, extra_ID)

data8_gapped_interpolated <- data7_gapped_full %>%
    group_by(sampleID) %>%
    arrange(exp_count) %>%
    mutate(interped = is.na(diff_fromC_perday), 
           infer_diff_perday = na.approx(diff_fromC_perday, rule = 2),
           infer_samp_perday = na.approx(samp_co2_perday, rule = 2)) # to interpolate beyond max x val

# #### **** FIX ME **** #### 2020-05-19---i have no idea whats wrong here
# temp0 <- data7_gapped_full %>% 
#     mutate(infer = na.approx(diff_fromC_perday))
# ##########

calculated_data <- data8_gapped_interpolated %>% 
    select(sampleID, trt_ID, exp_count, samp_co2_perday, diff_fromC_perday, 
           infer_samp_perday, infer_diff_perday, everything())

outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_')

write_rds(calculated_data, here::here(paste0('results/3_calculated_', outlier_name, switch_file,'.rds')))
write_rds(irga_days, here::here('results/irga_days.rds'))