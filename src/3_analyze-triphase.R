library(ggplot2)
library(zoo)
library(dplyr)
library(purrr)
library(readr)

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

# Calculate ctrl vals + merge [data2_ref, data3_ctrl, data3_with_C]-----------------------

# this is just for knowing when to cut off the control tubes
# summarize reference replicates by tube and day
data2_ref <- data1_orig %>%
    filter(treatment == 'R') %>%
    select(exp_count, MC, rep, samp_co2_perday) %>%
    group_by(MC, exp_count) %>%
    summarise_each(list(~mean(., na.rm=TRUE), ~se), samp_co2_perday) %>% 
    rename(MC.R_day_avg = mean,
           MC.R_day_se = se)

# this is for differences between tubes
data2_ctrl <- data1_orig %>%
    filter(treatment == 'C') %>%
    select(exp_count, MC, rep, samp_co2_perday) %>%
    group_by(MC, exp_count) %>%
    summarise_each(list(~mean(., na.rm=TRUE), ~se), samp_co2_perday) %>% 
    rename(MC.C_day_avg = mean,
           MC.C_day_se = se)

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
extra_ID <- unique(data4_diff[c('sampleID', 'MC','treatment', 'rep')])

phase_lookup <- function(day_count) {
    if(day_count <= 42){
        phase = 1
    } else if (day_count <= 172 & day_count >= 43) {
        phase = 2
    } else {phase = 3}
}

# making empty grid table
max_days <- max(data4_diff$exp_count)
days_all <- seq(1, max_days)
grid_empty <- expand.grid(extra_ID$sampleID, days_all) 
colnames(grid_empty) <- c('sampleID', 'exp_count')
phase_looks <- map_dbl(grid_empty$exp_count, ~phase_lookup(.x))
grid_vals <- grid_empty %>% 
    mutate(phase = phase_looks)


# basic_diff is the last table with non-inferred data, trimmed
data5_diff_trim <- data4_diff %>%
    # select(sampleID, exp_count, diff_fromC_perday)
    select(-phase, -MC, -treatment, -rep)

# Create blank grid and interpolate [daily_summ]------------------------------------
# outer join blank table with CO2 data
data6_gapped <- left_join(grid_vals, data5_diff_trim, by=c('sampleID', 'exp_count')) %>% 
    mutate(trt_ID = substr(sampleID, 1,3))

# include MC, treatment, and rep columns
data7_gapped_full <- left_join(data6_gapped, extra_ID, by='sampleID')

data8_gapped_interpolated <- data7_gapped_full %>%
    group_by(sampleID) %>%
    arrange(exp_count) %>%
    mutate(interped = is.na(diff_fromC_perday), 
           infer_diff_perday = na.approx(diff_fromC_perday, rule = 2),
           infer_samp_perday = na.approx(samp_co2_perday, rule = 2)) # to interpolate beyond max x val

calculated_data <- data8_gapped_interpolated %>% 
    select(sampleID, exp_count, samp_co2_perday, diff_fromC_perday, infer_samp_perday, infer_diff_perday, everything())

write_csv(calculated_data, here::here(paste0('results/calculated_',switch_file)))
# write_csv(irga_days, here::here('results/irga_days.csv'))