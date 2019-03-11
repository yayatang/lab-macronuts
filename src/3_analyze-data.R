

library(ggplot2)
library(zoo) # for interpolation
library(here)
library(tidyr)

source(here::here('src/0_exp-1-fxns.R'))

# Import all tables and flatten all samplings into one data frame------

setwd(here::here('results/'))

# === import all merged and clean data ====
all_samp <- read.csv(here::here('results/all_phases_clean_data.csv'), stringsAsFactors=TRUE)

# === merge IRGA data with dry soil data ===
data_orig <- arrange(all_samp, sampleID, phase_count) # ordering the data in the table

# known standard gas CO2 ppm value
known_std <- 1997

# this is a statement that checks that no injection values are missing
inj_idx <- (data_orig$inject_num != 0)

# data_orig has all the information needed for adjusting to references
data_orig$inject_num[!inj_idx] <- 1

# Calculate gross daily ppm values [data_orig]------

# scaling the sample integral value to the calibration + injection number
# amount the ppm adjusts with each 5ml co2-free air injection
inj_constant <- 1.096

# sub-value of true ppm
data_orig$samp_co2_sub <- (known_std * (data_orig$integral/data_orig$std_vector))*(inj_constant^data_orig$inject_num)

# converting to CO2 micromoles, then micrograms-C, divide for ppm->microM
co2c_const <- 0.05 * 12.0011 / 22.4

# calculate _final_ CO2 ppm rate per day, in units CO2-C (microg C/g/h)
data_orig$samp_co2_tot <- data_orig$samp_co2_sub * co2c_const / data_orig$actual_dsoil
data_orig$samp_co2_rate <- data_orig$samp_co2_tot / data_orig$total_time_incub
data_orig <- mutate(data_orig, samp_co2_perday = samp_co2_rate*24)

# Calculate average reference values [ref_data, ctrl_data]-----------------------

# this is just for knowing when to cut off the control tubes
# summarize reference replicates
ref_data <- data_orig %>%
   filter(treatment=='R') %>%
   select(phase_count, MC, rep, samp_co2_perday) %>%
   group_by(MC, phase_count) %>%
   summarise(MC_ref_avg = mean(samp_co2_perday, na.rm=T))

# this is for differences between tubes
ctrl_data <- data_orig %>%
   filter(treatment=='C') %>%
   select(phase_count, MC, rep, samp_co2_perday) %>%
   group_by(MC, phase_count) %>%
   summarise(MC_ctrl_avg = mean(samp_co2_perday, na.rm=T))

# # ===FOR DEBUGGING===
# # plot the reference average values over time
# ggplot(ref_data, aes(x=phase_count, y=MC_ref_avg, color=factor(MC))) +
#   geom_point(shape=20, size=4) +
#   geom_line(size=1, aes(factor=(MC)))

# to find which observations have NA!
# diff_data[!complete.cases(diff_data),]

# Calculate DIFFERENCE/NET of treatment average from reference [data_summary, diff_data, diff_summary_C] ---------

# merge reference averages to rest of data
data_summary <- data_orig %>%
   group_by(MC, treatment, phase_count) %>%
   summarise_each(funs(mean(., na.rm = TRUE), se), samp_co2_perday)
data_summary <- rename(data_summary, mean_reps=mean, se_reps=se)

# merge control averages to rest of data
diff_summary_C <- merge(data_summary, ctrl_data, by=c('MC', 'phase_count')) %>%
   mutate(diff_perday_C = mean_reps - MC_ctrl_avg)

diff_summary_C <- diff_summary_C %>%
   mutate(trt_ID=paste0(as.character(MC), as.character(treatment)))

# Declare variables for unique treatments, labels, tubes, for a simplified data table [basic_data_C]-----

# irga_days is a data frame bc it needs to merge later
irga_days <- data.frame(phase_count = unique(diff_summary_C$phase_count)) %>% arrange(phase_count)
trt_vec <- sort(as.character(unique(diff_summary_C$treatment)))
MC_vec <- as.character(unique(diff_summary_C$MC))
tube_IDs <- unique(diff_summary_C$trt_ID)
tube_labels <- data.frame(trt_ID = as.factor(tube_IDs),
                          MC = substr(tube_IDs, 1, 2),
                          treatment = substr(tube_IDs, 3, 3)) %>%
   arrange(MC,treatment)

max_days <- max(diff_summary_C$phase_count)
days_all <- seq(1, max_days)

# reduce number of variables carried, no need for diff_summary_C anymore
basic_data_C <- select(diff_summary_C, trt_ID, phase_count, diff_perday_C, se_reps)
# basic_data_C is the last table with non-inferred data

# Create blank grid and merge with data [gapped_full_C] ------------------------------------

# matrix for all treatments across all days (to fill in with interpolated values)
grid_vals <- expand.grid(tube_IDs, days_all)
colnames(grid_vals) <- c('trt_ID', 'phase_count')

# outer join blank table with CO2 data
gapped_data_C <- merge(grid_vals, basic_data_C, all.x=TRUE, by=c('trt_ID', 'phase_count'))

# include MC, and treatment columns
gapped_full_C <- merge(gapped_data_C, tube_labels, all.x=TRUE, by='trt_ID')

# Interpolate daily CO2 + running cumulative values [cumul_summary_C]---------------------------

# each trt_ID (i.e. tube reps summary) will go through its own interpolation
filled_full_C <- gapped_full_C %>%
   group_by(trt_ID) %>%
   arrange(phase_count) %>%
   mutate(infer_perday_C = na.approx(diff_perday_C))

# calculate cumulative CO2 respiration for each tube
cumul_summary_C <- filled_full_C %>%
   group_by(trt_ID) %>%
   mutate(infer_cumul_C = order_by(phase_count, cumsum(infer_perday_C)))# %>%
# rename(mean_reps = diff_perday_C)

# # ===use this to check values are OK
# temptube <- cumul_summary_C %>% filter(trt_ID=='BGR')
# ggplot(temptube, aes(x=phase_count, y=infer_perday_C)) +
#   geom_point(size=4, shape=20)
# # ===end use here

# Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----

# these are the only days that get real bars, since these were true data days
errorbar_cumul_C <- merge(irga_days, gapped_full_C, all.x=TRUE)# %>% arrange(phase_count, treatment)
errorbar_diff_C <- merge(irga_days, diff_summary_C, all.x=TRUE)

# Loop for graphing results by MC---------

# the three things to change to graph between daily/cumulative:
# sub_data + sub_error: diff <-> cumul
# ggtitle: Daily <-> Cumulative

u_ymin <- -800 # irga 3-03:  -800/-60
u_ymax <- 2000 # irga 3-03: 2000/150

for (i in 1:length(MC_vec)){
   # (un)comment the appropriate lines to graph waht you want
   # i=4
   # sub_data <- diff_summary_C %>% filter(MC==MC_vec[i] & treatment!='C' & treatment!='R' ) %>% rename(plot_vals = infer_perday_C)
   sub_data <- cumul_summary_C %>% filter(MC==MC_vec[i] & treatment!='C' & treatment!='R' ) %>%
      rename(plot_vals = infer_cumul_C)

   print(
      ggplot(sub_data, aes(phase_count, plot_vals, color=factor(treatment), group=factor(treatment), ymin=u_ymin, ymax=u_ymax)) +
         geom_line(aes(group=treatment)) +
         geom_point(size=0.5) +
         geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
         geom_hline(yintercept=0) +
         # ggtitle(paste('Daily CO2-C by MC: ', MC_vec[i])))
         ggtitle(paste('Cumulative CO2-C by MC: ', MC_vec[i])))

   ggsave(paste0('by_MC-', MC_vec[i], '.pdf'), width=12, height=8, dpi=400)

   # used to find universal max for chart
   max_temp <- max(sub_data$diff_perday_C, na.rm=T)
   print(max_temp)
   min_temp <- min(sub_data$diff_perday_C, na.rm=T)
   print(min_temp)
}

# Loop for graphing results by treatment---------

# the three things to change to graph between daily/cumulative:
# sub_data + sub_error: diff <-> cumul
# ggtitle: Daily <-> Cumulative

# cumulative
u_ymin <- -400 # irga 3-03: cumul -300, daily -60
u_ymax <- 400 # irga 3-03: cumul 100, daily 150

# daily
# u_ymin <- -100 # irga 17: cumul -800, daily -60
# u_ymax <- 100 # irga 17: cumul 2000, daily 150

trt_vec <- trt_vec[1:9] # comment out if you want reference values, but not if you run this SECTION more than once

for (i in 1:length(trt_vec)){
   # ===to plot only ONE graph, start HERE!
   # i <- 4

   # create a subset of data for graphing
   sub_data <- cumul_summary_C %>% filter(treatment==trt_vec[i]) %>%
      # (un)comment these appropriate two lines for daily co2 vs cumulative
      rename(plot_vals = infer_cumul_C) # CUMULATIVE
      # rename(plot_vals = infer_perday_C) # DAILY

   print(
      ggplot(data=sub_data, aes(x=phase_count, y=plot_vals, color=factor(MC), group=factor(MC), ymin=u_ymin, ymax=u_ymax)) +
         geom_line(aes(group=MC)) +
         geom_point() +
         geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
         ggtitle(paste('Cumulative CO2-C by treatment: ', trt_vec[i])))
         # ggtitle(paste('Daily CO2-C by treatment: ', trt_vec[i])))
   # ===plot one graph, end here

   ggsave(paste0('by_trt-', trt_vec[i], '.pdf'), width=12, height=8, dpi=400)

   # used to find universal max for chart
   max_temp <- max(sub_data$diff_perday_C, na.rm=T)
   print(max_temp)
   min_temp <- min(sub_data$diff_perday_C, na.rm=T)
   print(min_temp)
}

