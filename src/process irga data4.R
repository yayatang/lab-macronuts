# ======= header ============------------------------

# Analyze IRGA data
# April 2016

# ***to deal with switching the treatments:
# ***the line after a file is imported (or as soon as
# *** "sampleID" column is named) do the necessary switching

# Set working directory and clear workspace----------------------------------------

# working directory for round 3 in csv
setwd("C:/Users/Yaya/Dropbox/1 Research/2 EXPERIMENTS/1 soil microbes/2 Data/entered IRGA data/csv3/")

rm(list = ls()) # clear workspace

# Load packages ---------------------------
library(plyr)
# library(data.table)
library(reshape2)
library(ggplot2)
library(zoo) # for interpolation
library(dplyr)

source('C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/1 soil microbes/3 Analyses/exp 1 fxns.R')



# Import all tables and flatten all samplings into one data frame------

# === read all files in the directory + flatten ===
file_list <- list.files(pattern="*.csv") # file_list <- 'samp1.04.csv' # debugging

# get meta data + sample data for all files in directory
all_master <- lapply(file_list, get_info)
# flatten list of data frames into one data fram
# all_samp <- rbind_all(all_master) # apparently has deprecated
all_samp <- bind_rows(all_master)

# write to file: all standards interpolated, all samples properly labeled
write.csv(all_samp, file='C:/Users/yaya/Dropbox/1 Research/2 EXPERIMENTS/1 soil microbes/3 Analyses/all_samp.csv')

# === import tube actual soil values + merge ===
dsoil_raw <- read.csv('C:/Users/yaya/Dropbox/1 Research/2 EXPERIMENTS/1 soil microbes/2 Data/dsoil_actual_phase1.csv', header=T)
dsoil_table <- switch48(dsoil_raw)
dsoil_table <- subset(dsoil_table, sampleID != 'GGR.5')


# === merge IRGA data with dry soil data ===
table_merged <- merge(all_samp, dsoil_table, by=c('sampleID'))
data_orig <- arrange(table_merged, sampleID, incub_count) # ordering the data in the table

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
   select(incub_count, MC, rep, samp_co2_perday) %>%
   group_by(MC, incub_count) %>%
   summarise(MC_ref_avg = mean(samp_co2_perday, na.rm=T))

# this is for differences between tubes
ctrl_data <- data_orig %>%
   filter(treatment=='C') %>%
   select(incub_count, MC, rep, samp_co2_perday) %>%
   group_by(MC, incub_count) %>%
   summarise(MC_ctrl_avg = mean(samp_co2_perday, na.rm=T))

# # ===FOR DEBUGGING===
# # plot the reference average values over time
# ggplot(ref_data, aes(x=incub_count, y=MC_ref_avg, color=factor(MC))) +
#   geom_point(shape=20, size=4) +
#   geom_line(size=1, aes(factor=(MC)))

# to find which observations have NA!
# diff_data[!complete.cases(diff_data),]

# Calculate DIFFERENCE/NET of treatment average from reference [data_summary, diff_data, diff_summary_C] ---------

# merge reference averages to rest of data
data_summary <- data_orig %>%
   group_by(MC, treatment, incub_count) %>%
   summarise_each(funs(mean(., na.rm = TRUE), se), samp_co2_perday)
data_summary <- rename(data_summary, mean_reps=mean, se_reps=se)

# merge control averages to rest of data
diff_summary_C <- merge(data_summary, ctrl_data, by=c('MC', 'incub_count')) %>%
   mutate(diff_perday_C = mean_reps - MC_ctrl_avg)

diff_summary_C <- diff_summary_C %>%
   mutate(trt_ID=paste0(as.character(MC), as.character(treatment)))

# # # ===GRAPHS FOR DEBUGGING===
# # plot reference data by one single MC/soil
# ggplot(ctrl_data %>% filter(MC=='GG'), aes(x= incub_count, y=MC_ctrl_avg), color=MC) +
#    geom_point() +
#    ggtitle('Control values per soil MC') +
#    geom_line()
#
# # plotting all four MCs at once
# ggplot(ctrl_data, aes(x= incub_count, y=MC_ctrl_avg), color=MC, group=MC) +
#    geom_point() +
#    ggtitle('ALL control respiration values for each MC') +
#    geom_line(aes(group=MC, color=MC))
#
# # FOR GENERAL DEBUGGING IN THIS SECTION USE THIS
# # plot each treatment ppm by day over time
# ggplot(diff_summary_C, aes(x=incub_count, y=mean_reps, color=factor(interaction(MC, treatment)))) +
#    geom_point(shape=20, size=4) +
#    ggtitle('Raw CO2 values for each treatment') +
#    geom_errorbar(aes(ymin=mean_reps-se_reps, ymax=mean_reps+se_reps), width=0.3) +
#    geom_line(aes(factor=interaction(MC, treatment)))
# # # ===END DEBUGGING===

# Declare variables for unique treatments, labels, tubes, for a simplified data table [basic_data_C]-----

# irga_days is a data frame bc it needs to merge later
irga_days <- data.frame(incub_count = unique(diff_summary_C$incub_count)) %>% arrange(incub_count)
trt_vec <- sort(as.character(unique(diff_summary_C$treatment)))
MC_vec <- as.character(unique(diff_summary_C$MC))
tube_IDs <- unique(diff_summary_C$trt_ID)
tube_labels <- data.frame(trt_ID = as.factor(tube_IDs),
                          MC = substr(tube_IDs, 1, 2),
                          treatment = substr(tube_IDs, 3, 3)) %>%
   arrange(MC,treatment)

max_days <- max(diff_summary_C$incub_count)
days_all <- seq(1, max_days)

# reduce number of variables carried, no need for diff_summary_C anymore
basic_data_C <- select(diff_summary_C, trt_ID, incub_count, diff_perday_C, se_reps)
# basic_data_C is the last table with non-inferred data

# Create blank grid and merge with data [gapped_full_C] ------------------------------------

# matrix for all treatments across all days (to fill in with interpolated values)
grid_vals <- expand.grid(tube_IDs, days_all)
colnames(grid_vals) <- c('trt_ID', 'incub_count')

# outer join blank table with CO2 data
gapped_data_C <- merge(grid_vals, basic_data_C, all.x=TRUE, by=c('trt_ID', 'incub_count'))

# include MC, and treatment columns
gapped_full_C <- merge(gapped_data_C, tube_labels, all.x=TRUE, by='trt_ID')

# Interpolate daily CO2 + running cumulative values [cumul_summary_C]---------------------------

# each trt_ID (i.e. tube reps summary) will go through its own interpolation
filled_full_C <- gapped_full_C %>%
   group_by(trt_ID) %>%
   arrange(incub_count) %>%
   mutate(infer_perday_C = na.approx(diff_perday_C))

# calculate cumulative CO2 respiration for each tube
cumul_summary_C <- filled_full_C %>%
   group_by(trt_ID) %>%
   mutate(infer_cumul_C = order_by(incub_count, cumsum(infer_perday_C)))# %>%
# rename(mean_reps = diff_perday_C)

# # ===use this to check values are OK
# temptube <- cumul_summary_C %>% filter(trt_ID=='BGR')
# ggplot(temptube, aes(x=incub_count, y=infer_perday_C)) +
#   geom_point(size=4, shape=20)
# # ===end use here

# Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----

# these are the only days that get real bars, since these were true data days
errorbar_cumul_C <- merge(irga_days, gapped_full_C, all.x=TRUE)# %>% arrange(incub_count, treatment)
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
      ggplot(sub_data, aes(incub_count, plot_vals, color=factor(treatment), group=factor(treatment), ymin=u_ymin, ymax=u_ymax)) +
         geom_line(aes(group=treatment)) +
         geom_point(size=0.5) +
         geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
         geom_hline(yintercept=0) +
         # ggtitle(paste('Daily CO2-C by MC: ', MC_vec[i])))
         ggtitle(paste('Cumulative CO2-C by MC: ', MC_vec[i])))

   ggsave(paste0('by_MC-', MC_vec[i], '.pdf'), width=6, height=4, dpi=400)

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
      ggplot(data=sub_data, aes(x=incub_count, y=plot_vals, color=factor(MC), group=factor(MC), ymin=u_ymin, ymax=u_ymax)) +
         geom_line(aes(group=MC)) +
         geom_point() +
         geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
         ggtitle(paste('Cumulative CO2-C by treatment: ', trt_vec[i])))
         # ggtitle(paste('Daily CO2-C by treatment: ', trt_vec[i])))
   # ===plot one graph, end here

   ggsave(paste0('by_trt-', trt_vec[i], '.pdf'), width=6, height=4, dpi=400)

   # used to find universal max for chart
   max_temp <- max(sub_data$diff_perday_C, na.rm=T)
   print(max_temp)
   min_temp <- min(sub_data$diff_perday_C, na.rm=T)
   print(min_temp)
}

# **(commented out)testing whether samplings differ from the reference --------------------------

# this should be made into a function... figure out which argument it needs

# for each MC, through all treatments
# pval_vec <- data.frame(row.names=mc_vec)

# trt_vec <- as.character(unique(diff_data$treatment))
# trt_vec <- sort(trt_vec)
#
# pval_mat <- as.data.frame(matrix(nrow=length(mc_vec), ncol=length(trt_vec)))
# names(pval_mat) <- trt_vec
#
# day_incub <- 40
# for (i in 1:length(mc_vec)){
#    ref <- diff_data %>% filter(MC==mc_vec[[i]], treatment=='R', incub_count==day_incub) %>% select(diff_co2_perday)
#    for (j in 1:length(trt_vec)){
#       group <- diff_data %>% filter(MC==mc_vec[[i]], treatment==trt_vec[[j]], incub_count==day_incub) %>% select(diff_co2_perday)
#
#       pval_mat[i,j] <- check_diff(ref, group)
#    }
# }
# pval_melt <- melt(pval_mat)
# pval_melt[pval_melt$value<=0.05,]