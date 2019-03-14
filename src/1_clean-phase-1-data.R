library(dplyr)
library(zoo)
library(here)

source(here::here('src/0_exp-1-fxns.R'))
# datasheet location for phase 1 csvs
setwd(here::here('data/entered IRGA data/csv/'))

# @@@ FUNCTION: to read and return data, interpolates standards, phase 1 specific
get_info <- function(fileloc) {
    # fileloc <- 'IRGA 1-13.csv' # for DEBUGGING
    
    # === first import META DATA ===
    
    # extracts first line as meta data for one day's values
    meta_raw <- scan(fileloc, nlines = 1, what = character(), sep = ',')
    # meta_raw[[3]] # this is the incubation day/count
    
    # === cleans and arranges meta-data into a table ===
    meta_raw <- meta_raw[meta_raw != '']
    meta_raw <- matrix(meta_raw, nrow = 2, ncol = 5)
    colnames(meta_raw) <- c(
        'incub_count',
        'start_day',
        'day_flush', 
        'day_msre',
        'std_ppm')
    meta_raw <- meta_raw[-1, ]
    meta <- as.data.frame(t(meta_raw))
    
    # === ensures all days and numbers entered are in a standard format ===
    meta$incub_count <- as.numeric(as.character(meta$incub_count))
    meta$start_day <- as.POSIXlt(as.character(meta$start_day), '%d-%b-%y', tz = Sys.timezone())
    meta$day_flush <- as.POSIXlt(as.character(meta$day_flush), '%d-%b-%y', tz = Sys.timezone())
    meta$day_msre <- as.POSIXlt(as.character(meta$day_msre), '%d-%b-%y', tz = Sys.timezone())
    meta$std_ppm <- as.numeric(as.character(meta$std_ppm))
    
    # === checks the incubation day value ===
    day1 <- strptime(meta$start_day, format='%Y-%m-%d')
    # use day measured and not flushed bc some sat for 48 hours
    day2 <- strptime(meta$day_msre, format='%Y-%m-%d')
    meta$incub_count_check <- difftime(day2, day1, units="days")+ 1
    # meta # for DEBUG
    
    # === imports sample integral data ===
    samp <- read.csv(fileloc, skip=1, header=T)
    samp <- samp[,1:10] # delete this for round 2, it's for removing the moistening cols
    colnames(samp) <- c(
        'tube_num', 
        'rep', 
        'rack', 
        'position', 
        'sampleID', 
        'time_flush', 
        'time_msre',
        'integral',
        'inject_num',
        'std_integral') # include this before std_integral in round 2: 'std_time_int'
    samp <- switch48(samp) # unswitch because it's not necc wrong!
    samp <- samp[colSums(!is.na(samp)) > 0]
    
    # === many lines for converting dates into the correct format ===
    samp$day_flush <- as.character(meta$day_flush)
    samp$day_msre <- as.character(meta$day_msre)
    samp$time_msre <- as.character(samp$time_msre)
    samp$time_flush <- as.character(samp$time_flush)
    samp$incub_count <- meta$incub_count_check
    
    # these variables are to fix some of the late night flushing dates
    # ROUND 2 will need something similar for the first flushing (split over two days)
    tomo_idx_flush <- (as.POSIXlt(samp$time_flush, format='%H:%M')$hour < 6)
    tomo_idx_msre <- (as.POSIXlt(samp$time_msre, format='%H:%M')$hour < 6)
    
    # change day to the next day bc it was after 12am
    samp$day_flush[tomo_idx_flush==T] <- as.character(as.Date(meta$day_flush)+1)
    samp$day_msre[tomo_idx_msre==T] <- as.character(as.Date(meta$day_msre)+1)
    
    # === make everything into a date = day + time ===
    samp$date_flush <- strptime(paste(samp$day_flush, samp$time_flush), format='%Y-%m-%d %H:%M')
    samp$date_msre <- strptime(paste(samp$day_msre, samp$time_msre), format='%Y-%m-%d %H:%M')
    samp$date_flush <- as.character(samp$date_flush)
    samp$date_msre <- as.character(samp$date_msre)
    
    # === vectorize calibration values/standards ===
    # ROUND 2 will make a standard table separate from the rest of the data + merge times
    count <- 2
    count_lim <- nrow(samp)-1
    
    fxn_samp <- data.frame(data_time = numeric(length=nrow(samp)),
                           std_data = numeric(length=nrow(samp)),
                           # std_time = numeric(length=nrow(samp)), # uncomment for ROUND 2
                           std_vector = numeric(length=nrow(samp)))
    fxn_samp$data_time <- samp$time_msre
    fxn_samp$std_data <- samp$std_integral
    
    # while loop to average all known triplets of standards, and NA the rest
    while (count <= count_lim) {
        if (!is.na(fxn_samp$std_data[count-1])  && !is.na(fxn_samp$std_data[count+1])){
            fxn_samp$std_vector[(count-1):(count+1)] <- mean(fxn_samp$std_data[(count-1):(count+1)])
            # count <- count+2 # not necessary to speed up
        } else fxn_samp$std_vector[count] <- NA
        
        count <- count+1
    }
    
    fxn_samp$std_vector <- na.approx(fxn_samp$std_vector)
    samp$std_vector <- fxn_samp$std_vector
    
    # === calculate total time in hours the tubes were incubating ===
    samp$total_time_incub <- as.numeric(difftime(samp$date_msre, samp$date_flush, units="hours"))
    # as.numeric keeps it from being a difftime object and dropping the unncessary info of units=hours
    
    # === return relevant variables ===
    master <- select(samp, sampleID, incub_count, total_time_incub, integral, inject_num, std_vector)
}

# Import all tables and flatten all samplings into one data frame------
# === read all files in the directory + flatten ===
file_list <- list.files(pattern="*.csv") # file_list <- 'IRGA 1-04.csv' # debugging

# get meta data + sample data for all files in directory
all_master <- lapply(file_list, get_info)
# flatten list of data frames into one data frame
all_samp <- bind_rows(all_master)

# this is a statement that checks that no injection values are missing
inj_idx <- (all_samp$inject_num != 0)

# data_orig has all the information needed for adjusting to references
all_samp$inject_num[!inj_idx] <- 1

# label data for inclusion in triphase experiment data frame
data_p1 <- all_samp %>%
    dplyr::rename(phase_count = incub_count) %>%
    mutate(phase = 1, 
           exp_count = phase_count) %>% 
    arrange(sampleID, exp_count)

write.csv(data_p1, file = here::here('results/all_clean_p1_switched.csv'), row.names=FALSE)
