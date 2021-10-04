# @@@ FUNCTION: switching 4 and 8, all phases--------------------------------------------

switch48 <- function(data48){
  # making a temp variable to keep the "factor" attribute of the data frame
  tempID <- as.character(data48$sampleID)
  tempID <- gsub('8.', '9.', tempID)
  tempID <- gsub('4.', '8.', tempID)
  tempID <- gsub('9.', '4.', tempID)
  data48$sampleID <- as.factor(tempID)
  
  # data48 <- dsoil_raw
  orig_levels <- levels(data48$treatment)
  data48$treatment <- as.character(data48$treatment)
  data48[which(data48$treatment=='8'),]$treatment <- '9'
  data48[which(data48$treatment=='4'),]$treatment <- '8'
  data48[which(data48$treatment=='9'),]$treatment <- '4'
  data48$treatment <- factor(data48$treatment) #, levels = orig_levels)
  
  return(data48)
}

# @@@ FUNCTION: calculate standard error ---------------------------------------------

se <- function(vals_err_calc){
  # standard error calculation excluding NAs
  val <- sd(vals_err_calc, na.rm=TRUE)/sqrt(sum(!is.na(vals_err_calc)))
  
  # this is if there are no NA values and you can assume length = number of obs
  # sqrt(var(vals_err_calc)/length(vals_err_calc))
}

# @@@ FUNCTION: t-test returning pval --------------------------

check_diff <- function(group1, group2){
  pval <- t.test(group1, group2)$p.value
  return(pval)
}

remove_outliers <- function(tubes_data) {
  library(readr)
  library(forcats)
  out_data <- read_csv(here::here('data/tube_outliers.csv')) %>% 
    rename(sampleID = tube,
           exp_count = removal_count)
  tubes_ID_levels <- levels(tubes_data$sampleID)
  out_data$sampleID <- factor(out_data$sampleID, levels = tubes_ID_levels)
  
  # first take care of individual points that are poor samples
  out_points <- out_data %>% 
    filter(exp_count != 'all') %>% 
    mutate(exp_count = as.numeric(exp_count))
  
  # second, remove the erratic tubes 
  out_tubes <- out_data %>% 
    filter(exp_count == 'all') %>% 
    select(sampleID)
  
  # make df of points not to remove
  tubes_data2 <- anti_join(tubes_data, out_points)
  deleted_points <- anti_join(tubes_data, tubes_data2)    
  # deleted_points
  
  tubes_data3 <- anti_join(tubes_data2, out_tubes)
  deleted_tubes <- anti_join(tubes_data, tubes_data3)
  
  return(tubes_data3)
}

phase_lookup <- function(day_count) {
  if(day_count <= 42){
    phase = 1
  } else if (day_count <= 172 & day_count >= 43) {
    phase = 2
  } else {phase = 3}
}

phase_count_lookup <- function(e_count) {
  if(e_count <= 42){
    p_count <- e_count
  } else if(e_count <=172 & e_count >= 43) {
    p_count <- e_count - 42
  }  else {p_count <- e_count - 172}
  p_count
}

c1_palette <- viridis(9)
names(c1_palette) <- c('1', '2', '3', '4', '5', '6', '7', '8', 'C')