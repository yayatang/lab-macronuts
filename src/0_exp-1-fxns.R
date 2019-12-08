# @@@ FUNCTION: switching 4 and 8, all phases--------------------------------------------

switch48 <- function(data48){
  # making a temp variable to keep the "factor" attribute of the data frame
  tempID <- as.character(data48$sampleID)
  tempID <- gsub('8.', '9.', tempID)
  tempID <- gsub('4.', '8.', tempID)
  tempID <- gsub('9.', '4.', tempID)
  data48$sampleID <- as.factor(tempID)
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
  out_data <- read_csv(here::here('data/tube_outliers.csv'))
  colnames(out_data) <- c('sampleID', 'exp_count')
  
  # first take care of individual points that are poor samples
  out_points <- out_data %>% 
    filter(exp_count != 'all') %>% 
    mutate(exp_count = as.numeric(exp_count))
  
  # second, remove the erratic tubes [***BUT DOUBLE CHECK THESE**]
  out_tubes <- out_data %>% 
    filter(exp_count == 'all') %>% 
    select(sampleID)
  
  # make df of points not to remove
  tubes_data2 <- anti_join(tubes_data, out_points)
  deleted_tubes <- anti_join(tubes_data, tubes_data2)    
  deleted_tubes
  
  tubes_data3 <- anti_join(tubes_data2, out_tubes)
}