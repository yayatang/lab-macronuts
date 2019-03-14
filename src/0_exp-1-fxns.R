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
