# This script runs the stats on the final data point (the cumulative CO2 respired)
# and 

# 6.2 end of phase stats
# switch_file <- 'switched'
save_toggle <- FALSE

library(tidyverse)
library(ggpubr)

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

phase_end_data <- read_rds(paste0(here::here('results/6_end_data_rows_'), outlier_name, switch_file, '.rds')) %>% 
    filter(treatment != 'R')

##### linear models #####
## these are NORMAL linear regression models. need to run with RANDOM EFFECTS

# from the gross respiration data

formula_c.gross <- 'cumul_gross ~ location + grazing + protein_prop'
formula_c.diff <- 'cumul_diff ~ location + grazing + protein_prop'
formula_p.gross <- 'cumul_phase_gross ~ location + grazing + protein_prop'
formula_p.diff <- 'cumul_phase_diff ~ location + grazing + protein_prop'

# choosing one of the possible formulas to see statistical summary
my_formula <- formula_p.diff

model_p1 <- lm(my_formula, filter(phase_end_data, phase==1))
summary(model_p1)

model_p2 <- lm(my_formula, filter(phase_end_data, phase==2))
summary(model_p2)

model_p3 <- lm(my_formula, filter(phase_end_data, phase==3))
summary(model_p3)


###########

# function to filter out data for each phase
phase_model <- function(my_phase, model_data, model_form) {
    my_data <- model_data %>% filter(phase == my_phase)
    my_model <- lm(model_form, my_data)
    
    return(my_model)
}

phases <- unique(phase_end_data$phase)
basic_formula <- as.formula('cumul_gross ~ location + grazing + protein_prop')

contrasts(phase_end_data$location)
contrasts(phase_end_data$grazing)


############
# ## ERROR---does the following even make sense?
# # phases <- 1
# # map(phases, phase_model(~., phase_end_data, basic_formula))
# 
# p1_model <- phase_model(1, phase_end_data, basic_formula)
# plot(p1_model)
# p2_model <- phase_model(2, phase_end_data, basic_formula)
# plot(p2_model)
# p3_model <- phase_model(3, phase_end_data, basic_formula)
# plot(p3_model)
# 
# # across all phases
# 
# model_all <- lm(cumul_gross ~ location + grazing + protein_prop, data = phase_end_data)
# plot(model_all)
# lin_summ <- summary(model_all)
# lin_summ
# lin_coeff <- model_all$coefficients
# lin_coeff
# 
# fixed_model <- lm(cumul_gross ~ protein_prop + grazing + location, data = phase_end_data)
# plot(fixed_model)
# 
######################
# # using the lme4 package to include random effects
# library('lme4')
# lme.fit <-lmer(cumul_gross ~ 1 + (1 | pop) )
