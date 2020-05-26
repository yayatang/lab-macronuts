# 7 graphs
# needs to import data
library(tidyverse)
library(ggpubr)
switch_file <- 'switched'
end_data <- read_rds(paste0(here::here('results/end_data_'), switch_file, '.rds')) %>% 
    rename(loc = site,
           graze = land_use) #%>% 
    # filter(treatment != 'R')
save_toggle <- FALSE

##### linear models #####
# from the gross respiration data

end_data$loc2 <- if_else(end_data$loc == 'Beit Guvrin', 0, 1)
end_data$graze2 <- if_else(end_data$graze == 'ungrazed', 0, 1)

model_all <- lm(cumul_gross ~ loc + graze + protein_prop, data = end_data)
lin_summ <- summary(model_all)
lin_summ
lin_coeff <- model_all$coefficients
lin_coeff

contrasts(end_data$loc)
contrasts(end_data$graze)

(prelim_plot <- ggplot(end_data, aes(x = protein_prop, y = cumul_gross)) +
        geom_point() +
        geom_smooth(method = "lm"))
