# x1 graphs 
# for demonstrating more of the data from experiment 1

#plots to make:
# 1. end of phase 1: C:N vs respiration (both gross + netto diff)
# 2. 

library(tidyverse)
library(here)
library(ggpubr)

trt_graphs0 <- read_rds(here::here('results/4_trts_to_plot_switched.rds')) %>% 
    filter(treatment != 'R')
tube_graphs0 <- read_rds(here::here('results/4_tubes_to_plot_switched.rds')) %>% 
    filter(treatment != 'R')


# order the levels so they make sense on the plot
trt_levels <- c('R', 'C', '8', '7', '6', '5', '4', '3', '2', '1')
levels(trt_graphs0) <- trt_levels
levels(tube_graphs0) <- trt_levels

#=============
# is this even valid for repeated measures? or
ggqqplot(tube_graphs0$infer_samp_perday)
ggqqplot(trt_graphs0$trt_gross_daily)
#=============

# find the max value per phase... maybe make into fxn? ****
max_p1 <- max(filter(tube_graphs0, phase == 1)$exp_count)
max_p2 <- max(filter(tube_graphs0, phase == 2)$exp_count)
max_p3 <- max(filter(tube_graphs0, phase == 3)$exp_count)

end_p1 <- filter(tube_graphs0, exp_count == max_p1)
end_p2 <- filter(tube_graphs0, exp_count == max_p2)
end_p3 <- filter(tube_graphs0, exp_count == max_p3)
end_p <- list(end_p1, end_p2, end_p3)

CN_raw <- read_csv(here::here('data/elemental analysis/exp1_CN_data.csv'))
colnames(CN_raw) <- c('sample_name', 'pH', 'sample_mass',
                      'C_percent', 'N_percent', 
                      'N_total_mg', 'C_total_mg')
# generate C:N ratios
data_CN <- CN_raw %>% 
    select(sample_name, pH, sample_mass, C_percent, N_percent) %>% 
    mutate(CN_ratio = C_percent / N_percent,
           C_total_mg = C_percent /100 * sample_mass,
           N_total_mg = N_percent /100 * sample_mass)
# rbind(c('C', NA, 0, 0, 0, 0, 0, 0)) # row for the empty reference


#split up the table into three types of samples: litter, soil, treatments
CN_lit <- data_CN[which(data_CN$sample_name == 'litter_1g'),]
CN_soils <- data_CN[which(data_CN$sample_name %in% c('BU', 'BG', 'GU', 'GG')),]
CN_trt <- data_CN[6:13,] %>% 
    rename(treatment = sample_name,
           trt_CN = CN_ratio,
           trt_C_total_mg = C_total_mg,
           trt_N_total_mg = N_total_mg) %>% 
    select(treatment, trt_CN, trt_C_total_mg, trt_N_total_mg)



#=======
# graphing data from phase 1
phase1_end <- left_join(end_p1, CN_trt)

ggplot(filter(phase1_end, MC == 'BU'), 
       aes(trt_CN, 
           # trt_gross_cumul, 
           cumul_gross,
           color = treatment, 
           group = treatment)) +
    geom_point(size=2) +
    labs(x="C:N ratio of treatment added",
         title = "CN amendment vs phase 1 total mineralized C") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))


