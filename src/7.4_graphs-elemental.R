# 7.4 C and N graphs
# 
library(tidyverse)
library(ggpubr)
library(lme4)
library(viridis)
library(gridExtra) # for grid.arrange fxn, can remove if not using
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")


save_toggle <- TRUE

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

element_data <- read_rds(paste0(here::here('results/6_CN_data_'), 
                                outlier_name, switch_file, '.rds'))

element_data$sample_name <- fct_relevel(element_data$sample_name, 
                                        'BU', 'BG', 'GU', 'GG', 
                                        # 'C', '8', '7', '6', '5', '4', '3', '2', '1',
                                        '1', '2', '3', '4', '5', '6', '7', '8',
                                        'litter_1g')
cbPalette <- c(viridis(8, option = "B"), '#a52c60')


## plot of total N total mg
## consider scaling this?
element_data %>% 
    arrange(sample_name) %>% 
    filter(!sample_name %in% c('BU', 'BG', 'GU', 'GG', 'C')) %>% 
    ggplot(aes(x = sample_name, y = N_total_mg,)) + 
    geom_bar(stat = 'identity',
             aes(sample_name,
                 fill = sample_name)) + 
    scale_fill_viridis(name = 'Sample', discrete=TRUE, option = 'magma',
                       labels = c('1', '2', '3', '4', '5', '6', '7', '8',
                                  'Litter')) +
    labs(x = "Amendment",
         y = "Total N (mg)",
         title = "Total N of each soil microcosm input") +
    scale_x_discrete(labels=c('1', '2', '3', '4', '5', '6', '7', '8', 'Litter')) + 
    theme_dissert() + 
    theme(legend.position = 'none')

### Figure: total C of each soil microcosm ------
element_data %>% 
    arrange(sample_name) %>% 
    filter(!sample_name %in% c('BU', 'BG', 'GU', 'GG', 'litter_1g', 'C')) %>% 
    
    ggplot(aes(x = sample_name, y = C_total_mg)) + 
    geom_bar(stat = 'identity',
             aes(sample_name,
                 fill = sample_name)) + 
    scale_fill_viridis(name = 'Sample', discrete=TRUE, option = 'magma',
                       labels = c('1', '2', '3', '4', '5', '6', '7', '8',
                                  'Litter')) +
    labs(x = "Amendment",
         y = "Total C (mg)",
         title = "Total C of each soil microcosm input") +
    theme_dissert() + 
    theme(legend.position = 'none')


## Figure: average C and N source within each tube

element_long_data <- element_data %>% 
    filter(sample_source != 'litter_1g') %>% 
    pivot_longer(c('C_total_mg', 'N_total_mg'), names_to = 'element', values_to='m_element_mg') %>% 
    mutate(element = substr(element, 1,1)) 

element_amend_avg <- element_long_data %>% 
    filter(sample_source == 'amendment') %>% 
    group_by(element) %>% 
    summarize(mean_amend_mg = mean(m_element_mg)) %>% 
    mutate(element_source = paste(element, 'amendment'))

element_soil_data <- element_long_data %>% 
    filter(sample_source == 'soil') %>% 
    # mutate(element_source = paste(element, 'soil')) %>% 
    full_join(element_amend_avg, by='element')
# ggplot()
# write_csv(element_soil_data, here::here('results/TEMP_soil.csv'))
# write_csv(element_amend_avg, here::here('results/TEMP_soil_amend.csv'))

temp_soil <- read_csv(here::here('TEMP_soil.csv'))

temp_soil %>% 
    arrange(desc(m_element_mg)) %>% 
    mutate(element_source = as_factor(paste(element, sample_source)),
           element_source = fct_relevel(element_source, c('N amendment', 'C amendment',  'N soil', 'C soil'))) %>% 
    ggplot(aes(sample_name,
               m_element_mg,
               fill = element_source)) +
    geom_col(position = 'stack') + 
    scale_fill_viridis(name = 'Element source', discrete=TRUE, option = 'plasma', direction =-1) +
    labs(x = "Soil source",
         y = "Mass of element (mg)",
         title = '') +
    scale_x_discrete(labels = c('Beit Guvrin\ngrazed', 
                                'Beit Guvrin\nungrazed', 
                                'Golan Heights\ngrazed', 
                                'Golan Heights\nungrazed')) + 
    theme_dissert()


