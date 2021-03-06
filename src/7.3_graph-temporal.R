# 7_graphs
# graphs only for end of phase or experiment protein proportion vs CO2

library(tidyverse)
library(viridis)

save_toggle <- TRUE #turn to false when trying new snippets of code

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

end_data <- read_rds(paste0(here::here('results/6_end_data_rows_'), outlier_name, switch_file, '.rds')) %>%     filter(treatment != 'R')  %>% 
    ungroup()

end_p <- read_rds(paste0(here::here('results/6_end_data_by_list_'), outlier_name, switch_file, '.rds'))

##### GRAPHS  ######
# graph faceted boxplots at the end of each phase for the cumulative gross 

cbPalette <- rev(viridis(10))

cumul_gr_plot <- ggboxplot(end_data, x = 'MC', y = 'cumul_gross',
                           color = 'treatment', 
                           title = paste0('Cumulative C by tube at end of each phase, gross C'), 
                           xlab = 'treatment',
                           ylab = 'cumul gross co2 by tube',
                           legend = 'right') +
    # guides(color = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = cbPalette) +
    facet_wrap(~ phase, nrow = 1)

cumul_gr_plot + guides(color = guide_legend(reverse = TRUE))


if(save_toggle == TRUE) ## save cumulative values of each phase, faceted
    ggsave(paste(here::here('results/', folder_date), '7', switch_file, 
                 'facet.phases_boxplot_cumul_gross.png', sep="_"), 
           width=10, height=8, dpi=600)

# ==== graphing cumulative gross + diff values phase by phase ====
# k <- 3

for (k in seq_along(end_p)){
    #===make + save plots
    end_cumul <- end_p[[k]] %>% 
        filter(treatment != 'R') # %>% 
    # filter(treatment != 6)
    
    # # look at the data
    # sample_n(end_cumul, 5)
    # str(end_cumul)
    # table(end_cumul$MC, end_cumul$treatment)
    
    #### boxplots for C mineralization of all by treatment and MC
    
    # theme(legend.position='none')
    
    cumul_gros_plot <- ggboxplot(end_cumul, x = 'MC', y = 'cumul_gross',
                                 color = 'treatment', 
                                 title = paste0('C by tube at end of phase ',k,
                                                ', diff from controls'), 
                                 xlab = 'MC',
                                 ylab = 'cumul diff from C, co2 by tube') +
        scale_color_manual(values = cbPalette) +
        theme(legend.position='none') 
    # facet_wrap( ~ MC, nrow = 1)
    print(cumul_gros_plot)
    
    if(save_toggle == TRUE) 
        ggsave(paste(here::here('results/', folder_date), '7', outlier_name, 
                     switch_file,'phase', k, 'boxplot_cumul_gross.png', sep="_"), 
               width=10, height=8, dpi=600)
    
    
    cumul_dif_plot <- ggboxplot(end_cumul, x = 'MC', y = 'cumul_diff',
                                color = 'treatment', 
                                title = paste0('C by tube at end of phase ',k,
                                               ', diff from controls'), 
                                xlab = 'direction',
                                ylab = 'cumul diff from C, co2 by tube') + 
        scale_color_manual(values = cbPalette) +
        theme(legend.position='none')
    # facet_wrap( ~ MC, nrow = 1)
    print(cumul_dif_plot)
    
    # to try and see which tubes had the most outlying values
    ggplotly(cumul_dif_plot)
    
    if(save_toggle == TRUE) 
        ggsave(paste(here::here('results/', folder_date), '7', outlier_name, 
                     switch_file, 'phase', k, 'boxplot_cumul_dif.png', sep="_"),
               width=10, height=8, dpi=600)
}

# ==== summary stats=====

# can check for treatment averages each end of phase

end_data %>% 
    filter(phase == 1) %>% 
    group_by(treatment) %>% 
    summarise(count = n(),
              mean = mean(cumul_phase_gross), 
              se = se(cumul_gross))

end_data %>% 
    filter(phase == 2) %>% 
    group_by(treatment) %>% 
    summarise(count = n(),
              mean = mean(cumul_phase_gross), 
              se = se(cumul_gross))

end_data %>% 
    filter(phase == 3) %>% 
    group_by(treatment) %>% 
    summarise(count = n(),
              mean = mean(cumul_phase_gross), 
              se = se(cumul_gross))

# ==== graphs for protein content vs cumulative CO2 at end ====
# my_formula <- y ~ poly(x, 2)
my_formula <- y ~ x

end_protein <- select(end_data, -carb_prop) 

# check protein normality
end_prot_test <- end_protein$cumul_gross
# end_prot_test <- end_protein[which(end_protein$MC=='BG'), ]$cumul_gross
ggqqplot(end_prot_test)
shapiro.test(end_prot_test) 

##### protein plots v CO2 for ALL MCs ####
# end_protein <- filter(end_protein, treatment != 'C')

library(ggpubr)
library(ggpmisc)

mc_palette <- rev(plasma(4))

final_protein <- end_protein %>% 
    filter(phase == 3)

prot_all <- ggscatter(final_protein, x = 'protein_prop', y = 'cumul_gross',
          color = 'MC',
          palette = mc_palette,
          title = 'protein proportion vs CO2: ALL',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75)) +
    # guides(color = guide_legend(reverse = TRUE)) + 
    # stat_cor(method = "pearson", label.x = 0)
    stat_smooth(aes(y=cumul_gross),
                method = "lm",
                formula = my_formula, se = T, size = 0.75) +
    stat_poly_eq(formula = my_formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., ..AIC.label.., sep = "~~~")), 
                 parse = TRUE)
prot_all
# ggplotly(prot_all)

if(save_toggle == TRUE) 
    ggsave(paste(here::here('results/',folder_date),
                 switch_file, 'end_prot_ALL.png',
                 sep="_"), width=8, height=4, dpi=600)

##### graphing by each phase, for each MC ####

graph_cumul.g <- function(which_MC, which_phase, source_data, palette_colors) {
    prot_title <- paste('protein proportion vs CO2:', which_MC, 'from start to end of phase', which_phase)
    
    # y_dims <- c((min(source_data$cumul_gross)*0.8), (max(source_data$cumul_gross)*1.2))
    source_MC <- source_data %>% 
        filter(MC == which_MC,
               phase == which_phase)
    
    g_MC <- ggscatter(source_MC, x = 'protein_prop', y='cumul_gross',
                      color = 'treatment',
                      palette = palette_colors,
                      title = prot_title,
                      legend = 'right',
                      xlab = 'protein proportion', 
                      ylab = 'cumulative gross CO2', 
                      # ylim = y_dims,
                      add.params = list(color = "black", fill = "grey", size = 0.75))+
        guides(color = guide_legend(reverse = TRUE)) + 
        stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE) +
        stat_smooth(aes(y=cumul_gross), method = "lm",formula = y ~ x , se = T, size = 0.75) +
        stat_poly_eq(aes(label = ..AIC.label..), label.x = "right", label.y = "bottom", 
                     size = 3, formula = y ~ x,  parse = TRUE)
    
    print(g_MC)
    if(save_toggle == TRUE) 
        ggsave(paste(here::here('results/',folder_date),
                     switch_file, 'end_prot_CUMUL.GROSS', which_MC, '.png',
                     sep="_"), width=6, height=4, dpi=600)
}

graph_cumul.p <- function(which_MC, which_phase, source_data, palette_colors) {
    prot_title <- paste('protein proportion vs CO2 by phase:', which_MC, 'phase', which_phase,'ONLY')
    
    y_dims <- c((min(source_data$cumul_phase_gross)*0.8), (max(source_data$cumul_phase_gross)*1.2))
    source_MC <- source_data %>% 
        filter(MC == which_MC,
               phase == which_phase)
    
    g_MC <- ggscatter(source_MC, x = 'protein_prop', y='cumul_phase_gross',
                      color = 'treatment',
                      palette = palette_colors,
                      title = prot_title,
                      legend = 'right',
                      xlab = 'protein proportion', 
                      ylab = 'cumulative phase gross CO2', 
                      # ylim = y_dims,
                      add.params = list(color = "black", fill = "grey", size = 0.75))+
        guides(color = guide_legend(reverse = TRUE)) + 
        stat_poly_eq(formula = y ~ x, 
                     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     parse = TRUE) +
        stat_smooth(aes(y=cumul_phase_gross), method = "lm",formula = y ~ x , se = T, size = 0.75) +
        stat_poly_eq(aes(label = ..AIC.label..), label.x = "right", label.y = "bottom", 
                     size = 3, formula = y ~ x,  parse = TRUE)
    
    print(g_MC)
    if(save_toggle == TRUE) 
        ggsave(paste(here::here('results/',folder_date),
                     switch_file, 'end_prot_CUMUL.PHASE',which_phase, which_MC, '.png',
                     sep="_"), width=6, height=4, dpi=600)
}

MCs <- unique(end_protein$MC)

map(MCs, function(x){graph_cumul.g(x, 2, end_protein, cbPalette)})
map(MCs, function(x){graph_cumul.g(x, 3, end_protein, cbPalette)})

# cumulative C by each phase
map(MCs, function(x){graph_cumul.p(x, 2, end_protein, cbPalette)})
map(MCs, function(x){graph_cumul.p(x, 3, end_protein, cbPalette)})

