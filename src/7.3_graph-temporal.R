# 7_graphs
# graphs only for end of phase or experiment protein proportion vs CO2

library(tidyverse)
library(viridis)
library(ggpubr)
library(plotly)
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")

save_toggle <- TRUE  #turn to false when trying new snippets of code

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

end_data <- read_rds(paste0(here::here('results/6_end_data_rows_'), outlier_name, switch_file, '.rds')) %>%     filter(treatment != 'R')  %>% 
    ungroup()

end_p <- read_rds(paste0(here::here('results/6_end_data_by_list_'), outlier_name, switch_file, '.rds'))

##### Figure: total cumulative C through each phase  ######
# graph faceted boxplots at the end of each phase for the cumulative gross 

cbPalette <- rev(viridis(10))

(cumul_gr_plot <- end_data %>% 
        mutate(treatment = fct_rev(treatment)) %>% 
        group_by(MC, treatment, phase) %>% 
        summarize(MC = first(MC),
                  treatment = first(treatment),
                  mean_cumul_gross = mean(cumul_gross),
                  se_cumul_gross = se(cumul_gross)) %>% 
        ggplot(aes(MC, 
                   mean_cumul_gross,
                   fill = treatment)) +
        geom_col(position = position_dodge()) + 
        geom_errorbar(aes(ymin = mean_cumul_gross - se_cumul_gross,
                          ymax = mean_cumul_gross + se_cumul_gross), 
                      width=0.3, 
                      position = position_dodge(width = 0.9)) +
        scale_fill_viridis(name = '', discrete=TRUE, ) +
        labs(x = 'Soil microbial community source',
             y = 'C mineralized (g)',
             title = 'Cumulative C mineralized through each experimental phase') +
        # scale_x_discrete(labels = c('Beit Guvrin\ngrazed', 
        #                             'Beit Guvrin\nungrazed', 
        #                             'Golan Heights\ngrazed', 
        #                             'Golan Heights\nungrazed')) + 
        facet_wrap(~ phase, nrow = 1) + 
        theme_yaya() +
        theme(plot.title = element_blank(),
              axis.title.x = element_blank())
)

my_ggsave(here::here('results/all_dynamics_faceted.png'), 7, 6)

# if(save_toggle == TRUE) ## save cumulative values of each phase, faceted
#     ggsave(paste(here::here('results/', folder_date), '7', switch_file, 
#                  'facet.phases_boxplot_cumul_gross.png', sep="_"), 
#            width=10, height=5, dpi=600)

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

get_CV <- function(list_data) {
    sd(list_data)/mean(list_data)*100
}

cv_table <- aggregate(cumul_gross ~ MC + treatment,
                      data = filter(end_data, phase ==3),
                      FUN = get_CV) %>% 
    pivot_wider(names_from = treatment, values_from = cumul_gross)
write_csv(cv_table, here::here('results/table_coefficient_of_variance.csv'))


# ==== graphs for protein content vs cumulative CO2 at end ====
# my_formula <- y ~ poly(x, 2)
my_formula <- y ~ x

end_protein <- select(end_data, -carb_prop) %>% 
    filter(treatment !='R')

# check protein normality
end_prot_test <- end_protein$cumul_gross
# end_prot_test <- end_protein[which(end_protein$MC=='BG'), ]$cumul_gross
ggqqplot(end_prot_test)
# shapiro.test(end_prot_test) 

##### protein plots v CO2 for ALL MCs ####
# end_protein <- filter(end_protein, treatment != 'C')

library(ggpubr)
library(ggpmisc)

mc_palette <- rev(plasma(4))

final_protein <- end_protein %>% 
    filter(phase == 3)

ggplot(final_protein, 
       aes(protein_prop, 
           cumul_gross,
           color = MC))+
    labs(x = 'Protein proportion', 
         y = 'Cumulative C mineralized',
         title = 'Protein proportion vs CO2: ALL') +
    # legend = 'right',
    # add = "reg.line", conf.int = T,
    # add.params = list(color = "black", fill = "grey", size = 0.75)) +
    ylim(0, max(final_protein$cumul_gross)) +
    theme_bw() +
    geom_jitter(width = 0.006,
                size = 2,
                aes(color = MC)) + 
    scale_color_manual(values = mc_palette, name = 'Necrobiome\nsource',
                       labels = c('Beit Guvrin ungrazed', 
                                  'Beit Guvrin grazed',
                                  'Golan Heights ungrazed', 
                                  'Golan Heights grazed')) +
    geom_smooth(method="lm") +
    # stat_smooth(aes(y=cumul_gross),
    #             method = "lm",
    #             formula = my_formula, se = T, size = 0.75) +
    stat_poly_eq(formula = my_formula,
                 # aes(label = paste(..eq.label.., ..rr.label.., ..AIC.label.., sep = "~~~")),
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) + 
    theme_yaya() + 
    theme(plot.title = element_blank())

ggsave(here::here('results/7.3_protein_proportion_all.png'), width = 8, height = 5, dpi= 600)

lmer_final_protein <- lmerTest::lmer(cumul_gross ~ protein_prop + (1|MC), data = final_protein)
summary(lmer_final_protein)# p value very very very small
plot(lmer_final_protein)

lm_final_protein <- lm(cumul_gross ~ protein_prop, data = final_protein)
summary(lm_final_protein) # p val also very very small

# prot_all <- ggscatter(final_protein, x = 'Protein proportion', y = 'Cumulative C mineralized',
#                       color = 'MC',
#                       palette = mc_palette,
#                       title = 'Protein proportion vs CO2: ALL',
#                       legend = 'right',
#                       add = "reg.line", conf.int = T,
#                       add.params = list(color = "black", fill = "grey", size = 0.75)) +
#     # geom_jitter(aes(color = 'MC')) +
#     # guides(color = guide_legend(reverse = TRUE)) + 
#     # stat_cor(method = "pearson", label.x = 0)
#     stat_smooth(aes(y=cumul_gross),
#                 method = "lm",
#                 formula = my_formula, se = T, size = 0.75) +
#     stat_poly_eq(formula = my_formula, 
#                  aes(label = paste(..eq.label.., ..rr.label.., ..AIC.label.., sep = "~~~")), 
#                  parse = TRUE)
# prot_all
# # ggplotly(prot_all)

if(save_toggle == TRUE) 
    ggsave(paste(here::here('results/',folder_date),
                 switch_file, 'end_prot_ALL.png',
                 sep="_"), width=7, height=6, dpi=600)

##### graphing by each phase, for each MC ####

graph_cumul.g <- function(which_MC, which_phase, source_data, palette_colors) {
    prot_title <- paste('Protein proportion vs CO2:', which_MC, 'through phase', which_phase)
    
    y_dims <- c(0, (max(source_data[which(source_data$phase == which_phase),]$cumul_gross))*1.1)
    source_MC <- source_data %>% 
        filter(MC == which_MC,
               phase == which_phase)
    
    g_MC <- ggscatter(source_MC, x = 'protein_prop', y='cumul_gross',
                      color = 'treatment',
                      palette = palette_colors,
                      title = prot_title,
                      legend = 'right',
                      xlab = 'Protein proportion', 
                      ylab = 'Cumulative gross CO2', 
                      ylim = y_dims,
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
    prot_title <- paste('Protein proportion vs CO2:', which_MC, 'phase', which_phase,'ONLY')
    
    y_dims <- c(0, (max(source_data[which(source_data$phase == which_phase),]$cumul_phase_gross))*1.1)
    # y_dims <- c(0, (max(source_data$cumul_phase_gross))*1.1)
    source_MC <- source_data %>% 
        filter(MC == which_MC,
               phase == which_phase)
    
    g_MC <- ggscatter(source_MC, x = 'protein_prop', y='cumul_phase_gross',
                      color = 'treatment',
                      palette = palette_colors,
                      title = prot_title,
                      legend = 'right',
                      xlab = 'Protein proportion', 
                      ylab = 'Cumulative phase gross CO2', 
                      ylim = y_dims,
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
map(MCs, function(x){graph_cumul.p(x, 1, end_protein, cbPalette)})
map(MCs, function(x){graph_cumul.p(x, 2, end_protein, cbPalette)})
map(MCs, function(x){graph_cumul.p(x, 3, end_protein, cbPalette)})

