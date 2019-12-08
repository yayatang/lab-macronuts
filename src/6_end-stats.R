# This script is for generating plots for end-of-experiment data
library(viridis)
library(tidyverse)
library(ggpubr)
library(plotly)
library(here)
source(here::here('src/0_exp-1-fxns.R'))

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
imported_data <- read_csv(paste0(here::here('results/4_tubes_to_plot_'),switch_file,'.csv')) %>% 
    filter(treatment != 'R')
levels(imported_data$treatment) <- c('C', '8', '7', '6', '5', '4', '3', '2', '1')

# filter data to only include final cumulative differences
max_p1 <- max(filter(imported_data, phase == 1)$exp_count)
max_p2 <- max(filter(imported_data, phase == 2)$exp_count)
max_p3 <- max(filter(imported_data, phase == 3)$exp_count)

end_p1 <- filter(imported_data, exp_count == max_p1)
end_p2 <- filter(imported_data, exp_count == max_p2)
end_p3 <- filter(imported_data, exp_count == max_p3)
end_p <- list(end_p1, end_p2, end_p3)

#=== decide which phase to check
k <- 2

#=== check ANOVAs for each phase
# can comment out to only look at end values
# for (k in seq_along(end_p)){
#===make + save plots
end_data <- end_p[[k]]

# # look at the data
# sample_n(end_data, 5)
# str(end_data)
# table(end_data$MC, end_data$treatment)

# boxplots for C mineralization of all by treatment and MC
cumul_gr_plot <- ggboxplot(end_data, x = 'MC', y = 'cumul_gross',
                           color = 'treatment', 
                           title = paste0('C by tube at end of phase ',k,', gross C'), 
                           xlab = 'treatment',
                           ylab = 'cumul gross co2 by tube')
# theme(legend.position='none')
# ggsave(paste(paste0(here::here('results/',switch_file)), 'phase', k, 'boxplot_cumul_gross.png', sep="_"), width=10, height=8, dpi=600)
ggsave(paste(here::here('results/'), '6',switch_file, 'phase', k, 'boxplot_cumul_gross.png', sep="_"), width=10, height=8, dpi=600)


cumul_dif_plot <- ggboxplot(end_data, x = 'MC', y = 'cumul_diff',
                            color = 'treatment', 
                            title = paste0('C by tube at end of phase ',k,', diff from controls'), 
                            xlab = 'direction',
                            ylab = 'cumul diff from C, co2 by tube')
# theme(legend.position='none')

# to try and see which tubes had the most outlying values
# ggplotly(cumul_dif_plot)

ggsave(paste(here::here('results/'),'6',switch_file,'phase', k, 'boxplot_cumul_dif.png', sep="_"), width=10, height=8, dpi=600)

# =====summary stats=====

# C mineralization GROSS means by treatment
samp1_trt_gr <- group_by(end_data, treatment)
samp1_trt_gr_s <- summarise(samp1_trt_gr, 
                            count = n(),
                            mean = mean(cumul_gross), 
                            se = se(cumul_gross))

# C mineralization GROSS means by MC
samp1_MC_gr <- group_by(end_data, MC)
samp1_MC_gr_s <- summarise(samp1_MC_gr, 
                           count = n(),
                           mean = mean(cumul_gross), 
                           se = se(cumul_gross))

# C mineralization DIFF means by treatment
samp1_trt_dif <- group_by(end_data, treatment)
samp1_trt_dif_s <- summarise(samp1_trt_dif, 
                             count = n(),
                             mean = mean(cumul_diff), 
                             se = se(cumul_diff))

# C mineralization DIFF means by MC
samp1_MC_dif <- group_by(end_data, MC)
samp1_MC_dif_s <- summarise(samp1_MC_dif, 
                            count = n(),
                            mean = mean(cumul_diff), 
                            se = se(cumul_diff))


# =====ANOVAs=====
# 2-way ANOVA for MC and treatment -- GROSS
res.gross.aov2 <- aov(cumul_gross ~ MC + treatment, data = end_data)
summary(res.gross.aov2)

res.gross.aovx <- aov(cumul_gross ~ MC * treatment, data = end_data)
summary(res.gross.aovx)

# 2-way ANOVA for MC and treatment -- DIFF
res.diff.aov2 <- aov(cumul_diff ~ MC + treatment, data = end_data)
summary(res.diff.aov2)

res.diff.aovx <- aov(cumul_diff ~ MC * treatment, data = end_data)
summary(res.diff.aovx)

# res.diff.aov1 <- aov(cumul_diff ~ treatment, data = end_data)
# summary(res.diff.aov1)
# 
# #===assumptions tests===
# plot(res.diff.aov2) #check plots for normally distributed values
# plot(res.diff.aovx) #check plots for normally distributed values
# 
# plot(res.diff.aov2) #check plots for normally distributed values
# plot(res.diff.aovx) #check plots for normally distributed values
# 
# 
# # levene's test for homogeneity of variances
# library(car)
# samp1_levenes <- leveneTest(cumul_diff ~ treatment, data = end_data)
# # p-value > 0.05 means no evidence to suggest variances statistically diff
# samp1_levenes

#=== post hoc
TukeyHSD(res.gross.aov2, which="treatment")
TukeyHSD(res.diff.aov2, which="treatment")
# }


#=== graphs for protein content vs cumulative CO2 at end

protein_trts <- read_csv(here::here('data/protein_treatments.csv'))
end_protein <- left_join(end_data, protein_trts)
cbPalette <- viridis(10)

ggscatter(end_protein, x = 'protein_prop', y = 'cumul_gross',
          color = 'treatment',
          shape = 'MC',
          palette = cbPalette,
          title = 'protein proportion vs CO2: ALL',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75)) +
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))  
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_ALL.png',
             sep="_"), width=10, height=8, dpi=600)


end_prot_bu <- filter(end_protein, MC=='BU')
ggscatter(end_prot_bu, x = 'protein_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'protein proportion vs CO2: BU',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))  
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_BU.png',
             sep="_"), width=10, height=8, dpi=600)

end_prot_bg <- filter(end_protein, MC=='BG')
ggscatter(end_prot_bg, x = 'protein_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'protein proportion vs CO2: BG',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))  
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_BG.png',
             sep="_"), width=10, height=8, dpi=600)


end_prot_gu <- filter(end_protein, MC=='GU')
ggscatter(end_prot_gu, x = 'protein_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'protein proportion vs CO2: GU',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))  
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_GU.png',
             sep="_"), width=10, height=8, dpi=600)

end_prot_gg <- filter(end_protein, MC=='GG')
ggscatter(end_prot_gg, x = 'protein_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'protein proportion vs CO2: GG',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))  
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_GG.png',
             sep="_"), width=10, height=8, dpi=600)
