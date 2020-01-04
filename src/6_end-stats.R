# This script is for generating plots for end-of-experiment data
library(viridis)
library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(plotly)
library(here)
source(here::here('src/0_exp-1-fxns.R'))

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
imported_data <- read_rds(paste0(here::here('results/4_tubes_to_plot_'),switch_file,'.rds')) %>% 
    filter(treatment != 'R')
trt_levels <- c('C', '8', '7', '6', '5', '4', '3', '2', '1')

# filter data to only include final cumulative differences
max_p1 <- max(filter(imported_data, phase == 1)$exp_count)
max_p2 <- max(filter(imported_data, phase == 2)$exp_count)
max_p3 <- max(filter(imported_data, phase == 3)$exp_count)

end_p1 <- filter(imported_data, exp_count == max_p1)
end_p2 <- filter(imported_data, exp_count == max_p2)
end_p3 <- filter(imported_data, exp_count == max_p3)
end_p <- list(end_p1, end_p2, end_p3)

amend_trts <- read_csv(here::here('data/protein_treatments.csv'))
end_percent <- left_join(end_p3, amend_trts) %>% 
    group_by(MC)

# graph faceted boxplots at the end of each phase
end_all <- rbind(end_p1, end_p2, end_p3)
cumul_gr_plot <- ggboxplot(end_all, x = 'MC', y = 'cumul_gross',
                           color = 'treatment', 
                           title = paste0('Cumulative C by tube at end of each phase, gross C'), 
                           xlab = 'treatment',
                           ylab = 'cumul gross co2 by tube',
                           legend = 'right')
# cumul_gr_plot
facet(cumul_gr_plot, facet.by='phase')
ggsave(paste(here::here('results/', folder_date), '6', switch_file, 
             'facet.phases_boxplot_cumul_gross.png', sep="_"), width=10, height=8, dpi=600)



#cumulative values in a violin plot
cumul_gr_viol <- end_data %>% 
    ggplot(aes(x = MC, y = cumul_gross))+
    geom_violin()+
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    theme(legend.position="none",
          plot.title = element_text(size=11)) +
    ggtitle("Violin chart") +
    xlab("MC")
cumul_gr_viol



# ==== decide which phase to check ====
# k <- 2

for (k in seq_along(end_p))
{
    
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

    
    # theme(legend.position='none')
    # ggsave(paste(paste0(here::here('results/',switch_file)), 'phase', k, 'boxplot_cumul_gross.png', sep="_"), width=10, height=8, dpi=600)
    ggsave(paste(here::here('results/', folder_date), '6', switch_file, 'phase',
                 k, 'boxplot_cumul_gross.png', sep="_"), width=10, height=8, dpi=600)
    
    
    cumul_dif_plot <- ggboxplot(end_data, x = 'MC', y = 'cumul_diff',
                                color = 'treatment', 
                                title = paste0('C by tube at end of phase ',k,', diff from controls'), 
                                xlab = 'direction',
                                ylab = 'cumul diff from C, co2 by tube')
    # theme(legend.position='none')
    
    # to try and see which tubes had the most outlying values
    # ggplotly(cumul_dif_plot)
    
    ggsave(paste(here::here('results/', folder_date),'6', switch_file, 'phase', 
                 k, 'boxplot_cumul_dif.png', sep="_"), width=10, height=8, dpi=600)
    
}

# ==== summary stats=====

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


# ==== ANOVAs=====
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


# ==== graphs for protein content vs cumulative CO2 at end ====
my_formula <- y ~ poly(x, 2)
# my_formula <- y ~ x

end_protein <- select(end_percent, -carb_prop) 
cbPalette <- viridis(10)

# # checking without the control
# end_protein <- filter(end_protein, treatment != 'C')

ggscatter(end_protein, x = 'protein_prop', y = 'cumul_gross',
          color = 'treatment',
          shape = 'MC',
          palette = cbPalette,
          title = 'protein proportion vs CO2: ALL',
          legend = 'right',
          # add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75)) +
    # stat_cor(method = "pearson", label.x = 0)
    
    stat_smooth(aes(y=cumul_gross),
                method = "lm",
                formula = my_formula, se = T, size = 0.75) +
    stat_poly_eq(formula = my_formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    stat_poly_eq(aes(label = ..AIC.label..),
                 label.x = "right", label.y = "bottom", size = 3,
                 formula = my_formula,
                 parse = TRUE)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_ALL.png',
             sep="_"), width=10, height=8, dpi=600)


end_prot_bu <- filter(end_protein, MC=='BU')
ggscatter(end_prot_bu, x = 'protein_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'protein proportion vs CO2: BU',
          legend = 'right',
          # add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_cor(method = "pearson", label.x = 0, label.y = 20000) +
    # stat_regline_equation(label.x =0)
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
    stat_smooth(aes(y=cumul_gross), method = "lm",formula = y ~ x , se = T, size = 0.75) +
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2)) +
    # stat_poly_eq(formula = y ~ x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
    stat_poly_eq(aes(label = ..AIC.label..), label.x = "right", label.y = "bottom", size = 3, formula = y ~ x,  parse = TRUE)

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
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))  # quadratic fitting
    # stat_cor(method = "pearson", label.x = 0)
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
    stat_smooth(aes(y=cumul_gross), method = "lm",formula = y ~ x , se = T, size = 0.75) +
    stat_poly_eq(aes(label = ..AIC.label..), label.x = "right", label.y = "bottom", size = 3, formula = y ~ x,  parse = TRUE)

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
    # stat_cor(method = "pearson", label.x = 0)
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
    stat_smooth(aes(y=cumul_gross), method = "lm",formula = y ~ x , se = T, size = 0.75) +
    stat_poly_eq(aes(label = ..AIC.label..), label.x = "right", label.y = "bottom", size = 3, formula = y ~ x,  parse = TRUE)

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
    # stat_cor(method = "pearson", label.x = 0)
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
    stat_smooth(aes(y=cumul_gross), method = "lm",formula = y ~ x , se = T, size = 0.75) +
    stat_poly_eq(aes(label = ..AIC.label..), label.x = "right", label.y = "bottom", size = 3, formula = y ~ x,  parse = TRUE)


ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_prot_GG.png',
             sep="_"), width=10, height=8, dpi=600)


#================================================

# ==== graphs for carbs ====
end_carb <- select(end_percent, -protein_prop)
end_carb <- filter(end_carb, treatment != 'C')

ggscatter(end_carb, x = 'carb_prop', y = 'cumul_gross',
          color = 'treatment',
          shape = 'MC',
          palette = cbPalette,
          title = 'carb proportion vs CO2: ALL',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75)) +
    stat_cor(method = "pearson", label.x = 0)
# stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_carb_ALL.png',
             sep="_"), width=10, height=8, dpi=600)

end_carb_bu <- filter(end_carb, MC=='BU')
ggscatter(end_carb_bu, x = 'carb_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'carb proportion vs CO2: BU',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
# stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_carb_BU.png',
             sep="_"), width=10, height=8, dpi=600)

end_carb_bg <- filter(end_carb, MC=='BG')
ggscatter(end_carb_bg, x = 'carb_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'carb proportion vs CO2: BG',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
# stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_carb_BG.png',
             sep="_"), width=10, height=8, dpi=600)


end_carb_gu <- filter(end_carb, MC=='GU')
ggscatter(end_carb_gu, x = 'carb_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'carb proportion vs CO2: GU',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
# stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_carb_GU.png',
             sep="_"), width=10, height=8, dpi=600)

end_carb_gg <- filter(end_carb, MC=='GG')
ggscatter(end_carb_gg, x = 'carb_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'carb proportion vs CO2: GG',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
# stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_carb_GG.png',
             sep="_"), width=10, height=8, dpi=600)


# ==== graphs for protein:carb proportion ====
end_propor <- end_percent %>% 
    mutate(propor_prop = protein_prop/carb_prop)

# end_propor <- filter(end_propor, treatment != 'C')

ggscatter(end_propor, x = 'propor_prop', y = 'cumul_gross',
          color = 'treatment',
          shape = 'MC',
          palette = cbPalette,
          title = 'prot:carb proportion vs CO2: ALL',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75)) +
    # stat_cor(method = "pearson", label.x = 0)
    stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_propor_ALL.png',
             sep="_"), width=10, height=8, dpi=600)

end_propor_bu <- filter(end_propor, MC=='BU')
ggscatter(end_propor_bu, x = 'propor_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'prot:carb proportion vs CO2: BU',
          legend = 'right',
          # add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    stat_smooth(aes(y=cumul_gross), 
                method = "lm", 
                # formula = y ~ x + I(x^2))
                formula = my_formula_quad, se = T, size = 0.75) +
    stat_poly_eq(formula = my_formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) + 
    stat_poly_eq(aes(label = ..AIC.label..),
                 label.x = "right", label.y = "bottom", size = 3,     
                 formula = my_formula, 
                 parse = TRUE)

# stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_propor_BU.png',
             sep="_"), width=10, height=8, dpi=600)

end_propor_bg <- filter(end_propor, MC=='BG')
ggscatter(end_propor_bg, x = 'propor_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'prot:carb proportion vs CO2: BG',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_propor_BG.png',
             sep="_"), width=10, height=8, dpi=600)


end_propor_gu <- filter(end_propor, MC=='GU')
ggscatter(end_propor_gu, x = 'propor_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'prot:carb proportion vs CO2: GU',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_propor_GU.png',
             sep="_"), width=10, height=8, dpi=600)

end_propor_gg <- filter(end_propor, MC=='GG')
ggscatter(end_propor_gg, x = 'propor_prop', y='cumul_gross',
          color = 'treatment',
          palette = cbPalette,
          title = 'prot:carb proportion vs CO2: GG',
          legend = 'right',
          add = "reg.line", conf.int = T,
          add.params = list(color = "black", fill = "grey", size = 0.75))+
    # stat_smooth(aes(y=cumul_gross), method = "lm", formula = y ~ x + I(x^2))
    stat_cor(method = "pearson", label.x = 0)
ggsave(paste(here::here('results/',folder_date),
             switch_file, 'end_propor_GG.png',
             sep="_"), width=10, height=8, dpi=600)

#=============================
# trying new things for linear models

amend_props <- read_csv(here::here('data/protein_treatments.csv'))
data_by_MC <- left_join(end_data, amend_props) %>% 
    group_by(MC) %>% 
    nest()
data_by_MC

MC_model <- function(df){
    lm(cumul_gross ~ protein_prop, data = df)
}

data_by_MC <- data_by_MC %>% 
    mutate(model = map(data, MC_model))
data_by_MC

data_by_MC <- data_by_MC %>% 
    mutate(resids = map2(data, model, add_residuals)
    )

resids <- unnest(data_by_MC, resids)
resids

resids %>% 
    ggplot(aes(trt_ID, resid)) +
    geom_line(aes(group = MC), alpha = 1/3) + 
    geom_smooth(se = FALSE)
