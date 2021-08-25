# 7.2 model comparison

library(tidyverse)
library(ggpubr)
library(lme4)
library(broom)
library(viridis)
library(plotly)
library(gridExtra) # for grid.arrange fxn, can remove if not using

save_toggle <- TRUE

#########
if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

#data by 
compare_raw <- read_rds(paste0(here::here('results/6_end_data_rows_'), 
                               outlier_name, switch_file, '.rds')) %>%
    filter(treatment != 'R') %>% 
    select(-interped) %>%
    mutate(protein2 = protein_prop ^2)

compare_end <- filter(compare_raw, phase == 3)

######

# 1) testing the overall model

formula_null <- 'cumul_gross ~ grazing + (1|location)'
formula_linear <- 'cumul_gross ~ grazing + protein_prop + (1|location)'
formula_quad <-  'cumul_gross ~ grazing + I(protein_prop^2) + protein_prop + (1|location)'

model_null <- lmer(as.formula(formula_null), compare_end)
model_linear <- lmer(as.formula(formula_linear), compare_end)
model_quad <- lmer(as.formula(formula_quad), compare_end)

anova(model_null, model_linear)
anova(model_linear, model_quad)

#########

# 2) testing for model fit by MC (no more random effect)

MC_list <- c('BU', 'BG', 'GU', 'GG')
phase_list <- c(3)

AIC_table <- expand_grid(MC_list, phase_list) %>% 
    rename(MC = MC_list,
           phase = phase_list)
AIC_table$x_var <- 'protein_prop'
AIC_table$y_var <- 'cumul_gross'
AIC_table$AIC_lin <- numeric(nrow(AIC_table))
AIC_table$AIC_quad <- numeric(nrow(AIC_table))
AIC_table$lin_ax <- numeric(nrow(AIC_table))
AIC_table$lin_b <- numeric(nrow(AIC_table))
AIC_table$lin_r2 <- numeric(nrow(AIC_table))
AIC_table$quad_ax2 <- numeric(nrow(AIC_table))
AIC_table$quad_bx <- numeric(nrow(AIC_table))
AIC_table$quad_c <- numeric(nrow(AIC_table))
AIC_table$quad_r2 <- numeric(nrow(AIC_table))


#--------------

for (graph_phase in phase_list){
    for (mod_MC in seq_along(MC_list)){
        # graph_phase <- 1
        # mod_MC <- 1
        compare_MC <- filter(compare_end, MC == MC_list[mod_MC])
        
        linear_mod <- lm(cumul_gross ~ protein_prop, data = compare_MC)
        summary(linear_mod)
        tidy(linear_mod)
        # glance(linear_mod)$r.squared ## broom pkg fxn
        # lin_coeff <- linear_mod$coefficients
        
        quad_mod <- lm(cumul_gross~ protein_prop + I(protein_prop^2), data = compare_MC)
        summary(quad_mod)
        tidy(quad_mod)
        # quad_coeff <- quad_mod$coefficients
        
        print(anova(linear_mod, quad_mod))
        
        # copy model AICs to table
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$AIC_lin <- AIC(linear_mod)
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$AIC_quad <- AIC(quad_mod)
        
        # #working on extracting equation parameters...
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$lin_ax <- 
            tidy(linear_mod)$estimate[2]
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$lin_b <-
            tidy(linear_mod)$estimate[1]
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$lin_r2 <- 
            glance(linear_mod)$adj.r.squared[1]
        
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$quad_ax2 <- 
            tidy(quad_mod)$estimate[3]
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$quad_bx <-
            tidy(quad_mod)$estimate[2]
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$quad_c <-
            tidy(quad_mod)$estimate[1]
        AIC_table[which(AIC_table$MC == MC_list[mod_MC] & AIC_table$phase== graph_phase), ]$quad_r2 <- 
            glance(quad_mod)$adj.r.squared[1]
    }
}

AIC_table[,5:8] <- round(AIC_table[,5:8], digits = 1) # for linear coeffs
AIC_table[,9] <- round(AIC_table[,9], digits = 2) # for quad coeffs
AIC_table[,10:12] <- round(AIC_table[,10:12], digits = 1) # for quad coeffs
AIC_table[,13] <- round(AIC_table[,13], digits = 2) # for quad coeffs

AIC_table$AIC_model <- if_else(AIC_table$AIC_lin < AIC_table$AIC_quad, 'linear', 'quad')
AIC_table$AIC_eq <- if_else(AIC_table$AIC_model == 'linear', 
                            paste0('y=', AIC_table$lin_ax,'x+',AIC_table$lin_b,', R^2=',AIC_table$lin_r2),
                            paste0('y=', AIC_table$quad_ax2,'x^2+',AIC_table$quad_bx,'x+',AIC_table$quad_c,', R^2=',AIC_table$quad_r2))

AIC_table <- AIC_table %>% 
    arrange(phase)
AIC_table
write_csv(AIC_table, here::here('results/AIC_model_selection_phase_3.csv'))


##### PHASE 3:graphing data from each MC ####
formula_lin <- y ~ x
formula_quad <- y ~ poly(x, 2)

graph_phase <- 3

cbPalette <- viridis(10)

# for (graph_phase in phase_list){
# combine info with results of AIC calculations and model selection
# NOT CORRECT
compare_AIC <- compare_end %>% 
    summarize(mean_gross = mean(cumul_gross),
              se_gross = se(cumul_gross),
              mean_phase_gross = mean(cumul_phase_gross),
              se_phase_gross = se(cumul_phase_gross)) %>% 
    left_join(AIC_table)


#### plot ALL tubes in phase 3 by final, gross cumulative values 
phase_min <- min(compare_AIC$cumul_gross) - 0.1*abs(min(compare_AIC$cumul_gross))
phase_max <- max(compare_AIC$cumul_gross) + 0.1*abs(max(compare_AIC$cumul_gross))

gr_p_ALL <- ggplot(compare_AIC,
                   aes(protein_prop,
                       cumul_gross,
                       color = treatment,
                       text = sampleID)) +
    geom_point(size=1) +
    # labs(x="amendment protein proportion",
    labs(x="Protein proportion",
         y = "CO2-C mineralized",
         title = paste0("Cumulative minerlization at end of experiment")) + 
    # title = paste0("Cumulative respiration from experiment beginning to end of phase ", ## for cumul_gross
    #                # title = paste0("Cumulative respiration from within phase ", ## for cumul_phase
    #                               graph_phase)) + 
    ylim(phase_min, phase_max) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_grid(cols = vars(MC)) + 
    scale_color_manual(values = cbPalette) +
    stat_smooth(data = subset(compare_AIC, AIC_model == 'linear'),
                method = lm,
                formula = formula_lin,
                aes(group = MC)) + 
    stat_smooth(data = subset(compare_AIC, AIC_model == 'quad'),
                method = lm,
                formula = formula_quad,
                aes(group = MC))

gr_p_ALL
ggsave(paste(here::here('results/', today()),
             'ALL.MC_phase',graph_phase,'scatter_N_cumul_gross.png', sep="_"),
       width=13, height=2.5, dpi=600)
# }

######## by MC #######
which_MC <- 'GU'

graph_phase <- 3

#### plot ALL tubes in phase 3 by final, gross cumulative mineralization
phase_min <- min(compare_end$cumul_gross) - 0.1*abs(min(compare_end$cumul_gross))
phase_max <- max(compare_end$cumul_gross) + 0.1*abs(max(compare_end$cumul_gross))

protein_key <- compare_end %>% 
    ungroup() %>% 
    select(treatment, protein_prop) %>% 
    unique()

compare_end_summ <- compare_end %>% 
    group_by(treatment) %>% 
    summarize(mean_trt_diff = mean(cumul_gross),
              se_trt_diff = se(cumul_gross),
              phase = median(phase)) %>% 
    mutate(MC = which_MC) %>% 
    left_join(AIC_table) %>% 
    left_join(protein_key)

gr_p1 <- ggplot(compare_end_summ,
                aes(protein_prop,
                    mean_trt_diff,
                    color = treatment)) +
    geom_point(size=2) +
    ylim(phase_min, phase_max) +
    labs(x="Protein proportion",
         y = "CO2-C mineralized",
         title = paste0("Cumulative respiration at end of experiment by treatment for ", which_MC)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    # facet_grid(cols = vars(MC)) +
    scale_color_manual(values = cbPalette) +
    stat_smooth(data = subset(compare_end_summ, AIC_model == 'linear'),
                method = lm,
                formula = formula_lin,
                aes(group = MC)) + 
    stat_smooth(data = subset(compare_end_summ, AIC_model == 'quad'),
                method = lm,
                formula = formula_quad,
                aes(group = MC)) 

ggplotly(gr_p1)
ggsave(paste(here::here('results/', today()),
             which_MC,graph_phase,'scatter_N_cumul_gross_at_phase_end.png', sep="_"),
       width=10, height=8, dpi=600)
