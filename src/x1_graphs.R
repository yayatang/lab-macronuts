# x1 graphs 
# for demonstrating more of the data from experiment 1
# for committee meeting update

#plots to make:
# 1. end of phase 1: C:N vs respiration (both gross + netto diff)
# 2. end of phase 1: compare respiration between treatments

library(tidyverse)
library(lubridate)
library(here)
library(ggpubr)
library(ggpmisc)
library(plotly)

trt_graphs0 <- read_rds(here::here('results/4_trts_to_plot_switched.rds')) %>% 
    filter(treatment != 'R')
tube_graphs0 <- read_rds(here::here('results/4_tubes_to_plot_switched.rds')) %>% 
    filter(treatment != 'R')
levels(tube_graphs0$MC) <- factor(c('BU', 'BG', 'GU', 'GG'))

'%!in%' <- function(x,y)!('%in%'(x,y))

# this may cause level issues down the line...
# order the levels so they make sense on the plot
# trt_levels <- c('R', 'C', '8', '7', '6', '5', '4', '3', '2', '1')

#### checking the normality of the daily inferred + gross data ####
# # is this even valid for repeated measures? or
# ggqqplot(tube_graphs0$infer_samp_perday)
# ggqqplot(trt_graphs0$trt_gross_daily)


#### find the max value per phase... maybe make into fxn one day?*** ####
max_p1 <- max(filter(tube_graphs0, phase == 1)$exp_count)
max_p2 <- max(filter(tube_graphs0, phase == 2)$exp_count)
max_p3 <- max(filter(tube_graphs0, phase == 3)$exp_count)

end_p1 <- filter(tube_graphs0, exp_count == max_p1)
end_p2 <- filter(tube_graphs0, exp_count == max_p2)
end_p3 <- filter(tube_graphs0, exp_count == max_p3)
end_p <- list(end_p1, end_p2, end_p3)

amend_trts <- read_csv(here::here('data/protein_treatments.csv')) %>% 
    filter(treatment != 'R')

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
CN_soils <- data_CN[which(data_CN$sample_name %in% c('BU', 'BG', 'GU', 'GG')),] %>% 
    rename(MC = sample_name)
CN_trt <- data_CN[6:13,] %>% 
    rename(treatment = sample_name,
           trt_CN = CN_ratio,
           trt_C_total_mg = C_total_mg,
           trt_N_total_mg = N_total_mg)

site_use <- tibble(MC = c('BU', 'BG', 'GU', 'GG'),
                   site = c(rep('Beit Guvrin', 2), rep('Golan Heights', 2)),
                   land_use = rep(c('ungrazed', 'grazed'), 2))
CN_soils <- CN_soils %>% left_join(site_use)
levels(CN_soils$MC) <- factor(c('BU', 'BG', 'GU', 'GG'))

##### make barplot showing C and N composition of soil
CN_soils_wide <- gather(CN_soils, 
                        key = "element",
                        value = "elemental_mg", C_total_mg, N_total_mg) %>% 
    gather(key = "element", 
           value = "elemental_percent", C_percent, N_percent)
CN_soils_wide$element <- substr(CN_soils_wide$element, 1, 1)
CN_soils_wide <- CN_soils_wide %>% 
    distinct(MC, element, .keep_all = TRUE) %>% 
    mutate(total_mg = elemental_percent /100 * sample_mass)
CN_soils_wide$element <- factor(CN_soils_wide$element, levels = c('N', 'C'))

ggplot(CN_soils_wide,
       aes(MC, total_mg, fill = element)) +
    geom_bar(position = "stack", stat = "identity", width=0.3)

# merge amendment protein proportion with elemental analysis
amend_properties <-  left_join(amend_trts, CN_trt)
# this table will need to be merged with end of phase data

CN_tube <- left_join(amend_properties, CN_soils_wide)


##### graphing data from phase 1, C:N vs cumulative @ end of phase 1 ####
formula_lin <- y ~ x
formula_quad <- y ~ poly(x, 2)

p1_outliers <- c('GG7.4', 'GU5.6', 'GG6.5', 'BG3.2')

phase1_end <- left_join(end_p1, amend_properties)
phase1_end <- phase1_end %>% 
    filter(sampleID %!in% p1_outliers)

# graphing N mass versus phase 1 cumulative respiration
# which_MC <- 'GG'
# data_MC <- filter(phase1_end, MC == which_MC)
# data_MC <- phase1_end

gr_p1 <- ggplot(phase1_end, 
                aes(trt_N_total_mg, 
                    cumul_diff,
                    color = treatment,
                    # shape = MC,
                    text = sampleID)) +
    geom_point(size=1) +
    # geom_boxplot() +
    labs(x="treatment N mass added (mg)",
         y = "CO2-C minerlized, corrected from reference",
         title = paste("heterotrphic necromass N vs phase 1 marginal mineralized C, ALL")) + #, which_MC)) +
    ylim(0, max(phase1_end$trt_N_total_mg)+10) +
    theme_bw() + 
    facet_grid(cols = vars(MC)) +
    stat_smooth(method = lm,
                formula = formula_quad,
                aes(fill = treatment,
                    # color = MC,
                    group = MC))
    # stat_poly_eq(formula = formula_quad, 
                 # aes(label = paste(..eq.label.., ..rr.label.., ..AIC.label.., 
                                   # sep = "~~~")), 
                 # parse = TRUE)
gr_p1
ggsave(paste(here::here('results/', today()), 
             'facet.MC_scatter_reg_cumul_diff.png', sep="_"), 
       width=12, height=5, dpi=600)

ggplotly(gr_p1)

# # are they diff? ANOVA not valid bc nonnormally distributed data
# N_aov <- aov(trt_N_total_mg ~ MC + treatment, data = phase1_end)
# summary(N_aov)

##### graphs for phase 2 #####
phase2_end <- left_join(end_p2, amend_properties)

# graphing N mass versus phase 1 cumulative respiration
formula_lin <- y ~ x
formula_quad <- y ~ poly(x, 2)

which_MC <- 'BU'
data_MC <- filter(phase2_end, MC == which_MC)
# data_MC <- phase2_end

gr_p2 <- ggplot(data_MC, 
                aes(trt_N_total_mg, 
                    cumul_diff,
                    color = treatment,
                    shape = MC)) +
    geom_point(size=2) +
    # geom_boxplot() +
    # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize=0.3) +
    labs(x="treatment N mass added (mg)",
         y = "CO2-C minerlized, corrected from reference",
         title = paste("amendment N vs phase 2 total mineralized C,", which_MC)) +
    ylim(0, max(phase2_end$trt_N_total_mg)+10) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
# facet_grid(MC ~ .)
# geom_smooth(method = lm, aes(fill = treatment,
#                              color = MC,
#                              group = MC))
gr_p2
ggplotly(gr_p2)



######
# stat_smooth(aes(y=cumul_gross), 
#             method = "lm", 
#             formula = formula_lin)
plot_reg <- lm(formula_lin, data = data_MC)

eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(plot_reg)[1], digits = 4),
                      b = format(coef(plot_reg)[2], digits = 4),
                      r2 = format(summary(plot_reg)$r.squared, digits = 3)))

dftext <- data.frame(x = 70, y = 50, eq = as.character(as.expression(eq)))

gr_p1 + geom_text(aes(label = eq), data = dftext, parse = TRUE)

#### graphs by phase + MC ##########
library(ggpmisc)

all_MCs <- unique(end_p1$MC)
end_p_all <- rbind(end_p1, end_p2, end_p3) %>% 
    left_join(amend_properties) %>% 
    left_join(CN_soils, by="MC")

graphs_p1

for (k in 1:length(end_p)){
    for (l in 1:length(all_MCs)){
        
        curr_phase_data <- end_p_all %>% 
            filter(phase == k, 
                   MC == all_MCs[[l]])
        
        ggscatter(curr_phase_data,
                  x = 'trt_N_total_mg', 
                  y = 'cumul_gross',
                  color = 'treatment',
                  shape = 'MC',
                  group = 'MC',
                  # palette = cbPalette,
                  title = paste('phase', k, ':',
                                'amendment N vs respiration for', all_MCs[[l]]),
                  legend = 'right',
                  add = "reg.line", conf.int = T,
                  add.params = list(color = "black", fill = "grey", size = 0.75)) +
            stat_poly_eq(formula = formula_lin, 
                         aes(label = paste(..eq.label.., ..rr.label.., ..AIC.label..,
                                           sep = "~~~")),
                         parse = TRUE)
        
        ggsave(paste(here::here('results/', today()),'phase', k, 'boxplot_cumul_dif.png', sep="_"), width=10, height=8, dpi=600)
        
        
    }
}
