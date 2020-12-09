# 6.2 AIC table making code

## need to import data from somewhere

###### AIC: regression models ########
## Run this section to generate the table of AIC to determine whether
## the regression fit should be linear or quadratic (also used for graphing)

# graphing should also be run from HERE (to re-fit models)
# fitting linear regression models for ALL phases & MCs
MC_list <- c('BU', 'BG', 'GU', 'GG')
phase_list <- c(1:3)

AIC_table <- expand_grid(MC_list, phase_list) %>% 
    rename(MC = MC_list,
           phase = phase_list)
AIC_table$x_var <- 'protein_prop' #trt_N_total_mg
AIC_table$y_var <- 'cumul_diff'
AIC_table$AIC_lin <- numeric(nrow(AIC_table))
AIC_table$AIC_quad <- numeric(nrow(AIC_table))
AIC_table$lin_ax <- numeric(nrow(AIC_table))
AIC_table$lin_b <- numeric(nrow(AIC_table))
AIC_table$lin_r2 <- numeric(nrow(AIC_table))
AIC_table$quad_ax2 <- numeric(nrow(AIC_table))
AIC_table$quad_bx <- numeric(nrow(AIC_table))
AIC_table$quad_c <- numeric(nrow(AIC_table))
AIC_table$quad_r2 <- numeric(nrow(AIC_table))

# model_equation <- function(model, ...) {
#     format_args <- list(...)
#     
#     model_coeff <- model$coefficients
#     format_args$x <- abs(model$coefficients)
#     model_coeff_sign <- sign(model_coeff)
#     model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
#                                     model_coeff_sign == 1 ~ " + ",
#                                     model_coeff_sign == 0 ~ " + ")
#     model_eqn <- paste('Y=', #strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
#                        paste(if_else(model_coeff[1]<0, "- ", ""),
#                              do.call(format, format_args)[1],
#                              paste(model_coeff_prefix[-1],
#                                    do.call(format, format_args)[-1],
#                                    "* X ",
#                                    sep = "", collapse = ""),
#                              sep = ""))
#     return(model_eqn)
# }
# model_equation(linear_mod, digits = 2) # from function above, would need to be inserted below

for (graph_phase in phase_list){
    for (mod_MC in seq_along(MC_list)){
        p_outliers <- list(c('GG7.4', 'GU5.6', 'GG6.5', 'BG3.2'),
                           c('GG1.5'),
                           c('GU6.6'))
        
        phase_end_models <- left_join(end_p[[graph_phase]], amend_properties)
        phase_end_models <- phase_end_models %>% 
            filter(sampleID %!in% p_outliers[[graph_phase]],
                   MC == MC_list[mod_MC])
        
        phase_end_models <- phase_end_models %>% 
            mutate(var_sq = protein_prop^2)
        
        linear_mod <- lm(cumul_diff~ protein_prop, data = phase_end_models)
        # tidy(linear_mod)
        # glance(linear_mod)$r.squared
        # # lin_summ <- summary(linear_mod)
        # lin_coeff <- linear_mod$coefficients
        
        quad_mod <- lm(cumul_diff~ protein_prop + var_sq, data = phase_end_models)
        # summary(quad_mod)
        quad_coeff <- quad_mod$coefficients
        
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
# write_csv(AIC_table, here::here('results/AIC_model_selection_by_phase.csv'))

#### AIC values for all inclusive models (all MCs) #### 
# linear mixed model with MC as random factor (is this what i want?)

p_outliers <- list(c('GG7.4', 'GU5.6', 'GG6.5', 'BG3.2'),
                   c('GG1.5'),
                   c('GU6.6'))

phase_end_a_mod <- left_join(end_p[[3]], amend_properties)
phase_end_a_mod <- phase_end_a_mod #%>% 
# filter(sampleID %!in% p_outliers[[3]])

phase_end_a_mod <- phase_end_a_mod %>% 
    mutate(var_sq = protein_prop^2)

## linear
linear_a_mod <- lm(cumul_diff~ protein_prop, data = phase_end_a_mod)
tidy(linear_a_mod)
glance(linear_a_mod)$r.squared
glance(linear_a_mod)$adj.r.squared #lin_summ <- 
summary(linear_a_mod) #lin_a_coeff <- 
linear_a_mod$coefficients

# set up AIC table
AIC_a_end <- setNames(AIC_table[1,3:15], colnames(AIC_table[,3:15])) # x var to end
AIC_a_end[1,] <- NA
AIC_a_end$x_var <- 'protein_prop'
AIC_a_end$y_var <- 'cumul_diff'

## linear model parameters
AIC_a_end[1, ]$lin_ax <- tidy(linear_a_mod)$estimate[2]
AIC_a_end[1, ]$lin_b <- tidy(linear_a_mod)$estimate[1]
AIC_a_end[1, ]$lin_r2 <- glance(linear_a_mod)$adj.r.squared[1]

## quadratic
quad_a_mod <- lm(cumul_diff~ protein_prop + var_sq, data = phase_end_a_mod)
tidy(quad_a_mod)
glance(quad_a_mod)$r.squared #quad_summ <- 
summary(quad_a_mod) #quad_a_coeff <- 
quad_a_mod$coefficients

AIC_a_end[1,]$quad_ax2 <- tidy(quad_a_mod)$estimate[3]
AIC_a_end[1,]$quad_bx <- tidy(quad_a_mod)$estimate[2]
AIC_a_end[1,]$quad_c <- tidy(quad_a_mod)$estimate[1]
AIC_a_end[1,]$quad_r2 <- glance(quad_a_mod)$adj.r.squared[1]

# straight up AIC values
AIC_a_end[1, ]$AIC_lin <- AIC(linear_a_mod)
AIC_a_end[1, ]$AIC_quad <- AIC(quad_a_mod)
AIC_a_end$AIC_model <- if_else(AIC_a_end$AIC_lin < AIC_a_end$AIC_quad, 'linear', 'quad')
AIC_a_end$AIC_eq <- if_else(AIC_a_end$AIC_model == 'linear', 
                            paste0('y=', AIC_a_end$lin_ax,'x+',AIC_a_end$lin_b,', R^2=',AIC_a_end$lin_r2),
                            paste0('y=', AIC_a_end$quad_ax2,'x^2+',AIC_a_end$quad_bx,'x+',AIC_a_end$quad_c,', R^2=',AIC_a_end$quad_r2))

AIC_a_end

# this uses MC as a random factor
pprop_a_mod <- lmer(cumul_diff ~ protein_prop + (1|MC), data = phase_end_a_mod, REML = TRUE)
anova(pprop_a_mod)
rand(pprop_a_mod)
AIC(pprop_a_mod)

##### model testing by initial C:N content ####
p_outliers <- list(c('GG7.4', 'GU5.6', 'GG6.5', 'BG3.2'),
                   c('GG1.5'),
                   c('GU6.6'))

phase_end_N_mod <- left_join(end_p[[3]], amend_properties)
phase_end_N_mod <- phase_end_N_mod #%>% 
# filter(sampleID %!in% p_outliers[[3]])

phase_end_N_mod <- phase_end_N_mod %>% 
    mutate(var_sq = protein_prop^2)

## linear
linear_N_mod <- lm(cumul_diff~ protein_prop, data = phase_end_N_mod)
tidy(linear_N_mod)
glance(linear_N_mod)$r.squared
glance(linear_N_mod)$adj.r.squared #lin_summ <- 
summary(linear_N_mod) #lin_N_coeff <- 
linear_N_mod$coefficients

# set up AIC table
AIC_N_end <- setNames(AIC_table[1,3:15], colnames(AIC_table[,3:15])) # x var to end
AIC_N_end[1,] <- NA
AIC_N_end$x_var <- 'protein_prop'
AIC_N_end$y_var <- 'cumul_diff'

## linear model parameters
AIC_N_end[1, ]$lin_ax <- tidy(linear_N_mod)$estimate[2]
AIC_N_end[1, ]$lin_b <- tidy(linear_N_mod)$estimate[1]
AIC_N_end[1, ]$lin_r2 <- glance(linear_N_mod)$adj.r.squared[1]

## quadratic
quad_N_mod <- lm(cumul_diff~ protein_prop + var_sq, data = phase_end_N_mod)
tidy(quad_N_mod)
glance(quad_N_mod)$r.squared #quad_summ <- 
summary(quad_N_mod) #quad_N_coeff <- 
quad_N_mod$coefficients

AIC_N_end[1,]$quad_ax2 <- tidy(quad_N_mod)$estimate[3]
AIC_N_end[1,]$quad_bx <- tidy(quad_N_mod)$estimate[2]
AIC_N_end[1,]$quad_c <- tidy(quad_N_mod)$estimate[1]
AIC_N_end[1,]$quad_r2 <- glance(quad_N_mod)$adj.r.squared[1]

# straight up AIC values
AIC_N_end[1, ]$AIC_lin <- AIC(linear_N_mod)
AIC_N_end[1, ]$AIC_quad <- AIC(quad_N_mod)
AIC_N_end$AIC_model <- if_else(AIC_N_end$AIC_lin < AIC_N_end$AIC_quad, 'linear', 'quad')
AIC_N_end$AIC_eq <- if_else(AIC_N_end$AIC_model == 'linear', 
                            paste0('y=', AIC_N_end$lin_ax,'x+',AIC_N_end$lin_b,', R^2=',AIC_N_end$lin_r2),
                            paste0('y=', AIC_N_end$quad_ax2,'x^2+',AIC_N_end$quad_bx,'x+',AIC_N_end$quad_c,', R^2=',AIC_N_end$quad_r2))

AIC_N_end

# this uses MC as a random factor
pprop_N_mod <- lmer(cumul_diff ~ protein_prop + (1|MC), data = phase_end_N_mod, REML = TRUE)
anova(pprop_N_mod)
rand(pprop_N_mod)
AIC(pprop_N_mod)

##### PHASE 1:graphing data from all MCs ####
formula_lin <- y ~ x
formula_quad <- y ~ poly(x, 2)

graph_phase <- 1

p_outliers <- list(c('GG7.4', 'GU5.6', 'GG6.5', 'BG3.2'),
                   c('GG1.5'),
                   c('GU6.6'))

cbPalette <- viridis(10)


for (graph_phase in phase_list){
    
    phase_end <- left_join(end_p[[graph_phase]], amend_properties)
    phase_end <- phase_end %>% 
        filter(sampleID %!in% p_outliers[[graph_phase]]) %>% 
        left_join(AIC_table)
    # treatment != 'C')
    
    # phase_end$protein_prop <- as.numeric(phase_end$protein_prop)
    phase_end$treatment <- factor(phase_end$treatment)
    
    #### plot ALL tubes in phase 1 by final, cumulative diff values 
    phase_min <- min(phase_end$cumul_diff) - 0.1*abs(min(phase_end$cumul_diff))
    phase_max <- max(phase_end$cumul_diff) + 0.1*abs(max(phase_end$cumul_diff))
    
    gr_p_ALL <- ggplot(phase_end,
                       aes(protein_prop,
                           cumul_diff,
                           color = treatment,
                           text = sampleID)) +
        geom_point(size=1) +
        # labs(x="amendment protein proportion",
        labs(x="Protein proportion",
             y = "CO2-C mineralized",
             title = paste0("Cumulative respiration from experiment beginning to end of phase ", ## for cumul_diff
                            # title = paste0("Cumulative respiration from within phase ", ## for cumul_diff
                            graph_phase)) + 
        ylim(phase_min, phase_max) +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_grid(cols = vars(MC)) + 
        scale_color_manual(values = cbPalette) +
        stat_smooth(data = subset(phase_end, AIC_model == 'linear'),
                    method = lm,
                    formula = formula_lin,
                    aes(group = MC)) + 
        stat_smooth(data = subset(phase_end, AIC_model == 'quad'),
                    method = lm,
                    formula = formula_quad,
                    aes(group = MC)) 
    
    ggplotly(gr_p_ALL)
    if(ggsave_toggle == TRUE) ggsave(paste(here::here('results/', today()),
                                           'ALL.MC',graph_phase,'scatter_N_cumul_diff.png', sep="_"),
                                     width=13, height=2.5, dpi=600)
}

######## by MC #######
which_MC <- 'GU'

graph_phase <- 3
min_y <- -1003.868
max_y <- 3077.306

p_outliers <- list(c('GG7.4', 'GU5.6', 'GG6.5', 'BG3.2'),
                   c('GG1.5'),
                   c('GU6.6'))

phase_end <- left_join(end_p[[graph_phase]], amend_properties)
phase_end <- phase_end %>%
    filter(MC == which_MC,
           sampleID %!in% p_outliers[[graph_phase]])
# phase_end$protein_prop <- as.numeric(phase_end$protein_prop)
phase_end$treatment <- factor(phase_end$treatment)

#### plot ALL tubes in phase 1 by final, cumulative diff values
phase_min <- min(phase_end$cumul_diff) - 0.1*abs(min(phase_end$cumul_diff))
phase_max <- max(phase_end$cumul_diff) + 0.1*abs(max(phase_end$cumul_diff))

protein_key <- phase_end %>% 
    ungroup() %>% 
    select(treatment, protein_prop) %>% 
    unique()

phase_end_summ <- phase_end %>% 
    group_by(treatment) %>% 
    summarize(mean_trt_diff = mean(cumul_diff),
              se_trt_diff = se(cumul_diff),
              phase = median(phase)) %>% 
    mutate(MC = which_MC) %>% 
    left_join(AIC_table) %>% 
    left_join(protein_key)

gr_p1 <- ggplot(phase_end_summ,
                aes(protein_prop,
                    mean_trt_diff,
                    color = treatment)) +
    geom_point(size=2) +
    ylim(min_y, max_y) +
    labs(x="Protein proportion",
         y = "CO2-C mineralized",
         title = paste0("Cumulative respiration at end of experiment by treatment for ", which_MC)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    # facet_grid(cols = vars(MC)) +
    scale_color_manual(values = cbPalette) +
    stat_smooth(data = subset(phase_end_summ, AIC_model == 'linear'),
                method = lm,
                formula = formula_lin,
                aes(group = MC)) + 
    stat_smooth(data = subset(phase_end_summ, AIC_model == 'quad'),
                method = lm,
                formula = formula_quad,
                aes(group = MC)) 

ggplotly(gr_p1)
if(ggsave_toggle == TRUE) ggsave(paste(here::here('results/', today()),
                                       which_MC,graph_phase,'scatter_N_cumul_diff_at.end.png', sep="_"),
                                 width=10, height=8, dpi=600)
