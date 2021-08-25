#7.1 overall model for level 1 of results

library(tidyverse)
library(ggpubr)
library(lme4)
library(gridExtra) # for grid.arrange fxn, can remove if not using

save_toggle <- TRUE

if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'
outlier_name <- if_else(outlier_bool == TRUE, 'WITH_outliers_', 'outliers_removed_') 

end_data <- read_rds(paste0(here::here('results/6_end_data_rows_'), 
                            outlier_name, switch_file, '.rds')) %>%
    filter(treatment != 'R') %>% 
    select(-interped) %>% 
    mutate(protein2 = protein_prop ^2)

debug_contr_error <- function (dat, subset_vec = NULL) {
    if (!is.null(subset_vec)) {
        ## step 0
        if (mode(subset_vec) == "logical") {
            if (length(subset_vec) != nrow(dat)) {
                stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
            }
            subset_log_vec <- subset_vec
        } else if (mode(subset_vec) == "numeric") {
            ## check range
            ran <- range(subset_vec)
            if (ran[1] < 1 || ran[2] > nrow(dat)) {
                stop("'numeric' `subset_vec` provided but values are out of bound")
            } else {
                subset_log_vec <- logical(nrow(dat))
                subset_log_vec[as.integer(subset_vec)] <- TRUE
            } 
        } else {
            stop("`subset_vec` must be either 'logical' or 'numeric'")
        }
        dat <- base::subset(dat, subset = subset_log_vec)
    } else {
        ## step 1
        dat <- stats::na.omit(dat)
    }
    if (nrow(dat) == 0L) warning("no complete cases")
    ## step 2
    var_mode <- sapply(dat, mode)
    if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
    var_class <- sapply(dat, class)
    if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
        stop("matrix variables with 'AsIs' class must be 'numeric'")
    }
    ind1 <- which(var_mode %in% c("logical", "character"))
    dat[ind1] <- lapply(dat[ind1], as.factor)
    ## step 3
    fctr <- which(sapply(dat, is.factor))
    if (length(fctr) == 0L) warning("no factor variables to summary")
    ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
    dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
    ## step 4
    lev <- lapply(dat[fctr], base::levels.default)
    nl <- lengths(lev)
    ## return
    list(nlevels = nl, levels = lev)
}

#### basic linear model ####

end_data_p1 <- filter(end_data, phase == 1)
end_data_p2 <- filter(end_data, phase == 2)
end_data_p3 <- filter(end_data, phase == 3)

#######

formula_intercept <- 'cumul_gross ~ grazing + protein_prop + (1|location)'
# # example model including random slopes
formula_slopes <- 'cumul_gross ~ grazing + protein_prop + (1+protein_prop|location)'

my_formula <- formula_intercept

model_cumul.p1 <- lmer(my_formula, end_data_p1)
model_cumul.p2 <- lmer(my_formula, end_data_p2)
model_cumul.p3 <- lmer(my_formula, end_data_p3)

my_model <- model_cumul.p3
summary(my_model)
plot(my_model)
shapiro.test(end_data_p3$cumul_gross)
# # qqnorm(end_data_p3$cumul_gross)
# qqline(end_data_p3$cumul_gross)
ggqqplot(residuals(my_model))

###### # mixed effect model ANOVA comparison
# comparing to the linear model
model_null1 <- lmer(cumul_gross ~ grazing + (1|location), 
                    end_data_p3)
anova(model_null1, my_model) 
# # significant if over the TOTAL cumulative gross
# # NOT significant if you look at only the cumulative respiration during phase 3
# # still significant for phase_gross in phase 2

######
# model_form1.quad <- lmer(cumul_gross ~ grazing + protein2 + protein_prop + (1|location), end_data_p3)
model_form1.quad <- lmer(cumul_gross ~ grazing + I(protein_prop^2) + protein_prop + (1|location), end_data_p3)
# viraj suggested needing to include the linear protein_prop term
summary(model_form1.quad)

anova(my_model, model_form1.quad)
anova(model_null1, model_form1.quad)






####################################
# ## code below isn't included in my analyses
# 
# # model with singular fit issue
# model_form2 <- lmer(formula_slopes, end_data_p3)
# summary(model_form2)
# # plot(model_form2)
# anova(my_model, model_form2)
# 
# # not sure what this means when considering mixed models...
# model_null1.2 <- lmer(cumul_gross ~ (1|location),
#                       filter(end_data, phase == 3))
# anova(model_null1.2, model_null1)
# 
# #for form 2 -- OVERFITTED
# # random slopes, without a ton more data, is not a better/more parsimonious fit
# model_null2 <- lmer(cumul_gross ~ grazing + (1+protein_prop|location), 
#                     end_data_p3, 
#                     REML = FALSE)
# anova(model_null2, model_form2)