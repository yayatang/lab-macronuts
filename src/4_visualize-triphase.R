#  for visualizing data

trt_vec <- sort(as.character(unique(data4_diff$treatment)))
MC_vec <- as.character(unique(data4_diff$MC))


# Generate error bars for sampling days  [errorbar_cumul_C, errorbar_diff_C]-----

# these are the only days that get real bars, since these were true data days
errorbar_daily_C <- merge(irga_days, daily_summ, all.x=TRUE)
errorbar_cumul_C <- merge(irga_days, cumul_all_summ, all.x=TRUE)

# Loop for graphing cumulative results by MC---------

u_ymin <- -400 # irga 3-03:  -800/-60
u_ymax <- 1200 # irga 3-03: 2000/150

for (i in 1:length(MC_vec)){
    # (un)comment the appropriate lines to graph waht you want
    # i=4
    
    sub_data <- cumul_all_summ %>% filter(MC==MC_vec[i] & treatment!='C' & treatment!='R' ) %>%
        rename(plot_vals = mean_reps)
    
    sub_data$treatment <- as.factor(sub_data$treatment)
    
    mc_plots <- ggplot(sub_data, aes(exp_count, plot_vals, ymin=u_ymin, ymax=u_ymax)) +
        scale_fill_continuous(guide = "colorbar") +
        scale_fill_hue(l=40) +
        facet_grid(~phase, scales="free") +
        geom_line(aes(group=treatment), size=1) +
        geom_point(size=0.5) +
        geom_errorbar(aes(ymin=plot_vals-se_reps, ymax=plot_vals+se_reps), width=0.3) +
        geom_hline(yintercept=0) +
        labs(x="Day", y="Cumulative CO2-C") +
        ggtitle(paste('Cumulative CO2-C in MC', MC_vec[i]))
    
    ggsave(paste0('by_MC-', MC_vec[i], '.pdf'), width=10, height=7.5, dpi=400)
    print(mc_plots)
    
    # # used to find universal max for chart
    # max_temp <- max(sub_data$plot_vals+sub_data$se_reps, na.rm=T)
    # print(max_temp)
    # min_temp <- min(sub_data$plot_vals-sub_data$se_reps, na.rm=T)
    # print(min_temp)
}
