#  for visualizing data
# install.packages("viridis")
library(viridis)
library(plotly)

irga_days <- read.csv(here::here('results/irga_days.csv'))
plot_data <- read.csv(here::here('results/tubes_to_plot.csv'))

cumul_c <- plot_data %>% 
    filter(treatment == 'C') %>% 
    rename(daily_C_gross = infer_samp_perday,
           cumul_C_gross = cumul_gross,
           cumul_C_phase = cumul_phase_gross) %>% 
    select(sampleID, trt_ID, MC, treatment, exp_count, daily_C_gross, cumul_C_gross, cumul_C_phase)

max_p1 <- max(filter(plot_data, phase == 1)$exp_count)
max_p2 <- max(filter(plot_data, phase == 2)$exp_count)

# DAILY GRAPHS
# Loop for graphing cumulative results by MC---------

graph_data <- plot_data %>% 
    filter(treatment!='R')

var_to_graph <- c('infer_samp_perday',
                  'infer_diff_perday',
                  'cumul_gross',
                  'cumul_diff',
                  'cumul_phase_gross',
                  'cumul_phase_diff')
se_to_graph <- rep(c('tube_se'),6)
# graph_group <- rep(c('sampleID'),6)
y_titles <- c(rep(c('Daily CO2-C'),2), rep('Cumulative CO2-C',4))
plot_titles <- c('Daily CO2 gross production by tube',
                 'Daily CO2 difference from control by tube',
                 'Cumulative gross CO2 total by tube',
                 'Cumulative CO2 difference from control by tube',
                 'Cumulative CO2 production by phase',
                 'Cumulative CO2 production difference from control')
dynamic_data <- tibble(var_to_graph, se_to_graph, y_titles, plot_titles)

# graph_data
#   exp_count
#   var to graph
#   sampleID
#   se var
#   y axis title
#   graph overall title
#   
#   1 = daily gross by tube
#   2 = daily diff by tube
#   3 = cumul gross by tube
#   4 = cumul diff by tube
#   5 = cumul phase gross by tube
#   6 = cumul phase diff by tube

# for (i in seq_along(length(dynamic_data))){

# i is the type of graph, according to the titles above
i <- 6
mc_filt <- 'GG'
selected_data <- graph_data %>%
    filter(MC == mc_filt) %>%
    select(sampleID, exp_count, phase, trt_ID, !!dynamic_data$var_to_graph[[i]], 
           !!dynamic_data$se_to_graph[[i]])
renamed_data <- selected_data %>% 
    rename(graph_yvar = !!dynamic_data$var_to_graph[[i]],
           graph_se = !!dynamic_data$se_to_graph[[i]])

plot_data <- renamed_data %>% 
    ungroup() %>% 
    rename(Treatment = trt_ID)

any_plot <- ggplot(plot_data, aes(exp_count, graph_yvar, color=Treatment, group=sampleID)) + 
    geom_line() +
    geom_vline(xintercept=max_p1, color="grey", size = 0.3) +
    geom_vline(xintercept=max_p2, color="grey", size = 0.3) +
    geom_hline(yintercept=0) +
    geom_point(size=0.5) +
    geom_errorbar(aes(ymin = graph_yvar - graph_se,ymax = graph_yvar + graph_se), width=0.3) +
    labs(x="Experimental days lapsed", y=dynamic_data$y_titles[[i]]) +
    ggtitle(paste(dynamic_data$plot_titles[[i]]))+
    scale_fill_viridis() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) 

# any_plot
p <- ggplotly(any_plot)
htmlwidgets::saveWidget(as_widget(p), paste0(i, mc_filt, "_index.html"))

# ggsave(paste0('results/',i,'_by_', dynamic_data$graph_group[i], '.png'), width=10, height=8, dpi=600)
ggsave(paste0('results/',i,'_by_', mc_filt, '.png'), width=10, height=8, dpi=600)
# }
