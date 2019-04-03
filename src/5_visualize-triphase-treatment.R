#  for visualizing data of treatment data
library(viridis)
library(plotly)
library(tidyverse)

imported_data <- read.csv(here::here('results/trts_to_plot.csv'))

max_p1 <- max(filter(imported_data, phase == 1)$exp_count)
max_p2 <- max(filter(imported_data, phase == 2)$exp_count)

# DAILY GRAPHS
# Loop for graphing cumulative results by MC---------

graph_data <- imported_data %>% 
    filter(treatment!='R')
# graph_data <- graph_data[,
# replace standard error values in rows where interped = TRUE

var_to_graph <- c('trt_gross_daily',
                  'trt_diff_daily',
                  'trt_gross_cumul',
                  'trt_diff_cumul')
se_to_graph <- c(rep(c('trt_se_daily'), 2),
                 rep(c('trt_se_cumul'), 2))
# graph_group <- rep(c('MC'),4)
y_titles <- c(rep(c('Daily CO2-C'),2), rep('Cumulative CO2-C',2))
plot_titles <- c('Daily CO2 gross production by treatment',
                 'Daily CO2 difference from control by treatment',
                 'Cumulative gross CO2 total by treatment',
                 'Cumulative CO2 difference from control by treatment')
dynamic_data <- tibble(var_to_graph, se_to_graph, y_titles, plot_titles)

# graph_data
#   exp_count
#   var to graph
#   treatment
#   se var
#   y axis title
#   graph overall title
#   
#   1 = daily gross by treatment
#   2 = daily diff by treatment
#   3 = cumul gross by treatment
#   4 = cumul diff by treatment

mcs <- c('BU', 'BG', 'GU', 'GG')

for (i in seq_along(var_to_graph)){
    # i is the type of graph, according to the titles above
    print(i)

    minmax_data <- graph_data %>%
        select(trt_ID, exp_count, phase, !!dynamic_data$var_to_graph[[i]], 
               !!dynamic_data$se_to_graph[[i]]) %>% #, !!dynamic_data$graph_group[[i]])
        rename(vals = !!dynamic_data$var_to_graph[[i]],
               all_se = !!dynamic_data$se_to_graph[[i]])
    # mutate(min_sum = vals - all_se)
    min_y_val <- minmax_data[which(minmax_data$vals == min(minmax_data$vals)),]$vals
    min_y <- if(min_y_val<0)min_y_val * 1.1 else min_y_val*0.9
    
    max_y_val <- minmax_data[which(minmax_data$vals == max(minmax_data$vals)),]$vals
    max_y <- if(max_y_val<0)max_y_val * 0.9 else max_y_val*1.1
    
    for (j in seq_along(mcs)){
        mc_filt <- mcs[[j]]
        print(mc_filt)
        
        selected_data <- graph_data %>%
            filter(MC == mc_filt) %>%
            select(trt_ID, exp_count, phase, !!dynamic_data$var_to_graph[[i]], 
                   !!dynamic_data$se_to_graph[[i]]) #, !!dynamic_data$graph_group[[i]])
        renamed_data <- selected_data %>% 
            rename(graph_yvar = !!dynamic_data$var_to_graph[[i]],
                   graph_se = !!dynamic_data$se_to_graph[[i]])
        
        plot_data <- renamed_data %>% 
            ungroup() %>% 
            rename(Treatment = trt_ID)
        
        any_plot <- ggplot(plot_data, aes(exp_count, graph_yvar, color=Treatment, group=Treatment)) + 
            ylim(min_y, max_y) +
            geom_line(aes(group=Treatment)) +
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
        htmlwidgets::saveWidget(as_widget(p), paste(i,var_to_graph[i], mc_filt, ".html", sep="_"))
        
        ggsave(paste0('results/',i,'_by.trt_', mc_filt, '.png'), width=10, height=8, dpi=600)
    }
}