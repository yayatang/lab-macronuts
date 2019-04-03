#  for visualizing data
library(viridis)
library(plotly)
library(tidyverse)
library(here)

switch_switch <- 1 # 1 is switched
if (switch_switch == 0) switch_file <- 'unswitched' else switch_file <- 'switched'

imported_data <- read.csv(paste0(here::here('results/tubes_to_plot_'),switch_file,'.csv'))

max_p1 <- max(filter(imported_data, phase == 1)$exp_count)
max_p2 <- max(filter(imported_data, phase == 2)$exp_count)

graph_data <- imported_data %>% 
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

mcs <- c('BU', 'BG', 'GU', 'GG')
# i <- 4
# mc_filt <- 'BU'
for (i in seq_along(var_to_graph)){
    # i is the type of graph, according to the titles above
    print(i)
    
    minmax_data <- graph_data %>%
        select(!!dynamic_data$var_to_graph[[i]], 
               !!dynamic_data$se_to_graph[[i]]) %>% #, !!dynamic_data$graph_group[[i]])
        rename(vals = !!dynamic_data$var_to_graph[[i]],
               all_se = !!dynamic_data$se_to_graph[[i]])
    min_y_val <- minmax_data[which(minmax_data$vals == min(minmax_data$vals)),]$vals
    min_y <- if(min_y_val<0)min_y_val * 1.1 else min_y_val*0.9
    
    max_y_val <- minmax_data[which(minmax_data$vals == max(minmax_data$vals)),]$vals
    max_y <- if(max_y_val<0)max_y_val * 0.9 else max_y_val*1.1
    
    for (j in seq_along(mcs)){
        mc_filt <- mcs[[j]]
        print(mc_filt)
        
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
        
        cbPalette <- viridis(10)
        
        any_plot <- ggplot(plot_data, aes(exp_count, graph_yvar, color=Treatment, group=sampleID)) + 
            ylim(min_y, max_y) +
            geom_line(aes(group=sampleID)) +
            geom_vline(xintercept=max_p1, color="grey", size = 0.3) +
            geom_vline(xintercept=max_p2, color="grey", size = 0.3) +
            geom_hline(yintercept=0) +
            geom_point(size=0.5) +
            geom_errorbar(aes(ymin = graph_yvar - graph_se,ymax = graph_yvar + graph_se), width=0.3) +
            labs(x="Experimental days lapsed", y=dynamic_data$y_titles[[i]]) +
            ggtitle(paste(dynamic_data$plot_titles[[i]], mc_filt)) +
            scale_color_manual(values = cbPalette) +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black")) 
        
        # any_plot
        p <- ggplotly(any_plot)
        # p
        htmlwidgets::saveWidget(as_widget(p), paste(switch_file, i, var_to_graph[i], mc_filt, "by.tube.html", sep="_"))
        
        ggsave(paste(switch_file, i,var_to_graph[i],'by.tube', mc_filt, '.png', sep="_"), width=10, height=8, dpi=600)
    }
}