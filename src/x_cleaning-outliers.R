# messing around with the lists I have
out1 <- read_csv(here::here('data/tube outliers to remove1.csv'))
out2 <- read_csv(here::here('data/tube outliers to remove2.csv'))
out3 <- read_csv(here::here('data/tube outliers to remove3.csv'))
colnames(out1) <- c('out', 'maybe', 'comments')
colnames(out2) <- c('out', 'maybe', 'comments')
colnames(out3) <- c('out', 'maybe', 'comments')

common_out1 <- inner_join(out1, out2) 
common_out2 <- inner_join(out1, out3)
common_out3 <- inner_join(out2, out3)

anti_out1 <- anti_join(out1, out2)
anti_out2 <- anti_join(out1, out3)
anti_out3 <- anti_join(out2, out3)


# drafting the function to remove outliers
remove_outliers <- function(tubes_data) {
    # remove this section once the function runs
    library(tidyverse)
    switch_file <- 'switched'
    tubes_data <- read.csv(paste0(here::here('results/tubes_to_plot_'),switch_file,'.csv'))
    
    out_tubes <- read_csv(here::here('data/tube_outliers.csv'))
    colnames(out_tubes) <- c('sampleID', 'exp_count')
    
    # first take care of individual points that are poor samples
    out_points <- out_tubes %>% 
        filter(exp_count != 'all') %>% 
        mutate(exp_count = as.numeric(exp_count))
    
    tubes_data2 <- anti_join(tubes_data, out_points)
    
    deleted_tubes <- anti_join(tubes_data, tubes_data2)    
    
    # second, remove individual tubes that are extra crazy
    
}
