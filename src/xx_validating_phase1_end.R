check_diff <- function(group1, group2){
    pval <- t.test(group1, group2)$p.value
    return(pval)
}


diff_data <- by_tube
mc.vec <- unique(diff_data$MC)
# for each MC, through all treatments

pval_vec <- data.frame(row.names=mc.vec)
trt_vec <- as.character(unique(diff_data$treatment))
trt_vec <- sort(trt_vec)

pval_mat <- as.data.frame(matrix(nrow=length(mc_vec), ncol=length(trt_vec)))
names(pval_mat) <- trt_vec

day_incub <- 40
for (i in 1:length(mc_vec)){
    ref <- diff_data %>% 
        ungroup() %>% 
        filter(MC==mc_vec[[i]], treatment=='R', exp_count==day_incub) %>% 
        select(infer_samp_perday)
    
    for (j in 1:length(trt_vec)){
        group <- diff_data %>% 
            ungroup() %>% 
            filter(MC==mc_vec[[i]], treatment==trt_vec[[j]], exp_count==day_incub) %>% 
            select(infer_samp_perday)
        
        pval_mat[i,j] <- check_diff(ref, group)
    }
}

pval_mat[pval_mat$value<=0.05,]


