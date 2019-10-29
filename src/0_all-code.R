source(here::here('src/1_clean-phase-1-data.R'))
source(here::here('src/1_clean-phase-2-data.R'))
source(here::here('src/1_clean-phase-3-data.R'))

# 1 is switched, needed for next scripts
switch_switch <- 0

source(here::here('src/2_merge-phase-data.R'))
source(here::here('src/3_analyze-triphase.R'))
source(here::here('src/4_generate-plotvals.R'))
source(here::here('src/5_visualize-triphase-treatment.R'))
