#### lab macronuts ####
library(lubridate)
switch_switch <- 1 # switch_switch = 1 means switched
folder_date <- gsub("-", ".",today())
outlier_bool <- TRUE # false = not removed

#### all code ####
source(here::here('src/0_exp-1-fxns.R'))
source(here::here('src/1_clean-phase-1-data.R'))
source(here::here('src/1_clean-phase-2-data.R'))
source(here::here('src/1_clean-phase-3-data.R'))
# switching of labels found here vvvvvv
source(here::here('src/2_merge-phase-data.R'))
#outlier removal found here vvvv
source(here::here('src/3_analyze-triphase.R'))
source(here::here('src/4_generate-plotvals.R'))
# source(here::here('src/5_visualize-triphase-tube.R'))
# source(here::here('src/5_visualize-triphase-treatment.R'))
# source(here::here('src/6_end-stats.R'))
