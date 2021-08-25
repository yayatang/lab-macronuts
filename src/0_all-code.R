#### lab macronuts ####
library(lubridate)
switch_switch <- 1 # switch_switch = 1 means switched
folder_date <- gsub("-", ".",today())
outlier_bool <- FALSE # false = outliers are removed

source(here::here('src/0_exp-1-fxns.R'))

#### code ####
#### skip these if you just want to re-process the data ####
source(here::here('src/1_clean-phase-1-data.R'))
source(here::here('src/1_clean-phase-2-data.R'))
source(here::here('src/1_clean-phase-3-data.R'))
# switching of labels found here vvvvvv
source(here::here('src/2_merge-phase-data.R'))

#### outlier removal found here vvvv ####
source(here::here('src/3_analyze-triphase.R'))
source(here::here('src/4_generate-plotvals.R'))

#### skip these two if you want to go straight to analysis ####
source(here::here('src/5_visualize-triphase-tube.R'))
source(here::here('src/5_visualize-triphase-treatment.R'))
source(here::here('src/6.1_end-data.R'))
source(here::here('src/6.2_end-stats.R'))
# source(here::here('src/7_analysis-graphs.R'))
source(here::here('src/7.1_overall-model.R'))
source(here::here('src/7.2_model-comparison.R'))
source(here::here('src/7.3_graph-temporal.R'))