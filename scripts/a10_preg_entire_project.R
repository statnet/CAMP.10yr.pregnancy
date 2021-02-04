
########################################################################
## Code for manuscript "XXXXX"

#setwd("C:/git/CAMP_10yr_pregnancy/scripts/")  # Change depending on machine
setwd("H:/_goodreau/git/CAMP.10yr.pregnancy/scripts/")  # Change depending on machine

#install.packages("EasyABC")
library(EasyABC)
library(MASS)
library(dplyr)
library(tidyverse)
library(magrittr)
library(nnet)

rm(list=ls())
set.seed(1)

########################################################################
### Common tasks across scenarios

source("a10_preg_backcalc_logistic.R")
source("a10_import.R")                          # Get all inputs
source("a10_process_inputs_all_but_bctypes.R")  # Process inputs (i.e. conduct regressions, etc.)
source("a10_process_inputs_bctypes.R")          # Process inputs (i.e. conduct regressions, etc.)
source("a10_reassign_bctypes.R")                # Move bc methods from input types to standardized types
source("a10_impute_even_years.R")               # Impute even years
source("a10_make_behav_inputs_all_2007.R")      # override 2009-2017 numbers with 2007 for both calibration and no-behavior-change models
source("a10_make_onlyLARC_scenarios.R")         # These differ from other types of scenarios and need to be made after previous two steps]

saveRDS(bctype_in_wts, file="../output/a10_bctype_in_wts.rda")

########################################################################
### # Scenario 1: No LARC prior to 2013 (minLARC)
source("a10_ABC_minLARC.R")
source("a10_calibration_minLARC.R")                  # calib pt 1
source("a10_no_behav_change_minLARC.R")              # No behavior change
source("a10_obs_behav_change_minLARC.R")             # Observed behavior change
source("a10_obs_contraception_change_minLARC.R")     # Observed contraception change only
source("a10_obs_sexual_activity_change_minLARC.R")   # Observed debut / partner numbers only
source("a10_obs_debut_change_minLARC.R")             # Observed debut only
source("a10_obs_mnppy_change_minLARC.R")             # Observed partner numbers only

saveRDS(a10_calib_minLARC, file = "../output/a10_calib_minLARC.rda")
saveRDS(prob_detpreg_minLARC, file="../output/prob_detpreg_minLARC.rda")
saveRDS(a10_nbc_minLARC, file="../output/a10_nbc_minLARC.rda")
saveRDS(a10_obs_minLARC, file='../output/a10_obs_minLARC.rda')
saveRDS(a10_obs_cc_minLARC, file='../output/a10_obs_cc_minLARC.rda')
saveRDS(a10_obs_sex_minLARC, file='../output/a10_obs_sex_minLARC.rda')
saveRDS(a10_obs_debut_minLARC, file='../output/a10_obs_debut_minLARC.rda')
saveRDS(a10_obs_mnppy_minLARC, file='../output/a10_obs_mnppy_minLARC.rda')

########################################################################
### # Scenario 2: Maximum LARC possible given responses from 2009 on (maxLARC)

source("a10_ABC_maxLARC.R")
source("a10_calibration_maxLARC.R")                  # calib pt 1
source("a10_no_behav_change_maxLARC.R")              # No behavior change
source("a10_obs_behav_change_maxLARC.R")             # Observed behavior change
source("a10_obs_contraception_change_maxLARC.R")     # Observed contraception change only
source("a10_obs_sexual_activity_change_maxLARC.R")   # Observed debut / partner numbers only
source("a10_obs_debut_change_maxLARC.R")             # Observed debut only
source("a10_obs_mnppy_change_maxLARC.R")             # Observed partner numbers only

saveRDS(a10_calib_maxLARC, file = "../output/a10_calib_maxLARC.rda")
saveRDS(prob_detpreg_maxLARC, file="../output/prob_detpreg_maxLARC.rda")
saveRDS(a10_nbc_maxLARC, file='../output/a10_nbc_maxLARC.rda')
saveRDS(a10_obs_maxLARC, file='../output/a10_obs_maxLARC.rda')
saveRDS(a10_obs_cc_maxLARC, file='../output/a10_obs_cc_maxLARC.rda')
saveRDS(a10_obs_sex_maxLARC, file='../output/a10_obs_sex_maxLARC.rda')
saveRDS(a10_obs_debut_maxLARC, file='../output/a10_obs_debut_maxLARC.rda')
saveRDS(a10_obs_mnppy_maxLARC, file='../output/a10_obs_mnppy_maxLARC.rda')

########################################################################
### # Scenario 3: Only LARC changes
source("a10_obs_contraception_change_onlyLARC_from_wdl.R")    # Observed behavior change
source("a10_obs_contraception_change_onlyLARC_from_cdm.R")    # Observed behavior change

saveRDS(a10_obs_cc_onlyLARC_maxL_from_wdl, file='../output/a10_obs_cc_onlyLARC_maxL_from_wdl.rda')
saveRDS(a10_obs_cc_onlyLARC_minL_from_wdl, file='../output/a10_obs_cc_onlyLARC_minL_from_wdl.rda')
saveRDS(a10_obs_cc_onlyLARC_maxL_from_cdm, file='../output/a10_obs_cc_onlyLARC_maxL_from_cdm.rda')
saveRDS(a10_obs_cc_onlyLARC_minL_from_cdm, file='../output/a10_obs_cc_onlyLARC_minL_from_cdm.rda')


########################################################################
### # Scenario X: Additional coital freq decline within partnerships
source("a10_coital_decline.R")

########################################################################
source("a10_credible_intervals.R")           # Observed behavior change

########################################################
## Generate results for paper 

#source("a10_results.R")

