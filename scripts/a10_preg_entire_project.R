
########################################################################
## Code for manuscript:
##   Goodreau SM, Pollock ED, Wang LY, Li J, Aslam MV, Katz DA, Hamilton DT,
##   Rosenberg ES. 2022. Declines in pregnancies among US adolescents from
##   2007 to 2017: behavioral contributors to the trend, Journal of Pediatric
##   and Adolescent Gynecology, doi: https://doi.org/10.1016/j.jpag.2022.07.008

setwd("CAMP.10yr.pregnancy/scripts/")  # Change depending on machine

## Install any needed packages
#install.packages("EasyABC")
#install.packages("MASS")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("magrittr")
#install.packages("nnet")

## Load packages
library(EasyABC)
library(MASS)
library(dplyr)
library(tidyverse)
library(magrittr)
library(nnet)

## Prepare environment
rm(list=ls())
set.seed(1)

########################################################################
### Common tasks across scenarios

source("a10_preg_backcalc_logistic.R")
source("a10_import.R")                          # Get all inputs
source("a10_process_inputs_popsizes.R")         # Process inputs (i.e. conduct regressions, etc.)
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
### # Scenario 3: Linear increases in LARC use prior to 2013 (medLARC)

source("a10_ABC_medLARC.R")
source("a10_calibration_medLARC.R")                  # calib pt 1
source("a10_no_behav_change_medLARC.R")              # No behavior change
source("a10_obs_behav_change_medLARC.R")             # Observed behavior change
source("a10_obs_contraception_change_medLARC.R")     # Observed contraception change only
source("a10_obs_sexual_activity_change_medLARC.R")   # Observed debut / partner numbers only
source("a10_obs_debut_change_medLARC.R")             # Observed debut only
source("a10_obs_mnppy_change_medLARC.R")             # Observed partner numbers only

saveRDS(a10_calib_medLARC, file = "../output/a10_calib_medLARC.rda")
saveRDS(prob_detpreg_medLARC, file="../output/prob_detpreg_medLARC.rda")
saveRDS(a10_nbc_medLARC, file='../output/a10_nbc_medLARC.rda')
saveRDS(a10_obs_medLARC, file='../output/a10_obs_medLARC.rda')
saveRDS(a10_obs_cc_medLARC, file='../output/a10_obs_cc_medLARC.rda')
saveRDS(a10_obs_sex_medLARC, file='../output/a10_obs_sex_medLARC.rda')
saveRDS(a10_obs_debut_medLARC, file='../output/a10_obs_debut_medLARC.rda')
saveRDS(a10_obs_mnppy_medLARC, file='../output/a10_obs_mnppy_medLARC.rda')

########################################################################
### # Scenario 4: Only LARC changes

source("a10_obs_contraception_change_onlyLARC_from_all.R")
source("a10_obs_contraception_change_onlyLARC_from_wdl.R")
source("a10_obs_contraception_change_onlyLARC_from_cdm.R")
source("a10_obs_contraception_change_onlyLARC_from_pil.R")

saveRDS(a10_obs_cc_onlyLARC_maxL_from_all, file='../output/a10_obs_cc_onlyLARC_maxL_from_all.rda')
saveRDS(a10_obs_cc_onlyLARC_minL_from_all, file='../output/a10_obs_cc_onlyLARC_minL_from_all.rda')
saveRDS(a10_obs_cc_onlyLARC_medL_from_all, file='../output/a10_obs_cc_onlyLARC_medL_from_all.rda')
saveRDS(a10_obs_cc_onlyLARC_maxL_from_wdl, file='../output/a10_obs_cc_onlyLARC_maxL_from_wdl.rda')
saveRDS(a10_obs_cc_onlyLARC_minL_from_wdl, file='../output/a10_obs_cc_onlyLARC_minL_from_wdl.rda')
saveRDS(a10_obs_cc_onlyLARC_medL_from_wdl, file='../output/a10_obs_cc_onlyLARC_medL_from_wdl.rda')
saveRDS(a10_obs_cc_onlyLARC_maxL_from_cdm, file='../output/a10_obs_cc_onlyLARC_maxL_from_cdm.rda')
saveRDS(a10_obs_cc_onlyLARC_minL_from_cdm, file='../output/a10_obs_cc_onlyLARC_minL_from_cdm.rda')
saveRDS(a10_obs_cc_onlyLARC_medL_from_cdm, file='../output/a10_obs_cc_onlyLARC_medL_from_cdm.rda')
saveRDS(a10_obs_cc_onlyLARC_maxL_from_pil, file='../output/a10_obs_cc_onlyLARC_maxL_from_pil.rda')
saveRDS(a10_obs_cc_onlyLARC_minL_from_pil, file='../output/a10_obs_cc_onlyLARC_minL_from_pil.rda')
saveRDS(a10_obs_cc_onlyLARC_medL_from_pil, file='../output/a10_obs_cc_onlyLARC_medL_from_pil.rda')

########################################################################
### # Scenario 5: Additional coital freq decline within partnerships
source("a10_coital_decline.R")

########################################################################
### Create credible intervals
source("a10_credible_intervals.R")

########################################################
## Generate results for paper
source("a10_results.R")

