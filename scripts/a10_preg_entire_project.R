
########################################################################
## Code for manuscript "XXXXX"

setwd("C:/git/CAMP_10yr_pregnancy/scripts/")  # Change depending on machine
#setwd("C:/git/CAMP.10yr.pregnancy/scripts/")  # Change depending on machine
rm(list=ls())

#install.packages("EasyABC")
library(EasyABC)
library(MASS)
library(dplyr)
library(tidyverse)
library(magrittr)
library(nnet)

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

########################################################################
### # Scenario 2: Maximum LARC possible given responses from 2009 on (maxLARC)

#source("a10_ABC_maxLARC.R")
source("a10_calibration_maxLARC.R")                  # calib pt 1
source("a10_no_behav_change_maxLARC.R")              # No behavior change
source("a10_obs_behav_change_maxLARC.R")             # Observed behavior change
source("a10_obs_contraception_change_maxLARC.R")     # Observed contraception change only
source("a10_obs_sexual_activity_change_maxLARC.R")   # Observed debut / partner numbers only
source("a10_obs_debut_change_maxLARC.R")             # Observed debut only
source("a10_obs_mnppy_change_maxLARC.R")             # Observed partner numbers only

########################################################################
#source("a10_credible_intervals.R")           # Observed behavior change




########################################################
## Generate results for paper 

source("a10_results.R")

