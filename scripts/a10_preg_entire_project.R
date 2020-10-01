
########################################################################
## Code for manuscript "XXXXX"

setwd("C:/git/CAMP_10yr_pregnancy/scripts/")  # Change depending on machine
#setwd("C:/git/CAMP.10yr.pregnancy/scripts/")  # Change depending on machine
rm(list=ls())

#install.packages("EasyABC")
library(EasyABC)
library(MASS)
set.seed(1)

########################################################################
### Inputs for GC and CT

source("a10_preg_backcalc_logistic.R")
source("a10_import.R")                          # Get all inputs
source("a10_process_inputs.R")                  # Process inputs (i.e. conduct regressions, etc.)
source("a10_reassign_bctypes.R")                # Move bc methods from input types to standardized types
source("a10_impute_even_years.R")               # Impute even years
source("a10_make_behav_inputs_all_2007.R")      # override 2009-2017 numbers with 2007 for both calibration and no-behavior-change models
source("a10_ABC_minLARC.R")

########################################################################
### # Scenario 1: No LARC prior to 2013 (minLARC)
source("a10_calibration_minLARC.R")          # calib pt 1

#source("a10_gc_calibration_ABC_check.R")    # Load calibration check function
#boxplot(a10_calib_pt1$param)                # Check pt 1 calibration
#calib_test_gc(a10_calib_gc_pt1, 
#  "../output/a10_calib_test_gc_step1_f.pdf", 
#  "../output/a10_calib_test_gc_step1_m.pdf")

########################################################################
### Run scenarios and credible intervals

source("a10_no_behav_change_script.R")       # No behavior change
source("a10_obs_behav_change.R")             # Observed behavior change
source("a10_obs_contraception_change.R")     # Observed contraception change only
source("a10_obs_sexual_activity_change.R")   # Observed debut / partner numbers only

#source("a10_credible_intervals.R")           # Observed behavior change


########################################################
## Generate results for paper 

source("a10_results.R")

