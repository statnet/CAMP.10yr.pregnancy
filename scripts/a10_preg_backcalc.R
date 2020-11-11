###
setwd("C:/git/CAMP_10yr_pregnancy/scripts")
### Back-calculating pregnancies from births

a10_preg_backcalc <- function(input_params) {
  intercept <- input_params[1]
  slope <- input_params[2]
  # Guttmacher's #s for 2007 pregnancies (13-14, 15-17, 18-19): this is what we want to match
  pregs_guttmacher_binned <- c(14520, 247000, 506100)
  # NVS #s for 2007 births
  births_nvs_1yrage <- c(925, 5120, 18449, 43267, 78850, 127034, 177299)

  birth_to_preg_ratio_1yrage <- intercept + ((13:19)-12) * slope
  pregs_output_1yrage <- births_nvs_1yrage / birth_to_preg_ratio_1yrage
  
  pregs_output_binned <- c(
    sum(pregs_output_1yrage[1:2]),
    sum(pregs_output_1yrage[3:5]),
    sum(pregs_output_1yrage[6:7])
  )
  
  pregs_distance <- sum(abs(pregs_output_binned - pregs_guttmacher_binned))
  return(pregs_distance)
}

###############################################
### Set parameters for first round 

calib_preg_tolerance=c(10, 
                      5, 2.5, 1, 
                      0.5, 0.25, 0.1, 
                      0.05, 0.025, 0.01, 
                      0.005, 0.0025, 0.001,
                      0.0005, 0.00025
)

lower <- 0
upper <- 1

calib_preg_priors=list(c("unif", lower, upper), c("unif", lower, upper))

###############################################
## Run first version of ABC

a10_preg_calib <-ABC_sequential(method="Beaumont",
                               model=a10_preg_backcalc,
                               prior=calib_preg_priors,
                               nb_simul=100,
                               summary_stat_target=0,
                               tolerance_tab=calib_preg_tolerance,
                               verbose=TRUE,
                               progress_bar=TRUE)

save(a10_preg_calib, file = "../output/a10_calib_preg.rda")

a10_preg_calib$param[which(a10_preg_calib$stats==min(a10_preg_calib$stats)),]

