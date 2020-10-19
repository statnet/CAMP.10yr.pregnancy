###############################################
### Set parameters for first round 

calib_minLARC_tolerance=c(10, 5, 2.5, 1, 
                          0.5, 0.25, 0.1, 
                          0.05, 0.025, 0.01, 
                          0.005, 0.0025, 0.001, 
                          0.0005, 0.00025, 0.0001,
                          0.00005, 0.000025, 0.00001,
                          0.000005, 0.0000025, 0.000001,
                          0.0000005, 0.00000025, 0.0000001
        )

cal_times <- 2
lower <- 0
upper <- 0.3

calib_minLARC_priors=list(c("unif", lower, upper), c("unif", lower, upper), 
                         c("unif", lower, upper), c("unif", lower, upper),
                         c("unif", lower, upper), c("unif", lower, upper))

###############################################
## Run first version of ABC

a10_calib_minLARC <-ABC_sequential(method="Beaumont",
                               model=a10_ABC_minLARC,
                               prior=calib_minLARC_priors,
                               nb_simul=100,
                               summary_stat_target=0,
                               tolerance_tab=calib_minLARC_tolerance,
                               verbose=FALSE,
                               progress_bar=TRUE)

minrun <- which(a10_calib_minLARC$stats==min(a10_calib_minLARC$stats))
prob_detpreg_minLARC <- a10_calib_minLARC$param[minrun,]

#prob_detpreg <- colMeans(a10_calib_minLARC$param)

save(a10_calib_minLARC, prob_detpreg_minLARC, file = "../output/a10_calib_minLARC.rda")



