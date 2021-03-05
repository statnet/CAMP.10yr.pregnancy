  
##########################################################
### Determining credible intervals for pregnancy model

### Save all
save.image(file="../output/a10_preg_all_before_boot.rda")

### Bootstrapping

nreps <- 3
bctype_in_wts_boot <- array(dim=c(neths, nages, nyears, nbctypes_in, nreps))
eversex_boot_yes <- array(dim=c(neths, nages, nyears, nreps))
eversex_boot_no <- array(dim=c(neths, nages, nyears, nreps))
AgeByDebutAge_lp_boot <- array(dim=c(neths, nages, 7, nyears, nreps))
AgeByDebutAge_num_boot <- array(dim=c(neths, nages, 7, nyears, nreps))


for (i in 1:length(years)) {
  filename <- paste(datapath, "/bctypes_ind_", years[i], ".csv", sep="")
  temp <- read.csv(filename)

  # Recoding based on David's email of 11/13/2020 - to get in line with exact structure of the original data files
  temp$sex <- recode(temp$sex, '1' = 'female', '2' = 'male')
  temp$race <- recode(temp$race, '1'='white', '2'='black', '3'='hispanic')
  temp <- filter(temp, sex=='female')
  temp <- filter(temp, age %in% 13:18)
  temp <- filter(temp, !is.na(age))
  temp <- filter(temp, !is.na(sex))
  temp <- filter(temp, !is.na(race))
  temp <- filter(temp, !is.na(pregprev2))
  
  if(years[i] %in% c(2007, 2009)) {
    temp$pregprev2 <- recode(temp$pregprev2,
                             '1'='no method', '2'='pills', '3'='injection', 
                             '4'='condoms', '5'='withdrawal', '6'='other79')}
  if(years[i] %in% c(2011)) {
    temp$pregprev2 <- recode(temp$pregprev2, 
                             '1'='no method', '2'='pills', '3'='other hormonal+LARC',
                             '4'='condoms', '5'='withdrawal', '6'='other1')}
  if(years[i] %in% c(2013, 2015, 2017)) {
    temp$pregprev2 <- recode(temp$pregprev2,
                             '1'='no method', '2'='pills', '3'='other hormonal',
                             '4'='condoms', '5'='LARC', '6'='withdrawal/other')}

  filename <- paste(datapath, "/data_ind_", years[i], ".csv", sep="")  # These data came limited to females
  temp2 <- read.csv(filename)
  temp2 <- rename(temp2, "weight"="Analysis.weight", "age"="Current.age", "race"="X4.level.race.variable",
                          "eversex"="Ever.had.sexual.intercourse", "age1stsex"="At.at.first.sex", "nlifepart"="Lifetime.number.of.sexual.partners")
  temp2 <- filter(temp2, age %in% 13:18)
  temp2 <- filter(temp2, !is.na(age))
  temp2 <- filter(temp2, age!=".")
  temp2 <- filter(temp2, race!=".")
  temp2 <- filter(temp2, race!="Other")
  temp2$race <- recode(temp2$race, 'White'='white', 'Black'='black', 'Hispanic'='hispanic')
  temp2$age1stsex <- recode(temp2$age1stsex, '11'='12')
  
  n <- nrow(temp)
  for (s in 1:nreps) {
    indices <- sample(1:n, n, prob=temp$weight, replace=TRUE)
    resample <- temp[indices,]
    for (j in 1:neths) {
      for (k in 1:nages) {
        for (m in 1:nbctypes_in) {
            bctype_in_wts_boot[j,k,i,m,s] <- nrow(resample %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev2==bctypes_in[m]))
        }
      }
    }
  cat(s, ' ', sep='')
  }
  
  n <- nrow(temp2)
  for (s in 1:nreps) {
    indices <- sample(1:n, n, prob=temp2$weight, replace=TRUE)
    resample <- temp2[indices,]
    for (j in 1:neths) {
      for (k in 1:nages) {
          eversex_boot_yes[j,k,i,s] <- nrow(resample %>% filter(race==eths_lc[j], age==ages[k], eversex=="Yes"))
          eversex_boot_no[j,k,i,s] <- nrow(resample %>% filter(race==eths_lc[j], age==ages[k], !eversex=="Yes"))  #Consistency with previous methods
          for (z in 1:7) {
            AgeByDebutAge_num_boot[j,k,z,i,s] <- nrow(resample %>% filter(race==eths_lc[j], age==ages[k], age1stsex==z+12))
            if (AgeByDebutAge_num_boot[j,k,z,i,s]>0) {
              AgeByDebutAge_lp_boot[j,k,z,i,s] <- suppressWarnings(
                mean(as.numeric((resample %>% filter(race==eths_lc[j], age==ages[k], age1stsex==z+12))$nlifepart))
              )
            }
          }
      }
    }
    cat(s, ' ', sep='')
  }
}

#### Run the model on each bootstrapped version

bctype_in_prob_boot <- sweep(bctype_in_wts_boot, c(1:3,5), apply(bctype_in_wts_boot, c(1:3,5), sum), "/")

pred_bctype_minLARC_dyn_boot <- list()  # Temp

for (bootrep in 1:nreps) {
  bctype_in_wts <- bctype_in_wts_boot[,,,,bootrep]
  bctype_in_prob <- bctype_in_prob_boot[,,,,bootrep]
  AgeByDebutAge_num_f <- AgeByDebutAge_num_boot[,,,,bootrep]
  AgeByDebutAge_lp_f <- AgeByDebutAge_lp_boot[,,,,bootrep]
  eversex_f <- eversex_boot_yes[,,,bootrep]

  source("a10_process_inputs_popsizes.R")  # Process inputs (i.e. conduct regressions, etc.)
  
  wts_f <- eversex_boot_yes[,,,bootrep] + eversex_boot_no[,,,bootrep]
  wts_m <- wts_f

  source("a10_process_inputs_all_but_bctypes.R")  # Process inputs (i.e. conduct regressions, etc.)
  source("a10_process_inputs_bctypes.R")          # Process inputs (i.e. conduct regressions, etc.)
  source("a10_reassign_bctypes.R")                # Move bc methods from input types to standardized types
  source("a10_impute_even_years.R")               # Impute even years
  source("a10_make_behav_inputs_all_2007.R")      # override 2009-2017 numbers with 2007 for both calibration and no-behavior-change models
  
  pred_bctype_minLARC_dyn_boot[[bootrep]] <- pred_bctype_minLARC_dyn  # TEMP
  
  repnum <- paste(ifelse(bootrep<10, "0", ""), bootrep, sep="")

  ########################################################################
  ### # Scenario 1: No LARC prior to 2013 (minLARC)
  #source("a10_ABC_minLARC.R")
  source("a10_calibration_minLARC.R")                  # calib pt 1
  source("a10_no_behav_change_minLARC.R")              # No behavior change
  source("a10_obs_behav_change_minLARC.R")             # Observed behavior change
  source("a10_obs_contraception_change_minLARC.R")     # Observed contraception change only
  source("a10_obs_sexual_activity_change_minLARC.R")   # Observed debut / partner numbers only
  source("a10_obs_debut_change_minLARC.R")             # Observed debut only
  source("a10_obs_mnppy_change_minLARC.R")             # Observed partner numbers only
  
  saveRDS(a10_calib_minLARC,     file=paste("../output/a10_calib_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(prob_detpreg_minLARC,  file=paste("../output/prob_detpreg_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_nbc_minLARC,       file=paste("../output/a10_nbc_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_minLARC,       file=paste("../output/a10_obs_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_cc_minLARC,    file=paste("../output/a10_obs_cc_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_sex_minLARC,   file=paste("../output/a10_obs_sex_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_debut_minLARC, file=paste("../output/a10_obs_debut_minLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_mnppy_minLARC, file=paste("../output/a10_obs_mnppy_minLARC_boot",repnum,".rda",sep=""))
  
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
  
  saveRDS(a10_calib_maxLARC,     file=paste("../output/a10_calib_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(prob_detpreg_maxLARC,  file=paste("../output/prob_detpreg_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_nbc_maxLARC,       file=paste("../output/a10_nbc_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_maxLARC,       file=paste("../output/a10_obs_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_cc_maxLARC,    file=paste("../output/a10_obs_cc_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_sex_maxLARC,   file=paste("../output/a10_obs_sex_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_debut_maxLARC, file=paste("../output/a10_obs_debut_maxLARC_boot",repnum,".rda",sep=""))
  saveRDS(a10_obs_mnppy_maxLARC, file=paste("../output/a10_obs_mnppy_maxLARC_boot",repnum,".rda",sep=""))
  
  filename <- paste("../output/a10_preg_boot", ifelse(i<10, "0", ""), bootrep, ".rda", sep="")
  save.image(file=filename)
  cat("Finished bootstrap ", bootrep, ".\n", sep="")
}
