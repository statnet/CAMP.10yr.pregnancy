
library(dplyr)
library(tidyverse)
library(magrittr)

# Make sure working directory is in /scripts/
#setwd("C:/git/CAMP_10yr_pregnancy/scripts")

datapath <- "../dat"
#file_classes <- c("propsexrace.txt", "eversex.txt", "condom.txt", 
#                  "matrix1.txt", "matrix2.txt")

years <- seq(2007, 2017, by=2)
ages <- 13:18
eths <- c("Black", "Hispanic", "White")
eths_all <- c("Black", "Hispanic", "White", "Other")  # For popsizes, to remove Others from census estimates
eths_lc <- c("black", "hispanic", "white")

bctypes <- c("no method", "condoms", "hormonal", "hormonal+LARC", "LARC", "withdrawal", 
             "other79", "other1", "other357")

nages <- length(ages)
nyears <- length(years)
neths <- length(eths)
neths_all <- length(eths_all)
nbctypes <- length(bctypes)

##### Read in the pop size weights
wts_f <- wts_m <- array(dim=c(neths_all, nages, nyears))

for (i in 1:length(years)) {
    filename <- paste(datapath, "/propsexrace_allrace_", years[i], ".txt", sep="")
    temp <- read.csv(filename)
    for (j in 1:neths_all) {
      wts_f[j,,i] <- unname(unlist(
          temp %>% filter(sex=="Female", race4==eths_all[j]) %>% dplyr::select(starts_with("Age"))
      ))
      wts_m[j,,i] <- unname(unlist(
          temp %>% filter(sex=="Male", race4==eths_all[j]) %>% dplyr::select(starts_with("Age"))
      ))
    }
}

wts_f <- wts_f %>% replace_na(0)
wts_m <- wts_m %>% replace_na(0)

#### Read in the eversex numbers
# NB:the name of the sex column is different hear than for wts

eversex_f <- eversex_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/eversex_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    eversex_f[j,,i] <- unname(unlist(
      temp %>% filter(sex_active=="Female", race==eths[j]) %>% dplyr::select(starts_with("Age"))
    ))
    eversex_m[j,,i] <- unname(unlist(
      temp %>% filter(sex_active=="Male", race==eths[j]) %>% dplyr::select(starts_with("Age"))
    ))
  }
}

eversex_f <- eversex_f %>% replace_na(0)
eversex_m <- eversex_m %>% replace_na(0)

### Read in the bith control numbers
## These are expressed differently than previous values, as popsizes for each type
## And the types keep changing across years
## There are a total of nine different possible repsonses with different associated 
##    efficacies present in at least 1 year:
##       - no method:     2007, 2009, 2011, 2013, 2015, 2017
##       - condoms:       2007, 2009, 2011, 2013, 2015, 2017
##       - hormonal:      2007, 2009, ----, 2013, 2015, 2017
##       - hormonal+LARC: ----, ----, 2011, ----, ----, ----
##       - LARC:          ----, ----, ----, 2013, 2015, 2017
##       - withdrawal:    2007, 2009, 2011, ----, ----, ----
##       - other79:       2007, 2009, ----, ----, ----, ----
##       - other1:        ----, ----, 2011, ----, ----, ----
##       - other357:      ----, ----, ----, 2013, 2015, 2017
##

## "no method", "condoms", "hormonal", "hormonal+LARC", "LARC", "withdrawal", 
##             "other79", "other1", "other357"
             
## Note that names of the three "others" are my names, meant to distinguish them in terms of what
##   they represent ("other" relative to three different sets of provided lists)
##  The names for them in the actual data files are:
##    other79 =  "other"
##    other1  =  "other"
##    other357 = "withdrawal/other"
##  The code below not only imports the nine types, but records the various others to these 
##    three named types of other

#condom_f <- condom_m <- array(dim=c(neths, nages, nyears))
#condom_wts_f <- condom_wts_m <- array(dim=c(neths, nages, nyears))

bctype_wts <- array(dim=c(neths, nages, nyears, nbctypes))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/bctypes_", years[i], ".csv", sep="")
  temp <- read.csv(filename)
  temp$WgtFreq <- suppressWarnings(as.numeric(as.character(temp$WgtFreq)))
  temp$WgtFreq <- temp$WgtFreq %>% replace_na(0)

  if(years[i] %in% c(2007, 2009))       temp$pregprev <- recode(temp$pregprev, other="other79")
  if(years[i] %in% c(2011))             temp$pregprev <- recode(temp$pregprev, other="other1")
  if(years[i] %in% c(2013, 2015, 2017)) temp$pregprev <- recode(temp$pregprev, 
                                                                "withdrawal/other"="other357")
  
  for (j in 1:neths) {
    for (k in 1:nages) {
      for (m in 1:nbctypes) {
          if(nrow(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev==bctypes[m]
                                  ))==0) {  ## Cases where row is missing altogether
             bctype_wts[j,k,i,m] <- 0
          } else {
            if(sum(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev==bctypes[m]) %>% dplyr::select(WgtFreq))==0) {  
                      ## Cases where freq is 0 (either in the original data, or as a replacement for NA as done above)
              bctype_wts[j,k,i,m] <- 0
            } else {
              bctype_wts[j,k,i,m] <- unlist(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev==bctypes[m]) %>% dplyr::select(WgtFreq))
            }}  
      }
    }
  }
}

bctype_prob <- sweep(bctype_wts, 1:3, apply(bctype_wts, 1:3, sum), "/")
bctype_yearprob <- apply(bctype_wts, c(3,4), sum) / apply(bctype_wts, 3, sum)
bctype_yearprob <- na_if(bctype_yearprob, 0)
matplot(bctype_yearprob, type='b', xaxt="n" , ylab= "Prop. reporting method",
        main = "Method of birth control reported be females, YRBS", ylim=c(0,0.7))
axis(1, 1:6, seq(2007, 2017, 2))
legend(2, 0.7, c(
  '1 = no method', '2 = condoms', '3 = hormonal', '4 = hormonal or LARC', 
  '5 = LARC', '6 = withdrawal', '7 = other (including LARC)', '8 = other',
  '9 = other (including withdrawal)'),
  cex=0.7, text.col=1:6, col=1:6, lty= 1:5, ncol=2)

### Read in matrix1 (number by race by current age by age of debut by year)
## notice stop-gap in terms of dim 3 size

AgeByDebutAge_num_f <- AgeByDebutAge_num_m <- array(dim=c(neths, nages, 7, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/matrix1_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    AgeByDebutAge_num_f[j,,,i] <- unname(as.matrix(temp %>% 
                        filter(sex_active=="Female", race==eths[j]) %>% 
                        dplyr::select(starts_with("age1")), nages))
    AgeByDebutAge_num_m[j,,,i] <- unname(as.matrix(temp %>% 
                        filter(sex_active=="Male", race==eths[j]) %>% 
                        dplyr::select(starts_with("age1")), nages))
  }
}

### Read in matrix2 (mean lifetime partners by race by current age by age of debut by year)

AgeByDebutAge_lp_f <- AgeByDebutAge_lp_m <- array(dim=c(neths, nages, 7, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/matrix2_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    AgeByDebutAge_lp_f[j,,,i] <- unname(as.matrix(temp %>% 
                                 filter(sex_active=="Female", race==eths[j]) %>% 
                                 dplyr::select(starts_with("mean1")), nages))
    AgeByDebutAge_lp_m[j,,,i] <- unname(as.matrix(temp %>% 
                                 filter(sex_active=="Male", race==eths[j]) %>% 
                                 dplyr::select(starts_with("mean1")), nages))
  }
}

####################
# Total HS pop sizes
filename <- paste(datapath, "/schoolpops.csv", sep="")
schoolpops <- read.csv(filename)

####################
# Total pops 13-18

filename <- paste(datapath, "/totalpops.csv", sep="")
temp <- read.csv(filename)
totpop_f <- totpop_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  for (j in 1:neths) {
    totpop_f[j,,which(years==years[i])] <- unname(unlist(
      temp %>% filter(Year==years[i], Sex=="Female", Race==eths[j]) %>% dplyr::select(starts_with("Age"))
    ))
    totpop_m[j,,which(years==years[i])] <- unname(unlist(
      temp %>% filter(Year==years[i], Sex=="Male", Race==eths[j]) %>% dplyr::select(starts_with("Age"))
    ))
  }
}

####################
# Pregancies

## TODO Here we will import initial pregnancy info for calibration

preg_init <- mat3(c(20, 100, 200, 300, 400, 500,
                    20, 100, 200, 300, 400, 500,
                    20, 100, 200, 300, 400, 500
                  ))

#dx_gc_10_14_f <- dx_gc_10_14_m <- dx_gc_15_19_f <- dx_gc_15_19_m <- array(dim=c(neths, 1, nyears))
#dx_gc_f <- dx_gc_m <- array(dim=c(neths, nages, nyears))
#dx_ct_10_14_f <- dx_ct_10_14_m <- dx_ct_15_19_f <- dx_ct_15_19_m <- array(dim=c(neths, 1, nyears))
#dx_ct_f <- dx_ct_m <- array(dim=c(neths, nages, nyears))

#for (i in 1:nyears) {
#  filename <- paste(datapath, "/diagnoses_", years[1], ".csv", sep="")
#  temp <- read.csv(filename)
  
#  for (j in 1:neths) {
#    dx_gc_10_14_f[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="GC", Sex=="F", Ethn==eths_all[j], Age=="10-14", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#    dx_gc_15_19_f[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="GC", Sex=="F", Ethn==eths_all[j], Age=="15-19", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#    dx_gc_10_14_m[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="GC", Sex=="M", Ethn==eths_all[j], Age=="10-14", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#    dx_gc_15_19_m[j,,i] <- unname(unlist(
#        dplyr::select(Rate)
#    ))
#    dx_ct_10_14_f[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="CT", Sex=="F", Ethn==eths_all[j], Age=="10-14", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#    dx_ct_15_19_f[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="CT", Sex=="F", Ethn==eths_all[j], Age=="15-19", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#    dx_ct_10_14_m[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="CT", Sex=="M", Ethn==eths_all[j], Age=="10-14", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#    dx_ct_15_19_m[j,,i] <- unname(unlist(
#      temp %>% filter(Infection=="CT", Sex=="M", Ethn==eths_all[j], Age=="15-19", !is.na(Ethn)) %>% 
#        dplyr::select(Rate)
#    ))
#  }  
#}

### Costs
filename <- paste(datapath, "/costs.csv", sep="")
costs <- read.csv(filename)

save.image("../output/a10_inputs_raw.rda")

