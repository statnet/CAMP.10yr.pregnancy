
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

bctypes_in <- c("no method", "condoms", "withdrawal", "pills", "injection", "LARC", 
             "other hormonal", "other hormonal+LARC", "other79", "other1", "withdrawal/other")

nages <- length(ages)
nyears <- length(years)
neths <- length(eths)
neths_all <- length(eths_all)
nbctypes_in <- length(bctypes_in)

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

eversex_f <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/eversex_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    eversex_f[j,,i] <- unname(unlist(
      temp %>% filter(sex_active=="Female", race==eths[j]) %>% dplyr::select(starts_with("Age"))
    ))
    #eversex_m[j,,i] <- unname(unlist(
    #  temp %>% filter(sex_active=="Male", race==eths[j]) %>% dplyr::select(starts_with("Age"))
    #))
  }
}

eversex_f <- eversex_f %>% replace_na(0)
#eversex_m <- eversex_m %>% replace_na(0)

### Read in the bith control numbers
## These are expressed differently than previous values, as popsizes for each type
## And the types keep changing across years

## OLD***********************************
## There are a total of nine different possible responses with different associated 
##    efficacies present in at least 1 year:
##       1 * no method:     2007, 2009, 2011, 2013, 2015, 2017
##       2 * condoms:       2007, 2009, 2011, 2013, 2015, 2017
##       3 * hormonal:      2007, 2009, ----, 2013, 2015, 2017
##       4   hormonal+LARC: ----, ----, 2011, ----, ----, ----
##       5 * LARC:          ----, ----, ----, 2013, 2015, 2017
##       6 * withdrawal:    2007, 2009, 2011, ----, ----, ----
##       7   other79:       2007, 2009, ----, ----, ----, ----
##       8 * other1:        ----, ----, 2011, ----, ----, ----
##       9   other357:      ----, ----, ----, 2013, 2015, 2017
##

##INPUTS:
##       1 * no method:           2007, 2009, 2011, 2013, 2015, 2017
##       2 * condoms:             2007, 2009, 2011, 2013, 2015, 2017
##       3   withdrawal:          2007, 2009, 2011, ----, ----, ----
##       4 * pills:               2007, 2009, 2011, 2013, 2015, 2017
##       5   injection:           2007, 2009, ----, ----, ----, ----
##       6 * LARC:                ----, ----, ----, 2013, 2015, 2017
##       7 * other hormonal:      ----, ----, ----, 2013, 2015, 2017
##       8   other hormonal+LARC: ----, ----, 2011, ----, ----, ----
##       9   other79:             2007, 2009, ----, ----, ----, ----
##      10   other1:              ----, ----, 2011, ----, ----, ----
##      11 * withdrawal/other:    ----, ----, ----, 2013, 2015, 2017

## "no method", "condoms", "withdrawal", "pills", "injection", "LARC",
## "other hormonal", "other hormonal+LARC", "other79", "other1", "withdrawal/other"

## Note that names of the fist two "others" are my names, meant to distinguish them in terms of what
##   they represent ("other" relative to three different sets of provided lists)
##  The names for them in the actual data files are:
##    other79 =  "other"
##    other1  =  "other"
##  The code imports the 11 types, and then  renames these two

#condom_f <- condom_m <- array(dim=c(neths, nages, nyears))
#condom_wts_f <- condom_wts_m <- array(dim=c(neths, nages, nyears))

bctype_in_wts <- array(dim=c(neths, nages, nyears, nbctypes_in))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/bctypes_", years[i], ".csv", sep="")
  temp <- read.csv(filename)
  temp$WgtFreq <- suppressWarnings(as.numeric(as.character(temp$WgtFreq)))
  temp$WgtFreq <- temp$WgtFreq %>% replace_na(0)

  if(years[i] %in% c(2007, 2009))       temp$pregprev2 <- recode(temp$pregprev2, other="other79")
  if(years[i] %in% c(2011))             temp$pregprev2 <- recode(temp$pregprev2, other="other1")

  for (j in 1:neths) {
    for (k in 1:nages) {
      for (m in 1:nbctypes_in) {
          if(nrow(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev2==bctypes_in[m]
                                  ))==0) {  ## Cases where row is missing altogether
             bctype_in_wts[j,k,i,m] <- 0
          } else {
            if(sum(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev2==bctypes_in[m]) %>% dplyr::select(WgtFreq))==0) {  
                      ## Cases where freq is 0 (either in the original data, or as a replacement for NA as done above)
              bctype_in_wts[j,k,i,m] <- 0
            } else {
              bctype_in_wts[j,k,i,m] <- unlist(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev2==bctypes_in[m]) %>% dplyr::select(WgtFreq))
            }}  
      }
    }
  }
}

bctype_in_prob <- sweep(bctype_in_wts, 1:3, apply(bctype_in_wts, 1:3, sum), "/")

#####

## Read in matrix1 (number by race by current age by age of debut by year)
## notice stop-gap in terms of dim 3 size

AgeByDebutAge_num_f <- array(dim=c(neths, nages, 7, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/matrix1_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    AgeByDebutAge_num_f[j,,,i] <- unname(as.matrix(temp %>% 
                        filter(sex_active=="Female", race==eths[j]) %>% 
                        dplyr::select(starts_with("age1")), nages))
    #AgeByDebutAge_num_m[j,,,i] <- unname(as.matrix(temp %>% 
    #                    filter(sex_active=="Male", race==eths[j]) %>% 
    #                    dplyr::select(starts_with("age1")), nages))
  }
}

### Read in matrix2 (mean lifetime partners by race by current age by age of debut by year)

AgeByDebutAge_lp_f <- array(dim=c(neths, nages, 7, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/matrix2_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    AgeByDebutAge_lp_f[j,,,i] <- unname(as.matrix(temp %>% 
                                 filter(sex_active=="Female", race==eths[j]) %>% 
                                 dplyr::select(starts_with("mean1")), nages))
    #AgeByDebutAge_lp_m[j,,,i] <- unname(as.matrix(temp %>% 
    #                             filter(sex_active=="Male", race==eths[j]) %>% 
    #                             dplyr::select(starts_with("mean1")), nages))
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
# Pregnancies

# DOING INPUTS ONLY BY AGE NOT BY RACE/ETHN
preg_init <- pregs_1yr_model

### Costs
filename <- paste(datapath, "/costs.csv", sep="")
costs <- read.csv(filename)

save.image("../output/a10_inputs_raw.rda")

