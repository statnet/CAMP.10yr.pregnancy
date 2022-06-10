
#########################################################################
# teen-SPARC 10-year pregnancy paper - input processing
#
# Notes:
#   1. Change the setwd() line if needed

#########################################################################
### Basics

#setwd("C:/git/CAMP_10yr_pregnancy/scripts/")
years <- seq(2007, 2017, by=2)          # Set years info
nyears <- length(years)

#########################################################################
### HS-attending pop sizes by age/race/sex (averaged across years)

meanschoolpop <- mean(schoolpops$totschoolpop)             # Tot pop size from census, avgd across years
#mean_prop_age_race <- apply(wts_f+wts_m, 1:2, mean) /
#          sum(apply(wts_f+wts_m, 1:2, mean))               # Tot % age/race from YRBS, avgd across years and sexes
mean_prop_age_race <- apply(wts_f, 1:2, mean) /
  sum(apply(wts_f, 1:2, mean))               # Tot % age/race from YRBS, avgd across years and sexes

prop_f <- sum(wts_f) / (sum(wts_f) + sum(wts_m))            # Tot % female from YRBS, avgd across years

n_f <- array11(mean_prop_age_race * prop_f * meanschoolpop)
n_m <- array11(mean_prop_age_race * (1-prop_f) * meanschoolpop)

n_f <- n_f[-4,,]
n_m <- n_m[-4,,]

wts_f <- wts_f[-4,,]
wts_m <- wts_m[-4,,]

#########################################################################
### Total (HS or not HS) pop sizes by age/race/sex (averaged across years)

#totpop <- totpop_f + totpop_m
totpop <- totpop_f
meanpop_13to18 <- apply(totpop, 1:2, mean, na.rm=TRUE)

#totpop_prop_f <- sum(totpop_f, na.rm=TRUE) / (sum(totpop_f, na.rm=TRUE) + sum(totpop_m, na.rm=TRUE))
#meanpop_13to18_f <- meanpop_13to18 * totpop_prop_f
meanpop_13to18_f <- meanpop_13to18
meanpop_13to18_f <- array(rep(meanpop_13to18_f, 11), dim=c(3,6,11))
n_f[n_f > meanpop_13to18_f] <- meanpop_13to18_f[n_f > meanpop_13to18_f]
prop_in_school_f <- n_f / meanpop_13to18_f

#meanpop_13to18_m <- meanpop_13to18 * (1-totpop_prop_f)
#meanpop_13to18_m <- array(rep(meanpop_13to18_m, 11), dim=c(3,6,11))
#n_m[n_m > meanpop_13to18_m] <- meanpop_13to18_m[n_m > meanpop_13to18_m]
#prop_in_school_m <- n_m / meanpop_13to18_m

