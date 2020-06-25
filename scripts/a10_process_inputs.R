
#########################################################################
# teen-SPARC 10-year pregnancy paper - input processing
# 
# Notes: 
#   1. Change the setwd() line if needed

#########################################################################
### Basics
library(nnet)

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
#n_m <- array11(mean_prop_age_race * (1-prop_f) * meanschoolpop)

n_f <- n_f[-4,,]
#n_m <- n_m[-4,,]
wts_f <- wts_f[-4,,]
#wts_m <- wts_m[-4,,]

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

#########################################################################
### Eversex sizes

prop_eversex_f <- eversex_f / wts_f
prop_eversex_f[prop_eversex_f==Inf] <- 0
prop_eversex_f[is.nan(prop_eversex_f)] <- 0
prop_eversex_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(prop_eversex_f_df) <- c('ethn', 'age', 'year')
prop_eversex_f_df$prop_eversex <- as.vector(prop_eversex_f)
prop_eversex_f_df$wts <- as.vector(wts_f)
prop_eversex_f_df$agefac <- relevel(as.factor(prop_eversex_f_df$age), ref='16')
prop_eversex_f_df$ym2007 <- prop_eversex_f_df$year - 2007
eversex_f_reg <- suppressWarnings(glm(prop_eversex ~ agefac + ym2007 + ethn + ym2007*ethn,
                    data=prop_eversex_f_df, weights=wts, 
                    family="binomial"))
pred_eversex_f_df_indep <- expand.grid(c('B','H','W'), 13:18, 2007:2017)
colnames(pred_eversex_f_df_indep) <- c('ethn', 'age', 'year')
pred_eversex_f_df_indep$agefac <- relevel(as.factor(pred_eversex_f_df_indep$age), ref='16')
pred_eversex_f_df_indep$ym2007 <- pred_eversex_f_df_indep$year - 2007
pred_eversex_f <- array(predict(eversex_f_reg, type='response', 
                                newdata= pred_eversex_f_df_indep), dim=c(3,6,11))

#prop_eversex_m <- eversex_m / wts_m
#prop_eversex_m[prop_eversex_m==Inf] <- 0
#prop_eversex_m[is.nan(prop_eversex_m)] <- 0
#prop_eversex_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
#colnames(prop_eversex_m_df) <- c('ethn', 'age', 'year')
#prop_eversex_m_df$prop_eversex <- as.vector(prop_eversex_m)
#prop_eversex_m_df$wts <- as.vector(wts_m)
#prop_eversex_m_df$agefac <- relevel(as.factor(prop_eversex_m_df$age), ref='16')
#prop_eversex_m_df$ym2007 <- prop_eversex_m_df$year - 2007
#eversex_m_reg <- suppressWarnings(glm(prop_eversex ~ agefac + ym2007 + ethn + ym2007*ethn,
#                                      data=prop_eversex_m_df, weights=wts, 
#                                      family="binomial"))
#pred_eversex_m_df_indep <- expand.grid(c('B','H','W'), 13:18, 2007:2017)
#colnames(pred_eversex_m_df_indep) <- c('ethn', 'age', 'year')
#pred_eversex_m_df_indep$agefac <- relevel(as.factor(pred_eversex_m_df_indep$age), ref='16')
#pred_eversex_m_df_indep$ym2007 <- pred_eversex_m_df_indep$year - 2007
#pred_eversex_m <- array(predict(eversex_m_reg, type='response', 
#                                newdata= pred_eversex_m_df_indep), dim=c(3,6,11))

#########################################################################
### Birth control

# Create a full data frame of all of the data across all years
# Revised  strategy involves running a separate multinomial regression for each year given
#    the changing categories, so we end up subsetting out the data again. But it is still 
#    useful to have it all assembled in a single df for bookkeeping

bctype_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(bctype_df) <- c('ethn', 'age', 'year')

bctype_df$wts <- as.vector(apply(bctype_wts, 1:3, sum))
bctype_df$agefac <- relevel(as.factor(bctype_df$age), ref='16')

bctype_df[,6:14] <- sapply(1:9, function(x) as.vector(bctype_prob[,,,x]))
names(bctype_df)[6:14] <- bctypes

bctype_df <- bctype_df %>% replace(is.na(.), 0)

## Create partitions

included_cols_by_year <- list()
included_cols_by_year[[1]] <- included_cols_by_year[[2]] <- 
  c("no method", "condoms", "hormonal", "withdrawal", "other79")
included_cols_by_year[[3]] <- c("no method", "condoms", "hormonal+LARC", "withdrawal", "other1")
included_cols_by_year[[4]] <- included_cols_by_year[[5]] <- included_cols_by_year[[6]] <-  
  c("no method", "condoms", "hormonal", "LARC", "other357")

skipped_cols_by_year <- list()
skipped_cols_by_year[[1]] <- skipped_cols_by_year[[2]] <- 
                c("hormonal+LARC", "LARC", "other1", "other357")
skipped_cols_by_year[[3]] <- c("hormonal", "LARC", "other79", "other357")
skipped_cols_by_year[[4]] <- skipped_cols_by_year[[5]] <- skipped_cols_by_year[[6]] <-  
                c("hormonal+LARC", "withdrawal", "other79", "other1")

bctype_by_year <- list()
for (i in 1:6) {
  bctype_by_year[[i]] <- bctype_df %>% filter(year==years[i])
  bctype_by_year[[i]] <- bctype_by_year[[i]][,!names(bctype_by_year[[i]]) %in% 
    skipped_cols_by_year[[i]]]
  bctype_by_year[[i]][rowSums(bctype_by_year[[i]][,-(1:5)])==0,"no method"] <- 1
  # allows for fitting but does not matter since wts = 0

}

bctype_reg_agefac <- bctype_reg_agenum <- list() 
for (i in 1:6) {
  bctype_reg_agefac[[i]] <- multinom(as.matrix(bctype_by_year[[i]][,-(1:5)])~ 
                                   bctype_by_year[[i]]$agefac + bctype_by_year[[i]]$ethn, 
                                   weights=bctype_by_year[[i]]$wts)

  bctype_reg_agenum[[i]] <- multinom(as.matrix(bctype_by_year[[i]][,-(1:5)])~ 
                                    bctype_by_year[[i]]$age + bctype_by_year[[i]]$ethn, 
                                    weights=bctype_by_year[[i]]$wts)
}

bctype_reg_agefac_AIC <- sapply(1:6, function(x) bctype_reg_agefac[[x]]$AIC)
bctype_reg_agenum_AIC <- sapply(1:6, function(x) bctype_reg_agenum[[x]]$AIC)  
# Agenum are lower, but we are still proceeding with agefac because our goal here is not to
#   develop the most parsimonious model, but rather to develop the fullest model that smooths
#   the data observed across all multi-way categories using first principles

### assemble the predicted value matrices
### This could be done with four-dimenwsional arrays and less verbose code, 
###   but I like having 3 dims and explicit names to reduce room for confusion and error

pred_bctype_agefac <- pred_bctype_agenum <- list(
                            no_method     = array(dim=c(3,6,11)),
                            condoms       = array(dim=c(3,6,11)),
                            hormonal      = array(dim=c(3,6,11)),
                            hormonal_LARC = array(dim=c(3,6,11)),
                            LARC          = array(dim=c(3,6,11)),
                            withdrawal    = array(dim=c(3,6,11)),
                            other79       = array(dim=c(3,6,11)),
                            other1        = array(dim=c(3,6,11)),
                            other357      = array(dim=c(3,6,11))
)

for (year in 1:6) {
  types <- which(bctypes%in%included_cols_by_year[[year]])
  for (type_index in 1:length(types)) {
      pred_bctype_agefac[[types[type_index]]][,,year*2-1] <- # Expands out to include even years
        matrix(bctype_reg_agefac[[year]]$fitted.values[,type_index], nrow=3)
      pred_bctype_agenum[[types[type_index]]][,,year*2-1] <- # Expands out to include even years
        matrix(bctype_reg_agenum[[year]]$fitted.values[,type_index], nrow=3)
  }
}

matplot(t(pred_bctype_agenum[[2]][,,c(11)]), type='b')

#######################################


#########################################################################
### mean # new partners per year (mnnppy)

mnppy_f <- array(dim=c(3,6,6))
mnppy_wts_f <- array(dim=c(3,6,6))
rowages <- 13:18
colages <- 11:17
returnages <- 13:18
for (i in 1:6) {
  for (j in 1:3) {
    popsizes <- AgeByDebutAge_num_f[j,,,i]
    lifeparts <- AgeByDebutAge_lp_f[j,,,i]
    temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)
    mnppy_f[j,,i] <- temp$mnppy
    mnppy_wts_f[j,,i] <- temp$wts
  }  
}

#mnppy_m <- array(dim=c(3,6,6))
#mnppy_wts_m <- array(dim=c(3,6,6))
#rowages <- 13:18
#colages <- 11:17
#returnages <- 13:18
#for (i in 1:6) {
#  for (j in 1:3) {
#    popsizes <- AgeByDebutAge_num_m[j,,,i]
#    lifeparts <- AgeByDebutAge_lp_m[j,,,i]
#    temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)
#    mnppy_m[j,,i] <- temp$mnppy
#    mnppy_wts_m[j,,i] <- temp$wts
#  }  
#}

mnppy_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(mnppy_f_df) <- c('ethn', 'age', 'year')
mnppy_f_df$mnppy <- as.vector(mnppy_f)
mnppy_f_df$wts <- as.vector(mnppy_wts_f)
mnppy_f_df$agefac <- relevel(as.factor(mnppy_f_df$age), ref='16')
mnppy_f_df$ym2007 <- mnppy_f_df$year - 2007
mnppy_f_reg <- suppressWarnings(glm(mnppy ~ ethn + ym2007 + ethn*ym2007 + age + I(age^2),
                   data=mnppy_f_df, weights=wts, na.action=na.exclude, family="poisson"))
pred_mnppy_f_df_indep <- expand.grid(c('B','H','W'), 13:18, 2007:2017)
colnames(pred_mnppy_f_df_indep) <- c('ethn', 'age', 'year')
pred_mnppy_f_df_indep$agefac <- relevel(as.factor(pred_mnppy_f_df_indep$age), ref='16')
pred_mnppy_f_df_indep$ym2007 <- pred_mnppy_f_df_indep$year - 2007
pred_mnppy_f <- array(predict(mnppy_f_reg, type='response',
                              newdata=pred_mnppy_f_df_indep), dim=c(3,6,11))

#mnppy_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
#colnames(mnppy_m_df) <- c('ethn', 'age', 'year')
#mnppy_m_df$mnppy <- as.vector(mnppy_m)
#mnppy_m_df$wts <- as.vector(mnppy_wts_m)
#mnppy_m_df$agefac <- relevel(as.factor(mnppy_m_df$age), ref='16')
#mnppy_m_df$ym2007 <- mnppy_m_df$year - 2007
#mnppy_m_reg <- suppressWarnings(glm(mnppy ~ ethn + ym2007 + ethn*ym2007 + age + I(age^2),
#                   data=mnppy_m_df, weights=wts, na.action=na.exclude, family="poisson"))
#pred_mnppy_m_df_indep <- expand.grid(c('B','H','W'), 13:18, 2007:2017)
#colnames(pred_mnppy_m_df_indep) <- c('ethn', 'age', 'year')
#pred_mnppy_m_df_indep$agefac <- relevel(as.factor(pred_mnppy_m_df_indep$age), ref='16')
#pred_mnppy_m_df_indep$ym2007 <- pred_mnppy_m_df_indep$year - 2007
#pred_mnppy_m <- array(predict(mnppy_m_reg, type='response',
#                                 newdata=pred_mnppy_m_df_indep), dim=c(3,6,11))

#########################################################################
### Coital acts per partner

capp_f <- array11(mat3(c( 9.4, 9.4, 9.4, 24.7, 24.7, 46.7,
                          9.4, 9.4, 9.4, 24.7, 24.7, 46.7,
                          9.4, 9.4, 9.4, 24.7, 24.7, 46.7
                         )))

#capp_m <- array11(mat3(c( 11.9, 11.9, 11.9, 19.3, 19.3, 29.3,
#                          11.9, 11.9, 11.9, 19.3, 19.3, 29.3,
#                          11.9, 11.9, 11.9, 19.3, 19.3, 29.3
#                         )))

#########################################################################
### Small inputs

prob_detpreg_f <- mat3(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                         0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                         0.1, 0.1, 0.1, 0.1, 0.1, 0.1
                         )) ## TODO: This will be calib parameter, no?

#########################################################################
### NOTES

save.image("../output/a10_inputs_processed.rda")
