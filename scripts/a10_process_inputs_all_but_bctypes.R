
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

# Failure rates of no method, condoms, pills, LARC, other hormonal, withdrawal/other
# Relative to no method
# broken out by race/ethn and age

failure_rate <- list()
failure_rate[[1]] <- mat3(rep(1.000, 18))
failure_rate[[2]] <- mat3(rep(0.153, 18))
failure_rate[[3]] <- mat3(rep(0.082, 18))
failure_rate[[4]] <- mat3(rep(0.0024, 18))
failure_rate[[5]] <- mat3(rep(0.047, 18))
failure_rate[[6]] <- mat3(rep(0.235, 18))

#########################################################################
### NOTES

save.image("../output/a10_inputs_processed.rda")

