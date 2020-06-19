
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

bctype_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(bctype_df) <- c('ethn', 'age', 'year')

bctype_df$wts <- as.vector(apply(bctype_wts, 1:3, sum))

bctype_df[,5:13] <- sapply(1:9, function(x) as.vector(bctype_prob[,,,x]))
names(bctype_df)[5:13] <- bctypes

bctype_df <- bctype_df %>% replace(is.na(.), 0)

bctype_df$agefac <- relevel(as.factor(bctype_df$age), ref='16')
bctype_df$yearfac <- relevel(as.factor(bctype_df$year), ref='2007')
bctype_df$ym2007 <- bctype_df$year - 2007

#bctype_df_reg <- suppressWarnings(multinom(as.matrix(bctype_df[,5:13]) ~ 
#                                             bctype_df$agefac + 
#                                             bctype_df$yearfac + bctype_df$ethn + 
#                                             bctype_df$yearfac*bctype_df$ethn,
#                                       weights=bctype_df$wts))

bctype_df_2007 <- bctype_df %>% filter(year==2007)

bctype_df_2007 <- bctype_df_2007[,!names(bctype_df_2007) %in% 
                                   c("hormonal+LARC", "LARC", "other1", "other357")]

bctype_df_2007[rowSums(bctype_df_2007[,5:9])==0,"no method"] <- 1   # allows for fitting but does not matter since wts = 0

bctype_df_reg_2007 <- multinom(as.matrix(bctype_df_2007[,5:9])~ 
                                       bctype_df_2007$agefac + bctype_df_2007$ethn, 
                                       weights=bctype_df_2007$wts)

bctype_df_reg_2007_age <- multinom(as.matrix(bctype_df_2007[,5:9])~ 
                                 bctype_df_2007$age + bctype_df_2007$ethn, 
                                 weights=bctype_df_2007$wts)
  
#bctype_df_reg_2007_tests <- multinom(as.matrix(bctype_df_2007[,5:9])~ 
#                                 bctype_df_2007$agefac + bctype_df_2007$ethn, 
#                               weights=bctype_df_2007$wts)

#######################################3

pred_condom_f_df_indep <- expand.grid(c('B','H','W'), 13:18, 2007:2017)
colnames(pred_condom_f_df_indep) <- c('ethn', 'age', 'year')
pred_condom_f_df_indep$agefac <- relevel(as.factor(pred_condom_f_df_indep$age), ref='16')
pred_condom_f_df_indep$ym2007 <- pred_condom_f_df_indep$year - 2007
pred_condom_f <- array(predict(condom_f_reg, type='response',
                                  newdata=pred_condom_f_df_indep), dim=c(3,6,11))

#condom_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
#colnames(condom_m_df) <- c('ethn', 'age', 'year')
#condom_m_df$condom <- as.vector(condom_m)
#condom_m_df$wts <- as.vector(condom_wts_m)
#condom_m_df$agefac <- relevel(as.factor(condom_m_df$age), ref='16')
#condom_m_df$ym2007 <- condom_m_df$year - 2007
#condom_m_reg <- suppressWarnings(glm(condom ~ agefac + ym2007 + ethn + ym2007*ethn,
#                    data=condom_m_df, weights=wts, 
#                    family="binomial"))
#pred_condom_m_df_indep <- expand.grid(c('B','H','W'), 13:18, 2007:2017)
#colnames(pred_condom_m_df_indep) <- c('ethn', 'age', 'year')
#pred_condom_m_df_indep$agefac <- relevel(as.factor(pred_condom_m_df_indep$age), ref='16')
#pred_condom_m_df_indep$ym2007 <- pred_condom_m_df_indep$year - 2007
#pred_condom_m <- array(predict(condom_m_reg, type='response',
#                                  newdata=pred_condom_m_df_indep), dim=c(3,6,11))


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
