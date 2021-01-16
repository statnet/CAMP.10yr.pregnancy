####################################################################################
####################################################################################
## Original QC and debugging
####################################################################################
####################################################################################

####################################################################################
# Get total # of pregs across all ages and years for each bootstrap replicate 
#   and query in in various ways for QC
# Note: these include age 13, so should only be used for QC and not for final results

qc <- FALSE
if(qc) {
  boot_totpreg <- function(x, nreps) {sapply(1:nreps, function(rep) sum(x[[rep]]))}
  
  p_minL_nbc_boot_totpreg       <- boot_totpreg(p_minL_nbc_boot      , nreps)
  p_minL_obs_boot_totpreg       <- boot_totpreg(p_minL_obs_boot      , nreps)
  p_minL_obs_cc_boot_totpreg    <- boot_totpreg(p_minL_obs_cc_boot   , nreps)
  p_minL_obs_sex_boot_totpreg   <- boot_totpreg(p_minL_obs_sex_boot  , nreps)
  p_minL_obs_debut_boot_totpreg <- boot_totpreg(p_minL_obs_debut_boot, nreps)
  p_minL_obs_mnppy_boot_totpreg <- boot_totpreg(p_minL_obs_mnppy_boot, nreps)
  
  boxplot(p_minL_nbc_boot_totpreg, p_minL_obs_boot_totpreg,
          p_minL_obs_cc_boot_totpreg, p_minL_obs_sex_boot_totpreg, 
          p_minL_obs_debut_boot_totpreg, p_minL_obs_mnppy_boot_totpreg)
  rowMeans(rbind(p_minL_nbc_boot_totpreg, p_minL_obs_boot_totpreg,
                 p_minL_obs_cc_boot_totpreg, p_minL_obs_sex_boot_totpreg, 
                 p_minL_obs_debut_boot_totpreg, p_minL_obs_mnppy_boot_totpreg))
  sum(p_minL_nbc)
  
  p_maxL_nbc_boot_totpreg       <- boot_totpreg(p_maxL_nbc_boot      , nreps)
  p_maxL_obs_boot_totpreg       <- boot_totpreg(p_maxL_obs_boot      , nreps)
  p_maxL_obs_cc_boot_totpreg    <- boot_totpreg(p_maxL_obs_cc_boot   , nreps)
  p_maxL_obs_sex_boot_totpreg   <- boot_totpreg(p_maxL_obs_sex_boot  , nreps)
  p_maxL_obs_debut_boot_totpreg <- boot_totpreg(p_maxL_obs_debut_boot, nreps)
  p_maxL_obs_mnppy_boot_totpreg <- boot_totpreg(p_maxL_obs_mnppy_boot, nreps)
  
  boxplot(p_maxL_nbc_boot_totpreg, p_maxL_obs_boot_totpreg,
          p_maxL_obs_cc_boot_totpreg, p_maxL_obs_sex_boot_totpreg, 
          p_maxL_obs_debut_boot_totpreg, p_maxL_obs_mnppy_boot_totpreg)
  rowMeans(rbind(p_maxL_nbc_boot_totpreg, p_maxL_obs_boot_totpreg,
                 p_maxL_obs_cc_boot_totpreg, p_maxL_obs_sex_boot_totpreg, 
                 p_maxL_obs_debut_boot_totpreg, p_maxL_obs_mnppy_boot_totpreg))
  sum(p_maxL_nbc)
}

####################################################################################
#### Plot partitions of pregnancies averted by age and year by BC types, minLARC

# Still exploring

#pregs_averted_by_age_and_year <- p_minL_nbc - p_minL_obs_cc
#
#c(colSums(pregs_averted_by_age_and_year[1:2,]),
#  colSums(pregs_averted_by_age_and_year[3:4,]),
#  pregs_averted_by_age_and_year[5:6,])
#
#pregs_averted_by_age_and_year_binned4 <- rbind(
#    colSums(pregs_averted_by_age_and_year[1:4,]),
#    pregs_averted_by_age_and_year[5:6,])
#
#### 6 ages version
#matplot(t(p_minL_nbc - p_minL_obs_cc), type='b', pch=1:6, xaxt='n',
#        main = 'No. of pregnancies averted by changes in contraception usage',
#        xlab='Age', ylab='Number of pregnancies averted')
#abline(h=0, col='darkgray')
#axis(1, 1:10, 2008:2017)
#legend(1, 38000, c('13-yo','14-yo',
#                   '15-yo','16-yo',
#                   '17-yo','18-yo'),
#       cex=0.87, text.col=1:6,
#       col=1:6, lty=1, pch=1:6, ncol=2)
#
#### 4 ages version
#matplot(t(pregs_averted_by_age_and_year_binned4), type='b', pch=1:4, xaxt='n',
#        main = 'No. of pregnancies averted by changes in contraception usage',
#        xlab='Age', ylab='Number of pregnancies averted')
#abline(h=0, col='darkgray')
#axis(1, 1:10, 2008:2017)
#legend(1, 38000, c('13-14-yo','15-16-yo',
#                   '17-yo','18-yo'),
#       cex=0.87, text.col=1:4,
#       col=1:4, lty=1, pch=1:4, ncol=2)





#### function: wtavg_bctype

#wtavg_bctype <- function(obj, method, dim) {
#  apply((obj[[method]]*meanpop_13to18_f*pred_eversex_f_dyn), dim, sum) /
#    apply((meanpop_13to18_f*pred_eversex_f_dyn), dim, sum)
#}

####################################################################################
#### bctypes predicted by regression

#bctypes<- names(pred_bctype_minLARC_dyn)
#bctypes_mean_minLARC <- 
#  sapply(1:6, function(x) wtavg_bctype(pred_bctype_minLARC_dyn, bctypes[x],3))
#
#matplot(bctypes_mean_minLARC, type='b', xlab="year", xaxt="n",
#        ylab = "Prop. using method", main = "Minimum LARC use scenario",
#        ylim=c(0,0.8), pch=c('a', 'b', 'd', 'f', 'g', 'k'))
#
#legend(1.5, 0.8, c(
#  'a = no method', 'b = condoms', 'd = pills', 
#  'f = LARC', 'g = other hormonal', 'k = other (incl. withdrawal)'),
#  cex=0.7, text.col=1:6, col=1:6, lty= 1:5, ncol=2)
#
#axis(1, 1:11, 2007:2017)
#abline(h=0, col="lightgray", lty=3)
#
##bctype_in <- names(pred_bctype_minLARC_dyn)
#bctypes_mean_in <- 
#  sapply(1:11, function(x) wtavg_bctype(bctypes_in, bctypes[x],3))

