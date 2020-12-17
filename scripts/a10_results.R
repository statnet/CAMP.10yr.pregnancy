
#############################################################
#### Results for paper "XXXX"

# Costs saved in 2017 dollars by year

costs <- c(20308, 21057, 20090, 19325, 19160, 21247, 19670, 19255, 19080, 19338)

# Load main results

a10_nbc_minLARC <- readRDS("../output/a10_nbc_minLARC.rda")
a10_obs_minLARC <- readRDS("../output/a10_obs_minLARC.rda")
a10_obs_cc_minLARC <- readRDS("../output/a10_obs_cc_minLARC.rda")
a10_obs_sex_minLARC <- readRDS("../output/a10_obs_sex_minLARC.rda")
a10_obs_debut_minLARC <- readRDS("../output/a10_obs_debut_minLARC.rda")
a10_obs_mnppy_minLARC <- readRDS("../output/a10_obs_mnppy_minLARC.rda")

a10_nbc_maxLARC <- readRDS("../output/a10_nbc_maxLARC.rda")
a10_obs_maxLARC <- readRDS("../output/a10_obs_maxLARC.rda")
a10_obs_cc_maxLARC <- readRDS("../output/a10_obs_cc_maxLARC.rda")
a10_obs_sex_maxLARC <- readRDS("../output/a10_obs_sex_maxLARC.rda")
a10_obs_debut_maxLARC <- readRDS("../output/a10_obs_debut_maxLARC.rda")
a10_obs_mnppy_maxLARC <- readRDS("../output/a10_obs_mnppy_maxLARC.rda")

# Load and compile bootstrap results

a10_nbc_minLARC_boot <- a10_obs_minLARC_boot <- a10_obs_cc_minLARC_boot <- 
  a10_obs_sex_minLARC_boot <- a10_obs_debut_minLARC_boot <- a10_obs_mnppy_minLARC_boot <- 
  a10_nbc_maxLARC_boot <- a10_obs_maxLARC_boot <- a10_obs_cc_maxLARC_boot <- 
  a10_obs_sex_maxLARC_boot <- a10_obs_debut_maxLARC_boot <- a10_obs_mnppy_maxLARC_boot <- 
  list()

for ( bootrep in 1:nreps) {
 
  repnum <- paste(ifelse(bootrep<10, "0", ""), bootrep, sep="")
  
  a10_nbc_minLARC_boot[[bootrep]] <-       readRDS(paste("../output/a10_nbc_minLARC_boot",repnum,".rda",sep=""))
  a10_obs_minLARC_boot[[bootrep]] <-       readRDS(paste("../output/a10_obs_minLARC_boot",repnum,".rda",sep=""))
  a10_obs_cc_minLARC_boot[[bootrep]] <-    readRDS(paste("../output/a10_obs_cc_minLARC_boot",repnum,".rda",sep=""))
  a10_obs_sex_minLARC_boot[[bootrep]] <-   readRDS(paste("../output/a10_obs_sex_minLARC_boot",repnum,".rda",sep=""))
  a10_obs_debut_minLARC_boot[[bootrep]] <- readRDS(paste("../output/a10_obs_debut_minLARC_boot",repnum,".rda",sep=""))
  a10_obs_mnppy_minLARC_boot[[bootrep]] <- readRDS(paste("../output/a10_obs_mnppy_minLARC_boot",repnum,".rda",sep=""))
  
  a10_nbc_maxLARC_boot[[bootrep]] <-       readRDS(paste("../output/a10_nbc_maxLARC_boot",repnum,".rda",sep=""))
  a10_obs_maxLARC_boot[[bootrep]] <-       readRDS(paste("../output/a10_obs_maxLARC_boot",repnum,".rda",sep=""))
  a10_obs_cc_maxLARC_boot[[bootrep]] <-    readRDS(paste("../output/a10_obs_cc_maxLARC_boot",repnum,".rda",sep=""))
  a10_obs_sex_maxLARC_boot[[bootrep]] <-   readRDS(paste("../output/a10_obs_sex_maxLARC_boot",repnum,".rda",sep=""))
  a10_obs_debut_maxLARC_boot[[bootrep]] <- readRDS(paste("../output/a10_obs_debut_maxLARC_boot",repnum,".rda",sep=""))
  a10_obs_mnppy_maxLARC_boot[[bootrep]] <- readRDS(paste("../output/a10_obs_mnppy_maxLARC_boot",repnum,".rda",sep=""))

  cat(bootrep, " ", sep="")  
}

# Pull out core data from results and sum to: # pregs by year and age across scenarios

p_minL_nbc <- round(apply(a10_nbc_minLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_minL_obs <- round(apply(a10_obs_minLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_minL_obs_cc <- round(apply(a10_obs_cc_minLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_minL_obs_sex <- round(apply(a10_obs_sex_minLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   
p_minL_obs_debut <- round(apply(a10_obs_debut_minLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   
p_minL_obs_mnppy <- round(apply(a10_obs_mnppy_minLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   

p_maxL_nbc <- round(apply(a10_nbc_maxLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_maxL_obs <- round(apply(a10_obs_maxLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_maxL_obs_cc <- round(apply(a10_obs_cc_maxLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_maxL_obs_sex <- round(apply(a10_obs_sex_maxLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   
p_maxL_obs_debut <- round(apply(a10_obs_debut_maxLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   
p_maxL_obs_mnppy <- round(apply(a10_obs_mnppy_maxLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   

# Pull out core bootstrap data and sum to: # pregs by year and age across scenarios

p_minL_nbc_boot <- p_minL_obs_boot <- p_minL_obs_cc_boot <- 
  p_minL_obs_sex_boot <- p_minL_obs_debut_boot <- p_minL_obs_mnppy_boot <- 
  p_maxL_nbc_boot <- p_maxL_obs_boot <- p_maxL_obs_cc_boot <-
  p_maxL_obs_sex_boot <- p_maxL_obs_debut_boot <- p_maxL_obs_mnppy_boot <- 
  list()

for (bootnum in 1:nreps) {

  p_minL_nbc_boot[[bootnum]] <- round(apply(a10_nbc_minLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_minL_obs_boot[[bootnum]] <- round(apply(a10_obs_minLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_minL_obs_cc_boot[[bootnum]] <- round(apply(a10_obs_cc_minLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_minL_obs_sex_boot[[bootnum]] <- round(apply(a10_obs_sex_minLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  p_minL_obs_debut_boot[[bootnum]] <- round(apply(a10_obs_debut_minLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  p_minL_obs_mnppy_boot[[bootnum]] <- round(apply(a10_obs_mnppy_minLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  
  p_maxL_nbc_boot[[bootnum]] <- round(apply(a10_nbc_maxLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_maxL_obs_boot[[bootnum]] <- round(apply(a10_obs_maxLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_maxL_obs_cc_boot[[bootnum]] <- round(apply(a10_obs_cc_maxLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_maxL_obs_sex_boot[[bootnum]] <- round(apply(a10_obs_sex_maxLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  p_maxL_obs_debut_boot[[bootnum]] <- round(apply(a10_obs_debut_maxLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  p_maxL_obs_mnppy_boot[[bootnum]] <- round(apply(a10_obs_mnppy_maxLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   

}

# Get total # of pregs across all ages and years for each bootstrap replicate 
#    and query in in various ways for QC

boot_totpreg <- function(x, nreps) {
  sapply(1:nreps, function(rep) sum(x[[rep]]))
}

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

#### bctype data

bctype_in_wts <- readRDS("../output/a10_bctype_in_wts.rda")

bctype_in_yearprob <- apply(bctype_in_wts, c(3,4), sum) / apply(bctype_in_wts, 3, sum)
bctype_in_yearprob <- na_if(bctype_in_yearprob, 0)
matplot(bctype_in_yearprob, type='b', xaxt="n" , ylab= "Prop. reporting method",
        main = "Method of birth control reported by females, YRBS", ylim=c(0,0.8),
        pch=letters[1:length(bctypes_in)])
axis(1, 1:6, seq(2007, 2017, 2))
legend(1.5, 0.8, c(
  'a = no method', 'b = condoms', 'c = withdrawal', 'd = pills', 'e = injection',
  'f = LARC', 'g = other hormonal', 'h = other hormonal or LARC',
  'i = other (incl. LARC)', 'j = other', 'k = other (incl. withdrawal)'),
  cex=0.9, text.col=1:6, col=1:6, lty= 1:5, ncol=2)
abline(h=0, col="lightgray", lty=3)

#####
numerator <- apply(bctype_in_wts, c(2,3,4), sum)
denominator <- apply(bctype_in_wts, c(2,3), sum)
bctype_in_yearageprob <- array(dim=c(6,6,11))
for (i in 1:11) bctype_in_yearageprob[,,i] <- numerator[,,i]/denominator # I'm sure there's a better way to do this

bctype_in_yearageprob <- na_if(bctype_in_yearageprob, 0)
for(i in 1:6) {
  matplot(bctype_in_yearageprob[i,,], type='b', xaxt="n" , ylab= "Prop. reporting method",
          main = paste("Method of birth control reported by females age", i+12, ", YRBS", sep=''),
          ylim=c(0,0.8),pch=letters[1:length(bctypes_in)])
  axis(1, 1:6, seq(2007, 2017, 2))
  legend(1.5, 0.8, c(
    'a = no method', 'b = condoms', 'c = withdrawal', 'd = pills', 'e = injection',
    'f = LARC', 'g = other hormonal', 'h = other hormonal or LARC',
    'i = other (incl. LARC)', 'j = other', 'k = other (incl. withdrawal)'),
    cex=0.9, text.col=1:6, col=1:6, lty= 1:5, ncol=2)
  abline(h=0, col="lightgray", lty=3)
}  

####################################################################################
#### Fig: BC type by year, raw data

tiff("../output/Fig1.tif", height = 5*1200, width = 5*1200,
  units = "px", res = 1200, pointsize = 8,  compression = "lzw")

matplot(bctype_in_yearprob, type='b', xaxt="n" , ylab= "Prop. reporting method",
        main = "Method of birth control reported by females, YRBS", ylim=c(0,0.8),
        pch=letters[1:length(bctypes_in)])
axis(1, 1:6, seq(2007, 2017, 2))
legend(1.5, 0.8, c(
  'a = no method', 'b = condoms', 'c = withdrawal', 'd = pills', 'e = injection',
  'f = LARC', 'g = other hormonal', 'h = other hormonal or LARC',
  'i = other', 'j = other', 'k = other (incl. withdrawal)'),
  cex=0.9, text.col=1:6, col=1:6, lty= 1:5, ncol=2)
abline(h=0, col="lightgray", lty=3)
dev.off()
dev.off()


####################################################################################
### Plot partition of proportion averted, minLARC and maxLARC

pavert_minL_obs <- 1 - colSums(p_minL_obs)/colSums(p_minL_nbc)
pavert_minL_obs_debut <- 1 - colSums(p_minL_obs_debut)/colSums(p_minL_nbc)
pavert_minL_obs_mnppy <- 1 - colSums(p_minL_obs_mnppy)/colSums(p_minL_nbc)
pavert_minL_obs_cc <- 1 - colSums(p_minL_obs_cc)/colSums(p_minL_nbc)
pavert_maxL_obs <- 1 - colSums(p_maxL_obs)/colSums(p_maxL_nbc)
pavert_maxL_obs_cc <- 1 - colSums(p_maxL_obs_cc)/colSums(p_maxL_nbc)

tiff("../output/Fig2.tif", height = 5*1200, 5*1200,
    units = "px", res = 1200, pointsize = 8,  compression = "lzw")
plot(pavert_minL_obs, ylab="prop. pregs averted",
     ylim=c(-0.05,0.3), xaxt="n", xlab='year', main='Proportions of pregnancies averted by cause', type='b')
abline(h=0)
axis(1, 1:10, 2008:2017)
legend(1.5, 0.3, c('total',
                   'from delay in age at first sex',
                   'from changes in annual partner numbers post-debut',
                   'from changes in contraception methods'
),
  cex=0.9, text.col=c('black','red','darkgreen', 'blue'),
  col=c('black','red','darkgreen', 'blue'), pch = 1, ncol=1)

points(pavert_minL_obs_debut, col='red', type='b')
points(pavert_minL_obs_mnppy, col='darkgreen', type='b')
points(pavert_minL_obs_cc, col='blue', type='b')

points(pavert_maxL_obs, col='black', type='b')
points(pavert_maxL_obs_cc, col='blue', type='b')
dev.off()


####################################################################################
### Plot proportion averted by contraception, maxLARC, with CIs.

errbar <- function(x, up, low, ...) arrows(x, low, x, up, 
                                           length=0.00, angle=90, code=3, ...)

pavert_minL_obs_cc_boot <- t(sapply(1:nreps, function(x) 1 - colSums(p_minL_obs_cc_boot[[x]])/colSums(p_minL_nbc_boot[[x]])))
pavert_minL_obs_boot <- t(sapply(1:nreps, function(x) 1 - colSums(p_minL_obs_boot[[x]])/colSums(p_minL_nbc_boot[[x]])))
pavert_maxL_obs_cc_boot <- t(sapply(1:nreps, function(x) 1 - colSums(p_maxL_obs_cc_boot[[x]])/colSums(p_maxL_nbc_boot[[x]])))
pavert_maxL_obs_boot <- t(sapply(1:nreps, function(x) 1 - colSums(p_maxL_obs_boot[[x]])/colSums(p_maxL_nbc_boot[[x]])))


pavert_maxL_obs_cc_boot_mean <- colMeans(pavert_maxL_obs_cc_boot)
#pavert_maxL_obs_cc_boot_sd <- apply(pavert_maxL_obs_cc_boot, 2, sd)
#pavert_maxL_obs_cc_boot_ub <- pavert_maxL_obs_cc_boot_mean + 
#                                (1.96 * pavert_maxL_obs_cc_boot_sd / sqrt(nreps))
#pavert_maxL_obs_cc_boot_lb <- pavert_maxL_obs_cc_boot_mean -
#                                (1.96 * pavert_maxL_obs_cc_boot_sd / sqrt(nreps))

pavert_maxL_obs_cc_boot_ub <- apply(pavert_maxL_obs_cc_boot, 2, quantile, 0.75)
pavert_maxL_obs_cc_boot_lb <- apply(pavert_maxL_obs_cc_boot, 2, quantile, 0.25)

tiff("../output/Fig3.tif", height = 5*1200, 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
plot(1:10, pavert_maxL_obs_cc_boot_mean, ylim=c(-0.1, 0.1), col='blue', type='b',
     xaxt='n', xlab="Year", ylab = "Proportion of pregnancies")
mtext(side=3, line=2, cex=1, "Uncertainty in prop. of pregnancies averted by contraception changes")
mtext(side=3, line=1, cex=0.9, "by year, relative to 2007")
abline(h=0)
errbar(1:10, pavert_maxL_obs_cc_boot_lb, pavert_maxL_obs_cc_boot_ub, col='blue')
axis(1, 1:10, labels=2008:2017)
dev.off()

#pavert_maxL_obs_cc_boot_mean_allyrs <- mean(rowMeans(pavert_maxL_obs_cc_boot))
#pavert_maxL_obs_cc_boot_ub_allyrs <- quantile(rowMeans(pavert_maxL_obs_cc_boot), 0.75)
#pavert_maxL_obs_cc_boot_lb_allyrs <- quantile(rowMeans(pavert_maxL_obs_cc_boot), 0.25)

####################################################################################
#### Plot partitions of pregnancies averted by cause, by age, minLARC

# Still deciding whether to use this version

pregs_averted_by_age_and_cause_minL <- rbind(
  rowSums(p_minL_nbc - p_minL_obs_debut),
  rowSums(p_minL_nbc - p_minL_obs_mnppy),
  rowSums(p_minL_nbc - p_minL_obs_cc))

pregs_averted_by_age_and_cause_maxL <- rbind(
  rowSums(p_maxL_nbc - p_maxL_obs_debut),
  rowSums(p_maxL_nbc - p_maxL_obs_mnppy),
  rowSums(p_maxL_nbc - p_maxL_obs_cc))

pregs_averted_by_age_and_cause_minL <- pregs_averted_by_age_and_cause_minL[,2:6]

tiff("../output/Fig4.tif", height = 5*1200, 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")

bp <- barplot(pregs_averted_by_age_and_cause_minL, xaxt='n', beside=TRUE,
        col=c('red', 'darkgreen', 'blue'), 
        xlab='age', ylab='No. of pregnancies averted',
        main='No. of pregnancies averted by cause and by age, summed across years',
        ylim=c(-4e4,20e4))
#axis(1, bp[2,], 13:18, pos=-50000, tick = FALSE)
axis(1, bp[2,], c("14", 15:18), pos=-50000, tick = FALSE)
legend(bp[1,1], 2e5, c('from delay in age at first sex',
                       'from changes in annual partner numbers after first sex',
                       'from changes in contraception methods'),
        cex=0.9, text.col=c('red','darkgreen', 'blue'),
        fill = c('red','darkgreen', 'blue'))

dev.off()

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

####################################################################################
#### Costs by year and cause, total costs by cause

pregs_averted_by_year_and_cause <- rbind(
  colSums(p_minL_nbc - p_minL_obs_debut),
  colSums(p_minL_nbc - p_minL_obs_mnppy),
  colSums(p_minL_nbc - p_minL_obs_cc)
)

costs3 <- rbind(costs,costs,costs)

costs_averted_by_year_and_cause <- pregs_averted_by_year_and_cause * costs3
rowSums(costs_averted_by_year_and_cause)





###############################################################################3
# Isolating LARC, minLARC







# Weighted abg of preg prob

totpop_age <- colSums(meanpop_13to18_f)[,1]
sum(prob_detpreg_maxLARC[2:6] * totpop_age[2:6]) / sum(totpop_age[2:6])



####################################################################################
####################################################################################
## Original QC and debugging
####################################################################################
####################################################################################
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

