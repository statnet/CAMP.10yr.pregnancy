
####################################################################################
#### Results for paper
####################################################################################

nreps <- 107

####################################################################################
# Costs saved in 2017 dollars by year

# costs <- c(20308, 21057, 20090, 19325, 19160, 21247, 19670, 19255, 19080, 19338)
costs <- c(20308, 21057, 20090, 19325, 19160, 21247, 19670, 19255, 19080, 19013)  # email from Li Yan 12/14/20 last year updated

###############################################################################
# Weighted avg of preg prob

#totpop_age <- colSums(meanpop_13to18_f)[,1]
#sum(prob_detpreg_maxLARC[2:6] * totpop_age[2:6]) / sum(totpop_age[2:6])

####################################################################################
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

a10_nbc_medLARC <- readRDS("../output/a10_nbc_medLARC.rda")
a10_obs_medLARC <- readRDS("../output/a10_obs_medLARC.rda")
a10_obs_cc_medLARC <- readRDS("../output/a10_obs_cc_medLARC.rda")
a10_obs_sex_medLARC <- readRDS("../output/a10_obs_sex_medLARC.rda")
a10_obs_debut_medLARC <- readRDS("../output/a10_obs_debut_medLARC.rda")
a10_obs_mnppy_medLARC <- readRDS("../output/a10_obs_mnppy_medLARC.rda")

a10_obs_cc_onlyLARC_minL_from_all <- readRDS("../output/a10_obs_cc_onlyLARC_minL_from_all.rda")
a10_obs_cc_onlyLARC_minL_from_wdl <- readRDS("../output/a10_obs_cc_onlyLARC_minL_from_wdl.rda")
a10_obs_cc_onlyLARC_minL_from_cdm <- readRDS("../output/a10_obs_cc_onlyLARC_minL_from_cdm.rda")
a10_obs_cc_onlyLARC_minL_from_pil <- readRDS("../output/a10_obs_cc_onlyLARC_minL_from_pil.rda")

a10_obs_cc_onlyLARC_maxL_from_all <- readRDS("../output/a10_obs_cc_onlyLARC_maxL_from_all.rda")
a10_obs_cc_onlyLARC_maxL_from_wdl <- readRDS("../output/a10_obs_cc_onlyLARC_maxL_from_wdl.rda")
a10_obs_cc_onlyLARC_maxL_from_cdm <- readRDS("../output/a10_obs_cc_onlyLARC_maxL_from_cdm.rda")
a10_obs_cc_onlyLARC_maxL_from_pil <- readRDS("../output/a10_obs_cc_onlyLARC_maxL_from_pil.rda")

a10_obs_cc_onlyLARC_medL_from_all <- readRDS("../output/a10_obs_cc_onlyLARC_medL_from_all.rda")
a10_obs_cc_onlyLARC_medL_from_wdl <- readRDS("../output/a10_obs_cc_onlyLARC_medL_from_wdl.rda")
a10_obs_cc_onlyLARC_medL_from_cdm <- readRDS("../output/a10_obs_cc_onlyLARC_medL_from_cdm.rda")
a10_obs_cc_onlyLARC_medL_from_pil <- readRDS("../output/a10_obs_cc_onlyLARC_medL_from_pil.rda")

####################################################################################
# Load and compile bootstrap results

a10_nbc_minLARC_boot <- a10_obs_minLARC_boot <- a10_obs_cc_minLARC_boot <- 
  a10_obs_sex_minLARC_boot <- a10_obs_debut_minLARC_boot <- a10_obs_mnppy_minLARC_boot <- 
  a10_nbc_maxLARC_boot <- a10_obs_maxLARC_boot <- a10_obs_cc_maxLARC_boot <- 
  a10_obs_sex_maxLARC_boot <- a10_obs_debut_maxLARC_boot <- a10_obs_mnppy_maxLARC_boot <- 
  a10_nbc_medLARC_boot <- a10_obs_medLARC_boot <- a10_obs_cc_medLARC_boot <- 
  a10_obs_sex_medLARC_boot <- a10_obs_debut_medLARC_boot <- a10_obs_mnppy_medLARC_boot <- 
  list()

#nonconverged <- c(17, 23, 27, 44, 50, 52, 75)   # place holder
 nonconverged <- c(61, 62, 65, 75, 97, 98, 101)   # place holder
converged <- (1:nreps)[-nonconverged]

for (bootrep in converged) {
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
  a10_nbc_medLARC_boot[[bootrep]] <-       readRDS(paste("../output/a10_nbc_medLARC_boot",repnum,".rda",sep=""))
  a10_obs_medLARC_boot[[bootrep]] <-       readRDS(paste("../output/a10_obs_medLARC_boot",repnum,".rda",sep=""))
  a10_obs_cc_medLARC_boot[[bootrep]] <-    readRDS(paste("../output/a10_obs_cc_medLARC_boot",repnum,".rda",sep=""))
  a10_obs_sex_medLARC_boot[[bootrep]] <-   readRDS(paste("../output/a10_obs_sex_medLARC_boot",repnum,".rda",sep=""))
  a10_obs_debut_medLARC_boot[[bootrep]] <- readRDS(paste("../output/a10_obs_debut_medLARC_boot",repnum,".rda",sep=""))
  a10_obs_mnppy_medLARC_boot[[bootrep]] <- readRDS(paste("../output/a10_obs_mnppy_medLARC_boot",repnum,".rda",sep=""))
  cat(bootrep, " ", sep="")  
}

####################################################################################
# Pull out core data and sum to pregs by year and age across scenarios

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
p_medL_nbc <- round(apply(a10_nbc_medLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_medL_obs <- round(apply(a10_obs_medLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_medL_obs_cc <- round(apply(a10_obs_cc_medLARC$n_preg_total_f, 2:3, sum)[,3:12],0)
p_medL_obs_sex <- round(apply(a10_obs_sex_medLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   
p_medL_obs_debut <- round(apply(a10_obs_debut_medLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   
p_medL_obs_mnppy <- round(apply(a10_obs_mnppy_medLARC$n_preg_total_f, 2:3, sum)[,3:12],0)   


####################################################################################
# Pull out core bootstrap data and sum to pregs by year and age across scenarios

p_minL_nbc_boot <- p_minL_obs_boot <- p_minL_obs_cc_boot <- 
  p_minL_obs_sex_boot <- p_minL_obs_debut_boot <- p_minL_obs_mnppy_boot <- 
  p_maxL_nbc_boot <- p_maxL_obs_boot <- p_maxL_obs_cc_boot <-
  p_maxL_obs_sex_boot <- p_maxL_obs_debut_boot <- p_maxL_obs_mnppy_boot <- 
  p_medL_nbc_boot <- p_medL_obs_boot <- p_medL_obs_cc_boot <-
  p_medL_obs_sex_boot <- p_medL_obs_debut_boot <- p_medL_obs_mnppy_boot <- 
  list()

for (bootnum in converged) {
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
  p_medL_nbc_boot[[bootnum]] <- round(apply(a10_nbc_medLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_medL_obs_boot[[bootnum]] <- round(apply(a10_obs_medLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_medL_obs_cc_boot[[bootnum]] <- round(apply(a10_obs_cc_medLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)
  p_medL_obs_sex_boot[[bootnum]] <- round(apply(a10_obs_sex_medLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  p_medL_obs_debut_boot[[bootnum]] <- round(apply(a10_obs_debut_medLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
  p_medL_obs_mnppy_boot[[bootnum]] <- round(apply(a10_obs_mnppy_medLARC_boot[[bootnum]]$n_preg_total_f, 2:3, sum)[,3:12],0)   
}

####################################################################################
#### bctype data  (removing 13yo)

bctype_in_wts <- readRDS("../output/a10_bctype_in_wts.rda")
bctype_in_wts <- bctype_in_wts[,2:6,,] # remove 13yo

bctype_in_yearprob <- apply(bctype_in_wts, c(3,4), sum) / apply(bctype_in_wts, 3, sum)
bctype_in_yearprob <- na_if(bctype_in_yearprob, 0)

####################################################################################
#### Fig 1: BC type by year, raw data

tiff("../output/Fig1.tif", height = 5*1200, width = 5*1200,
  units = "px", res = 1200, pointsize = 8,  compression = "lzw")

neworder <- c(1,2,4,5,3,8,7,6,9,10,11)
  
matplot(bctype_in_yearprob[,neworder], type='b', xaxt="n" , ylab= "Prop. reporting method",
        #main = "Method of birth control reported by females, YRBS", 
        ylim=c(0,0.8), pch=letters[1:length(bctypes_in)])
axis(1, 1:6, seq(2007, 2017, 2))
legend(c(1,6), c(0.8,0.6), c( 'a = no method', 
                    'b = condoms', 
                    'c = pills', 
                    'd = injectables',
                    'e = withdrawal', 
                    'f = other hormonal or LARC',
                    'g = other hormonal', 
                    'h = LARC', 
                    'i = other (2007-9 options)', 
                    'j = other (2011 options)', 
                    'k = withdrawal or other',
                    '          (2013-7 options)'
                  ),
  cex=0.9, text.col=c(1:6,1:5,5), col=c(1:6,1:5,"white"), 
  lty= 1:5, ncol=2)
abline(h=0, col="lightgray", lty=3)
dev.off()


####################################################################################
### Remove 13yo from main results

p_minL_nbc       <- p_minL_nbc[-1,]
p_minL_obs       <- p_minL_obs[-1,]
p_minL_obs_cc    <- p_minL_obs_cc[-1,]   
p_minL_obs_sex   <- p_minL_obs_sex[-1,]  
p_minL_obs_debut <- p_minL_obs_debut[-1,]
p_minL_obs_mnppy <- p_minL_obs_mnppy[-1,]
p_maxL_nbc       <- p_maxL_nbc[-1,]
p_maxL_obs       <- p_maxL_obs[-1,] 
p_maxL_obs_cc    <- p_maxL_obs_cc[-1,] 
p_maxL_obs_sex   <- p_maxL_obs_sex[-1,]
p_maxL_obs_debut <- p_maxL_obs_debut[-1,]
p_maxL_obs_mnppy <- p_maxL_obs_mnppy[-1,]
p_medL_nbc       <- p_medL_nbc[-1,]
p_medL_obs       <- p_medL_obs[-1,] 
p_medL_obs_cc    <- p_medL_obs_cc[-1,] 
p_medL_obs_sex   <- p_medL_obs_sex[-1,]
p_medL_obs_debut <- p_medL_obs_debut[-1,]
p_medL_obs_mnppy <- p_medL_obs_mnppy[-1,]

for (bootnum in converged) {
  p_minL_nbc_boot[[bootnum]]       <- p_minL_nbc_boot[[bootnum]][-1,]
  p_minL_obs_boot[[bootnum]]       <- p_minL_obs_boot[[bootnum]][-1,]
  p_minL_obs_cc_boot[[bootnum]]    <- p_minL_obs_cc_boot[[bootnum]][-1,]   
  p_minL_obs_sex_boot[[bootnum]]   <- p_minL_obs_sex_boot[[bootnum]][-1,]  
  p_minL_obs_debut_boot[[bootnum]] <- p_minL_obs_debut_boot[[bootnum]][-1,]
  p_minL_obs_mnppy_boot[[bootnum]] <- p_minL_obs_mnppy_boot[[bootnum]][-1,]
  p_maxL_nbc_boot[[bootnum]]       <- p_maxL_nbc_boot[[bootnum]][-1,]
  p_maxL_obs_boot[[bootnum]]       <- p_maxL_obs_boot[[bootnum]][-1,] 
  p_maxL_obs_cc_boot[[bootnum]]    <- p_maxL_obs_cc_boot[[bootnum]][-1,] 
  p_maxL_obs_sex_boot[[bootnum]]   <- p_maxL_obs_sex_boot[[bootnum]][-1,]
  p_maxL_obs_debut_boot[[bootnum]] <- p_maxL_obs_debut_boot[[bootnum]][-1,]
  p_maxL_obs_mnppy_boot[[bootnum]] <- p_maxL_obs_mnppy_boot[[bootnum]][-1,]
  p_medL_nbc_boot[[bootnum]]       <- p_medL_nbc_boot[[bootnum]][-1,]
  p_medL_obs_boot[[bootnum]]       <- p_medL_obs_boot[[bootnum]][-1,] 
  p_medL_obs_cc_boot[[bootnum]]    <- p_medL_obs_cc_boot[[bootnum]][-1,] 
  p_medL_obs_sex_boot[[bootnum]]   <- p_medL_obs_sex_boot[[bootnum]][-1,]
  p_medL_obs_debut_boot[[bootnum]] <- p_medL_obs_debut_boot[[bootnum]][-1,]
  p_medL_obs_mnppy_boot[[bootnum]] <- p_medL_obs_mnppy_boot[[bootnum]][-1,]
}


####################################################################################
### Summary numbers: 
###  Num and proportion of pregs averted
###  Total costs averted

num_pregs_averted_by_year_and_cause_minL <- rbind(colSums(p_minL_nbc - p_minL_obs_debut),
                                             colSums(p_minL_nbc - p_minL_obs_mnppy),
                                             colSums(p_minL_nbc - p_minL_obs_cc))
num_pregs_averted_by_year_minL <- colSums(num_pregs_averted_by_year_and_cause_minL)
num_pregs_averted_by_cause_minL <- rowSums(num_pregs_averted_by_year_and_cause_minL)
num_pregs_averted_minL <- sum(num_pregs_averted_by_year_minL)

prop_pregs_averted_by_year_and_cause_minL <- num_pregs_averted_by_year_and_cause_minL / colSums(p_minL_nbc)
prop_pregs_averted_by_year_minL <- num_pregs_averted_by_year_minL / colSums(p_minL_nbc)
prop_pregs_averted_by_cause_minL <- num_pregs_averted_by_cause_minL / sum(p_minL_nbc)
prop_pregs_averted_minL <- num_pregs_averted_minL / sum(p_minL_nbc)

num_pregs_averted_by_year_and_cause_maxL <- rbind(colSums(p_maxL_nbc - p_maxL_obs_debut),
                                                  colSums(p_maxL_nbc - p_maxL_obs_mnppy),
                                                  colSums(p_maxL_nbc - p_maxL_obs_cc))
num_pregs_averted_by_year_maxL <- colSums(num_pregs_averted_by_year_and_cause_maxL)
num_pregs_averted_by_cause_maxL <- rowSums(num_pregs_averted_by_year_and_cause_maxL)
num_pregs_averted_maxL <- sum(num_pregs_averted_by_year_maxL)

prop_pregs_averted_by_year_and_cause_maxL <- num_pregs_averted_by_year_and_cause_maxL / colSums(p_maxL_nbc)
prop_pregs_averted_by_year_maxL <- num_pregs_averted_by_year_maxL / colSums(p_maxL_nbc)
prop_pregs_averted_by_cause_maxL <- num_pregs_averted_by_cause_maxL / sum(p_maxL_nbc)
prop_pregs_averted_maxL <- num_pregs_averted_maxL / sum(p_maxL_nbc)

num_pregs_averted_by_year_and_cause_medL <- rbind(colSums(p_medL_nbc - p_medL_obs_debut),
                                                  colSums(p_medL_nbc - p_medL_obs_mnppy),
                                                  colSums(p_medL_nbc - p_medL_obs_cc))
num_pregs_averted_by_year_medL <- colSums(num_pregs_averted_by_year_and_cause_medL)
num_pregs_averted_by_cause_medL <- rowSums(num_pregs_averted_by_year_and_cause_medL)
num_pregs_averted_medL <- sum(num_pregs_averted_by_year_medL)

prop_pregs_averted_by_year_and_cause_medL <- num_pregs_averted_by_year_and_cause_medL / colSums(p_medL_nbc)
prop_pregs_averted_by_year_medL <- num_pregs_averted_by_year_medL / colSums(p_medL_nbc)
prop_pregs_averted_by_cause_medL <- num_pregs_averted_by_cause_medL / sum(p_medL_nbc)
prop_pregs_averted_medL <- num_pregs_averted_medL / sum(p_medL_nbc)

num_pregs_averted_minL
num_pregs_averted_maxL
num_pregs_averted_medL

prop_pregs_averted_minL
prop_pregs_averted_maxL
prop_pregs_averted_medL

num_pregs_averted_by_cause_minL
prop_pregs_averted_by_cause_minL
prop_pregs_averted_by_cause_minL/sum(prop_pregs_averted_minL)

num_pregs_averted_by_cause_maxL
prop_pregs_averted_by_cause_maxL
prop_pregs_averted_by_cause_maxL/sum(prop_pregs_averted_maxL)

num_pregs_averted_by_cause_medL
prop_pregs_averted_by_cause_medL
prop_pregs_averted_by_cause_medL/sum(prop_pregs_averted_medL)

sum(p_minL_nbc)
sum(p_maxL_nbc)
sum(p_medL_nbc)

costs3 <- rbind(costs,costs,costs)

costs_averted_by_year_and_cause_minL <- num_pregs_averted_by_year_and_cause_minL * costs3
costs_averted_by_year_minL <- colSums(costs_averted_by_year_and_cause_minL)
(costs_averted_by_cause_minL <- rowSums(costs_averted_by_year_and_cause_minL))
(costs_averted__minL <- sum(costs_averted_by_year_and_cause_minL))

costs_averted_by_year_and_cause_maxL <- num_pregs_averted_by_year_and_cause_maxL * costs3
costs_averted_by_year_maxL <- colSums(costs_averted_by_year_and_cause_maxL)
(costs_averted_by_cause_maxL <- rowSums(costs_averted_by_year_and_cause_maxL))
(costs_averted__maxL <- sum(costs_averted_by_year_and_cause_maxL))

costs_averted_by_year_and_cause_medL <- num_pregs_averted_by_year_and_cause_medL * costs3
costs_averted_by_year_medL <- colSums(costs_averted_by_year_and_cause_medL)
(costs_averted_by_cause_medL <- rowSums(costs_averted_by_year_and_cause_medL))
(costs_averted__medL <- sum(costs_averted_by_year_and_cause_medL))

# costs by age and cause - alas, code before now didn't leave room for this to be done with maximal efficiency

costs_averted_by_age_and_cause_minL <- rbind(
  colSums(costs*t((p_minL_nbc - p_minL_obs_debut))),
  colSums(costs*t((p_minL_nbc - p_minL_obs_mnppy))),
  colSums(costs*t((p_minL_nbc - p_minL_obs_cc)))
)
costs_averted_by_age_and_cause_minL <- rbind(colSums(costs_averted_by_age_and_cause_minL),
                                             costs_averted_by_age_and_cause_minL)

costs_averted_by_age_and_cause_maxL <- rbind(
  colSums(costs*t((p_maxL_nbc - p_maxL_obs_debut))),
  colSums(costs*t((p_maxL_nbc - p_maxL_obs_mnppy))),
  colSums(costs*t((p_maxL_nbc - p_maxL_obs_cc)))
)
costs_averted_by_age_and_cause_maxL <- rbind(colSums(costs_averted_by_age_and_cause_maxL),
                                              costs_averted_by_age_and_cause_maxL)

costs_averted_by_age_and_cause_medL <- rbind(
  colSums(costs*t((p_medL_nbc - p_medL_obs_debut))),
  colSums(costs*t((p_medL_nbc - p_medL_obs_mnppy))),
  colSums(costs*t((p_medL_nbc - p_medL_obs_cc)))
)
costs_averted_by_age_and_cause_medL <- rbind(colSums(costs_averted_by_age_and_cause_medL),
                                             costs_averted_by_age_and_cause_medL)

####################################################################################
### Fig 2: plot partition of proportion averted, minLARC and maxLARC

errbar <- function(x, up, low, ...) arrows(x, low, x, up, length=0.00, angle=90, code=3, ...)

num_pregs_averted_by_year_all_minL_boot <- t(sapply(converged, function(x) 
  colSums(p_minL_nbc_boot[[x]]) - colSums(p_minL_obs_boot[[x]])))
num_pregs_averted_by_year_cc_minL_boot <- t(sapply(converged, function(x) 
  colSums(p_minL_nbc_boot[[x]]) - colSums(p_minL_obs_cc_boot[[x]])))
num_pregs_averted_by_year_sex_minL_boot <- t(sapply(converged, function(x) 
  colSums(p_minL_nbc_boot[[x]]) - colSums(p_minL_obs_sex_boot[[x]])))
num_pregs_averted_by_year_debut_minL_boot <- t(sapply(converged, function(x) 
  colSums(p_minL_nbc_boot[[x]]) - colSums(p_minL_obs_debut_boot[[x]])))
num_pregs_averted_by_year_mnppy_minL_boot <- t(sapply(converged, function(x) 
  colSums(p_minL_nbc_boot[[x]]) - colSums(p_minL_obs_mnppy_boot[[x]])))

num_pregs_averted_by_year_all_minL_boot_mean <- colMeans(num_pregs_averted_by_year_all_minL_boot, na.rm=TRUE)
num_pregs_averted_by_year_all_minL_boot_ub <- colSums(num_pregs_averted_by_year_and_cause_minL) + apply(num_pregs_averted_by_year_all_minL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_all_minL_boot_lb <- colSums(num_pregs_averted_by_year_and_cause_minL) - apply(num_pregs_averted_by_year_all_minL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_debut_minL_boot_mean <- colMeans(num_pregs_averted_by_year_debut_minL_boot, na.rm=TRUE)
num_pregs_averted_by_year_debut_minL_boot_ub <- num_pregs_averted_by_year_and_cause_minL[1,] + apply(num_pregs_averted_by_year_debut_minL_boot, 2, sd)*1.96/length(converged)^0.25
num_pregs_averted_by_year_debut_minL_boot_lb <- num_pregs_averted_by_year_and_cause_minL[1,] - apply(num_pregs_averted_by_year_debut_minL_boot, 2, sd)*1.96/length(converged)^0.25

num_pregs_averted_by_year_mnppy_minL_boot_mean <- colMeans(num_pregs_averted_by_year_mnppy_minL_boot, na.rm=TRUE)
num_pregs_averted_by_year_mnppy_minL_boot_ub <- num_pregs_averted_by_year_and_cause_minL[2,] + apply(num_pregs_averted_by_year_mnppy_minL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_mnppy_minL_boot_lb <- num_pregs_averted_by_year_and_cause_minL[2,] - apply(num_pregs_averted_by_year_mnppy_minL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_cc_minL_boot_mean <- colMeans(num_pregs_averted_by_year_cc_minL_boot, na.rm=TRUE)
num_pregs_averted_by_year_cc_minL_boot_ub <- num_pregs_averted_by_year_and_cause_minL[3,] + apply(num_pregs_averted_by_year_cc_minL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_cc_minL_boot_lb <- num_pregs_averted_by_year_and_cause_minL[3,] - apply(num_pregs_averted_by_year_cc_minL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_all_maxL_boot <- t(sapply(converged, function(x) 
  colSums(p_maxL_nbc_boot[[x]]) - colSums(p_maxL_obs_boot[[x]])))
num_pregs_averted_by_year_cc_maxL_boot <- t(sapply(converged, function(x) 
  colSums(p_maxL_nbc_boot[[x]]) - colSums(p_maxL_obs_cc_boot[[x]])))
num_pregs_averted_by_year_sex_maxL_boot <- t(sapply(converged, function(x) 
  colSums(p_maxL_nbc_boot[[x]]) - colSums(p_maxL_obs_sex_boot[[x]])))
num_pregs_averted_by_year_debut_maxL_boot <- t(sapply(converged, function(x) 
  colSums(p_maxL_nbc_boot[[x]]) - colSums(p_maxL_obs_debut_boot[[x]])))
num_pregs_averted_by_year_mnppy_maxL_boot <- t(sapply(converged, function(x) 
  colSums(p_maxL_nbc_boot[[x]]) - colSums(p_maxL_obs_mnppy_boot[[x]])))

num_pregs_averted_by_year_all_maxL_boot_mean <- colMeans(num_pregs_averted_by_year_all_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_year_all_maxL_boot_ub <- colSums(num_pregs_averted_by_year_and_cause_maxL) + apply(num_pregs_averted_by_year_all_maxL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_all_maxL_boot_lb <- colSums(num_pregs_averted_by_year_and_cause_maxL) - apply(num_pregs_averted_by_year_all_maxL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_debut_maxL_boot_mean <- colMeans(num_pregs_averted_by_year_debut_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_year_debut_maxL_boot_ub <- num_pregs_averted_by_year_and_cause_maxL[1,] + apply(num_pregs_averted_by_year_debut_maxL_boot, 2, sd)*1.96/length(converged)^0.25
num_pregs_averted_by_year_debut_maxL_boot_lb <- num_pregs_averted_by_year_and_cause_maxL[1,] - apply(num_pregs_averted_by_year_debut_maxL_boot, 2, sd)*1.96/length(converged)^0.25

num_pregs_averted_by_year_mnppy_maxL_boot_mean <- colMeans(num_pregs_averted_by_year_mnppy_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_year_mnppy_maxL_boot_ub <- num_pregs_averted_by_year_and_cause_maxL[2,] + apply(num_pregs_averted_by_year_mnppy_maxL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_mnppy_maxL_boot_lb <- num_pregs_averted_by_year_and_cause_maxL[2,] - apply(num_pregs_averted_by_year_mnppy_maxL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_cc_maxL_boot_mean <- colMeans(num_pregs_averted_by_year_cc_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_year_cc_maxL_boot_ub <- num_pregs_averted_by_year_and_cause_maxL[3,] + apply(num_pregs_averted_by_year_cc_maxL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_cc_maxL_boot_lb <- num_pregs_averted_by_year_and_cause_maxL[3,] - apply(num_pregs_averted_by_year_cc_maxL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_all_medL_boot <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_boot[[x]])))
num_pregs_averted_by_year_cc_medL_boot <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_cc_boot[[x]])))
num_pregs_averted_by_year_sex_medL_boot <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_sex_boot[[x]])))
num_pregs_averted_by_year_debut_medL_boot <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_debut_boot[[x]])))
num_pregs_averted_by_year_mnppy_medL_boot <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_mnppy_boot[[x]])))

num_pregs_averted_by_year_all_medL_boot_mean <- colMeans(num_pregs_averted_by_year_all_medL_boot, na.rm=TRUE)
num_pregs_averted_by_year_all_medL_boot_ub <- colSums(num_pregs_averted_by_year_and_cause_medL) + apply(num_pregs_averted_by_year_all_medL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_all_medL_boot_lb <- colSums(num_pregs_averted_by_year_and_cause_medL) - apply(num_pregs_averted_by_year_all_medL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_debut_medL_boot_mean <- colMeans(num_pregs_averted_by_year_debut_medL_boot, na.rm=TRUE)
num_pregs_averted_by_year_debut_medL_boot_ub <- num_pregs_averted_by_year_and_cause_medL[1,] + apply(num_pregs_averted_by_year_debut_medL_boot, 2, sd)*1.96/length(converged)^0.25
num_pregs_averted_by_year_debut_medL_boot_lb <- num_pregs_averted_by_year_and_cause_medL[1,] - apply(num_pregs_averted_by_year_debut_medL_boot, 2, sd)*1.96/length(converged)^0.25

num_pregs_averted_by_year_mnppy_medL_boot_mean <- colMeans(num_pregs_averted_by_year_mnppy_medL_boot, na.rm=TRUE)
num_pregs_averted_by_year_mnppy_medL_boot_ub <- num_pregs_averted_by_year_and_cause_medL[2,] + apply(num_pregs_averted_by_year_mnppy_medL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_mnppy_medL_boot_lb <- num_pregs_averted_by_year_and_cause_medL[2,] - apply(num_pregs_averted_by_year_mnppy_medL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_year_cc_medL_boot_mean <- colMeans(num_pregs_averted_by_year_cc_medL_boot, na.rm=TRUE)
num_pregs_averted_by_year_cc_medL_boot_ub <- num_pregs_averted_by_year_and_cause_medL[3,] + apply(num_pregs_averted_by_year_cc_medL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_year_cc_medL_boot_lb <- num_pregs_averted_by_year_and_cause_medL[3,] - apply(num_pregs_averted_by_year_cc_medL_boot, 2, sd)*1.96/length(converged)^0.5

offset <- 0.02
tiff("../output/Fig2.tif", height = 5*1200, 5*1200,
    units = "px", res = 1200, pointsize = 8,  compression = "lzw")
plot((1:10)-offset, num_pregs_averted_by_year_medL, ylab="Num. pregs averted",
     ylim=c(-1e5,2e5), xaxt="n", xlab='year', type='b', pch=20, cex=0.75)
#plot(pavert_medL_obs, ylab="prop. pregs averted",
#     ylim=c(-0.05,0.3), xaxt="n", xlab='year', type='b')
abline(h=0)
axis(1, 1:10, 2008:2017)
legend(1.5, 2e5, c('total',
                   'attributable to delay in age at first sexual intercourse (SI)',
                   'attributable to changes in annual partner numbers',
                   'attributable to changes in contraception methods used'
),
  cex=0.9, text.col=c('black','red','darkgreen', 'blue'),
  col=c('black','red','darkgreen', 'blue'), pch = 20, ncol=1)

#points(pavert_medL_obs_debut, col='red', type='b')
#points(pavert_medL_obs_mnppy, col='darkgreen', type='b')
#points(pavert_medL_obs_cc, col='blue', type='b')
points((1:10)+offset, num_pregs_averted_by_year_and_cause_medL[1,], col='red', type='b', pch=20, cex=0.75)
points((1:10)-offset, num_pregs_averted_by_year_and_cause_medL[2,], col='darkgreen', type='b', pch=20, cex=0.75)
points((1:10)+offset, num_pregs_averted_by_year_and_cause_medL[3,], col='blue', type='b', pch=20, cex=0.75)

errbar((1:10)-offset, num_pregs_averted_by_year_all_medL_boot_lb, num_pregs_averted_by_year_all_medL_boot_ub, col='black')
errbar((1:10)+offset, num_pregs_averted_by_year_debut_medL_boot_lb, num_pregs_averted_by_year_debut_medL_boot_ub, col='red')
errbar((1:10)-offset, num_pregs_averted_by_year_mnppy_medL_boot_lb, num_pregs_averted_by_year_mnppy_medL_boot_ub, col='darkgreen')
errbar((1:10)+offset, num_pregs_averted_by_year_cc_medL_boot_lb, num_pregs_averted_by_year_cc_medL_boot_ub, col='blue')
axis(1, 1:10, labels=2008:2017)

dev.off()

####################################################################################
#### Plot partitions of pregnancies averted by cause, by age, minLARC

num_pregs_averted_by_age_and_cause_minL <- rbind(
  rowSums(p_minL_nbc - p_minL_obs_debut),
  rowSums(p_minL_nbc - p_minL_obs_mnppy),
  rowSums(p_minL_nbc - p_minL_obs_cc))

num_pregs_averted_by_age_and_cause_maxL <- rbind(
  rowSums(p_maxL_nbc - p_maxL_obs_debut),
  rowSums(p_maxL_nbc - p_maxL_obs_mnppy),
  rowSums(p_maxL_nbc - p_maxL_obs_cc))

num_pregs_averted_by_age_and_cause_medL <- rbind(
  rowSums(p_medL_nbc - p_medL_obs_debut),
  rowSums(p_medL_nbc - p_medL_obs_mnppy),
  rowSums(p_medL_nbc - p_medL_obs_cc))

# Here siwtch to only doing med, not min and max also

num_pregs_averted_by_age_and_cause_medL[3,5]  # 18-yo contraception
num_pregs_averted_by_age_and_cause_medL[3,5] / sum(p_medL_nbc)
num_pregs_averted_by_age_and_cause_medL[3,5] / sum(num_pregs_averted_by_age_and_cause_medL)

navert_medL_obs_boot_year <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_boot[[x]])))

navert_medL_obs_debut_boot_year <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_debut_boot[[x]])))

navert_medL_obs_mnppy_boot_year <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_mnppy_boot[[x]])))

navert_medL_obs_cc_boot_year <- t(sapply(converged, function(x) 
  colSums(p_medL_nbc_boot[[x]]) - colSums(p_medL_obs_cc_boot[[x]])))

navert_medL_obs_boot_age <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_boot[[x]])))

navert_medL_obs_debut_boot_age <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_debut_boot[[x]])))

navert_medL_obs_mnppy_boot_age <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_mnppy_boot[[x]])))

navert_medL_obs_cc_boot_age <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_cc_boot[[x]])))

navert_medL_obs_debut_boot_age_ub <- num_pregs_averted_by_age_and_cause_medL[1,] + 
            apply(navert_medL_obs_debut_boot_age, 2, sd)*1.96/length(converged)^0.25
navert_medL_obs_debut_boot_age_lb <- num_pregs_averted_by_age_and_cause_medL[1,] - 
            apply(navert_medL_obs_debut_boot_age, 2, sd)*1.96/length(converged)^0.25

navert_medL_obs_mnppy_boot_age_ub <- num_pregs_averted_by_age_and_cause_medL[2,] + 
            apply(navert_medL_obs_mnppy_boot_age, 2, sd)*1.96/length(converged)^0.5
navert_medL_obs_mnppy_boot_age_lb <- num_pregs_averted_by_age_and_cause_medL[2,] - 
            apply(navert_medL_obs_mnppy_boot_age, 2, sd)*1.96/length(converged)^0.5

navert_medL_obs_cc_boot_age_ub <- num_pregs_averted_by_age_and_cause_medL[3,] + 
            apply(navert_medL_obs_cc_boot_age, 2, sd)*1.96/length(converged)^0.5
navert_medL_obs_cc_boot_age_lb <- num_pregs_averted_by_age_and_cause_medL[3,] - 
            apply(navert_medL_obs_cc_boot_age, 2, sd)*1.96/length(converged)^0.5

offset <- 0.02
tiff("../output/Fig3.tif", height = 5*1200, 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")

#bp <- barplot(pregs_averted_by_age_and_cause_minL, xaxt='n', beside=TRUE,
#        col=c('red', 'darkgreen', 'blue'), 
#        xlab='age', ylab='No. of pregnancies averted',
#        main='No. of pregnancies averted by cause and by age, summed across years',
#        ylim=c(-4e4,20e4))

ages <- 14:18
plot(ages-offset, num_pregs_averted_by_age_and_cause_medL[1,],
              xlab='age', ylab='No. of pregnancies averted',
              #main='No. of pregnancies averted by cause and by age, summed across years',
              ylim=c(-8e4,25e4), type='b', pch=20, cex=0.75, col='red')

points(ages-offset, num_pregs_averted_by_age_and_cause_medL[2,], type='b', col='darkgreen', pch=20, cex=0.75)
points(ages+offset, num_pregs_averted_by_age_and_cause_medL[3,], type='b', col='blue', pch=20, cex=0.75)
errbar(ages-offset, navert_medL_obs_debut_boot_age_lb, navert_medL_obs_debut_boot_age_ub, col='red')
errbar(ages-offset, navert_medL_obs_mnppy_boot_age_lb, navert_medL_obs_mnppy_boot_age_ub, col='darkgreen')
errbar(ages+offset, navert_medL_obs_cc_boot_age_lb, navert_medL_obs_cc_boot_age_ub, col='blue')

legend(14, 2.5e5, c('attributable to delay in age at first sexual intercourse',
                    'attributable to changes in annual partner numbers',
                    'attributable to changes in contraception methods'),
        cex=0.9, text.col=c('red','darkgreen', 'blue'),
        fill = c('red','darkgreen', 'blue'),
        border = c('red','darkgreen', 'blue'))
abline(h=0, lty=2)
dev.off()

###############################################################################
# Numbers for boot CIs 

# By year and cause

# Back to med, min and max

round(cbind(num_pregs_averted_by_year_all_minL_boot_lb,
            colSums(num_pregs_averted_by_year_and_cause_minL),
            num_pregs_averted_by_year_all_minL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_debut_minL_boot_lb,
            num_pregs_averted_by_year_and_cause_minL[1,],
            num_pregs_averted_by_year_debut_minL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_mnppy_minL_boot_lb,
            num_pregs_averted_by_year_and_cause_minL[2,],
            num_pregs_averted_by_year_mnppy_minL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_cc_minL_boot_lb,
            num_pregs_averted_by_year_and_cause_minL[3,],
            num_pregs_averted_by_year_cc_minL_boot_ub),0)

round(cbind(num_pregs_averted_by_year_all_maxL_boot_lb,
            colSums(num_pregs_averted_by_year_and_cause_maxL),
            num_pregs_averted_by_year_all_maxL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_debut_maxL_boot_lb,
            num_pregs_averted_by_year_and_cause_maxL[1,],
            num_pregs_averted_by_year_debut_maxL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_mnppy_maxL_boot_lb,
            num_pregs_averted_by_year_and_cause_maxL[2,],
            num_pregs_averted_by_year_mnppy_maxL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_cc_maxL_boot_lb,
            num_pregs_averted_by_year_and_cause_maxL[3,],
            num_pregs_averted_by_year_cc_maxL_boot_ub),0)

round(cbind(num_pregs_averted_by_year_all_medL_boot_lb,
            colSums(num_pregs_averted_by_year_and_cause_medL),
            num_pregs_averted_by_year_all_medL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_debut_medL_boot_lb,
            num_pregs_averted_by_year_and_cause_medL[1,],
            num_pregs_averted_by_year_debut_medL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_mnppy_medL_boot_lb,
            num_pregs_averted_by_year_and_cause_medL[2,],
            num_pregs_averted_by_year_mnppy_medL_boot_ub),0)
round(cbind(num_pregs_averted_by_year_cc_medL_boot_lb,
            num_pregs_averted_by_year_and_cause_medL[3,],
            num_pregs_averted_by_year_cc_medL_boot_ub),0)

# By age and cause

num_pregs_averted_by_age_all_minL_boot <- t(sapply(converged, function(x) 
  rowSums(p_minL_nbc_boot[[x]]) - rowSums(p_minL_obs_boot[[x]])))
num_pregs_averted_by_age_cc_minL_boot <- t(sapply(converged, function(x) 
  rowSums(p_minL_nbc_boot[[x]]) - rowSums(p_minL_obs_cc_boot[[x]])))
num_pregs_averted_by_age_sex_minL_boot <- t(sapply(converged, function(x) 
  rowSums(p_minL_nbc_boot[[x]]) - rowSums(p_minL_obs_sex_boot[[x]])))
num_pregs_averted_by_age_debut_minL_boot <- t(sapply(converged, function(x) 
  rowSums(p_minL_nbc_boot[[x]]) - rowSums(p_minL_obs_debut_boot[[x]])))
num_pregs_averted_by_age_mnppy_minL_boot <- t(sapply(converged, function(x) 
  rowSums(p_minL_nbc_boot[[x]]) - rowSums(p_minL_obs_mnppy_boot[[x]])))

num_pregs_averted_by_age_all_minL_boot_mean <- colMeans(num_pregs_averted_by_age_all_minL_boot, na.rm=TRUE)
num_pregs_averted_by_age_all_minL_boot_ub <- colSums(num_pregs_averted_by_age_and_cause_minL) + apply(num_pregs_averted_by_age_all_minL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_all_minL_boot_lb <- colSums(num_pregs_averted_by_age_and_cause_minL) - apply(num_pregs_averted_by_age_all_minL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_debut_minL_boot_mean <- colMeans(num_pregs_averted_by_age_debut_minL_boot, na.rm=TRUE)
num_pregs_averted_by_age_debut_minL_boot_ub <- num_pregs_averted_by_age_and_cause_minL[1,] + apply(num_pregs_averted_by_age_debut_minL_boot, 2, sd)*1.96/length(converged)^0.25
num_pregs_averted_by_age_debut_minL_boot_lb <- num_pregs_averted_by_age_and_cause_minL[1,] - apply(num_pregs_averted_by_age_debut_minL_boot, 2, sd)*1.96/length(converged)^0.25

num_pregs_averted_by_age_mnppy_minL_boot_mean <- colMeans(num_pregs_averted_by_age_mnppy_minL_boot, na.rm=TRUE)
num_pregs_averted_by_age_mnppy_minL_boot_ub <- num_pregs_averted_by_age_and_cause_minL[2,] + apply(num_pregs_averted_by_age_mnppy_minL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_mnppy_minL_boot_lb <- num_pregs_averted_by_age_and_cause_minL[2,] - apply(num_pregs_averted_by_age_mnppy_minL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_cc_minL_boot_mean <- colMeans(num_pregs_averted_by_age_cc_minL_boot, na.rm=TRUE)
num_pregs_averted_by_age_cc_minL_boot_ub <- num_pregs_averted_by_age_and_cause_minL[3,] + apply(num_pregs_averted_by_age_cc_minL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_cc_minL_boot_lb <- num_pregs_averted_by_age_and_cause_minL[3,] - apply(num_pregs_averted_by_age_cc_minL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_all_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(p_maxL_nbc_boot[[x]]) - rowSums(p_maxL_obs_boot[[x]])))
num_pregs_averted_by_age_cc_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(p_maxL_nbc_boot[[x]]) - rowSums(p_maxL_obs_cc_boot[[x]])))
num_pregs_averted_by_age_sex_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(p_maxL_nbc_boot[[x]]) - rowSums(p_maxL_obs_sex_boot[[x]])))
num_pregs_averted_by_age_debut_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(p_maxL_nbc_boot[[x]]) - rowSums(p_maxL_obs_debut_boot[[x]])))
num_pregs_averted_by_age_mnppy_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(p_maxL_nbc_boot[[x]]) - rowSums(p_maxL_obs_mnppy_boot[[x]])))

num_pregs_averted_by_age_all_maxL_boot_mean <- colMeans(num_pregs_averted_by_age_all_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_age_all_maxL_boot_ub <- colSums(num_pregs_averted_by_age_and_cause_maxL) + apply(num_pregs_averted_by_age_all_maxL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_all_maxL_boot_lb <- colSums(num_pregs_averted_by_age_and_cause_maxL) - apply(num_pregs_averted_by_age_all_maxL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_debut_maxL_boot_mean <- colMeans(num_pregs_averted_by_age_debut_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_age_debut_maxL_boot_ub <- num_pregs_averted_by_age_and_cause_maxL[1,] + apply(num_pregs_averted_by_age_debut_maxL_boot, 2, sd)*1.96/length(converged)^0.25
num_pregs_averted_by_age_debut_maxL_boot_lb <- num_pregs_averted_by_age_and_cause_maxL[1,] - apply(num_pregs_averted_by_age_debut_maxL_boot, 2, sd)*1.96/length(converged)^0.25

num_pregs_averted_by_age_mnppy_maxL_boot_mean <- colMeans(num_pregs_averted_by_age_mnppy_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_age_mnppy_maxL_boot_ub <- num_pregs_averted_by_age_and_cause_maxL[2,] + apply(num_pregs_averted_by_age_mnppy_maxL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_mnppy_maxL_boot_lb <- num_pregs_averted_by_age_and_cause_maxL[2,] - apply(num_pregs_averted_by_age_mnppy_maxL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_cc_maxL_boot_mean <- colMeans(num_pregs_averted_by_age_cc_maxL_boot, na.rm=TRUE)
num_pregs_averted_by_age_cc_maxL_boot_ub <- num_pregs_averted_by_age_and_cause_maxL[3,] + apply(num_pregs_averted_by_age_cc_maxL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_cc_maxL_boot_lb <- num_pregs_averted_by_age_and_cause_maxL[3,] - apply(num_pregs_averted_by_age_cc_maxL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_all_medL_boot <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_boot[[x]])))
num_pregs_averted_by_age_cc_medL_boot <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_cc_boot[[x]])))
num_pregs_averted_by_age_sex_medL_boot <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_sex_boot[[x]])))
num_pregs_averted_by_age_debut_medL_boot <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_debut_boot[[x]])))
num_pregs_averted_by_age_mnppy_medL_boot <- t(sapply(converged, function(x) 
  rowSums(p_medL_nbc_boot[[x]]) - rowSums(p_medL_obs_mnppy_boot[[x]])))

num_pregs_averted_by_age_all_medL_boot_mean <- colMeans(num_pregs_averted_by_age_all_medL_boot, na.rm=TRUE)
num_pregs_averted_by_age_all_medL_boot_ub <- colSums(num_pregs_averted_by_age_and_cause_medL) + apply(num_pregs_averted_by_age_all_medL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_all_medL_boot_lb <- colSums(num_pregs_averted_by_age_and_cause_medL) - apply(num_pregs_averted_by_age_all_medL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_debut_medL_boot_mean <- colMeans(num_pregs_averted_by_age_debut_medL_boot, na.rm=TRUE)
num_pregs_averted_by_age_debut_medL_boot_ub <- num_pregs_averted_by_age_and_cause_medL[1,] + apply(num_pregs_averted_by_age_debut_medL_boot, 2, sd)*1.96/length(converged)^0.25
num_pregs_averted_by_age_debut_medL_boot_lb <- num_pregs_averted_by_age_and_cause_medL[1,] - apply(num_pregs_averted_by_age_debut_medL_boot, 2, sd)*1.96/length(converged)^0.25

num_pregs_averted_by_age_mnppy_medL_boot_mean <- colMeans(num_pregs_averted_by_age_mnppy_medL_boot, na.rm=TRUE)
num_pregs_averted_by_age_mnppy_medL_boot_ub <- num_pregs_averted_by_age_and_cause_medL[2,] + apply(num_pregs_averted_by_age_mnppy_medL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_mnppy_medL_boot_lb <- num_pregs_averted_by_age_and_cause_medL[2,] - apply(num_pregs_averted_by_age_mnppy_medL_boot, 2, sd)*1.96/length(converged)^0.5

num_pregs_averted_by_age_cc_medL_boot_mean <- colMeans(num_pregs_averted_by_age_cc_medL_boot, na.rm=TRUE)
num_pregs_averted_by_age_cc_medL_boot_ub <- num_pregs_averted_by_age_and_cause_medL[3,] + apply(num_pregs_averted_by_age_cc_medL_boot, 2, sd)*1.96/length(converged)^0.5
num_pregs_averted_by_age_cc_medL_boot_lb <- num_pregs_averted_by_age_and_cause_medL[3,] - apply(num_pregs_averted_by_age_cc_medL_boot, 2, sd)*1.96/length(converged)^0.5

# 

round(cbind(num_pregs_averted_by_age_all_minL_boot_lb,
            colSums(num_pregs_averted_by_age_and_cause_minL),
            num_pregs_averted_by_age_all_minL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_debut_minL_boot_lb,
            num_pregs_averted_by_age_and_cause_minL[1,],
            num_pregs_averted_by_age_debut_minL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_mnppy_minL_boot_lb,
            num_pregs_averted_by_age_and_cause_minL[2,],
            num_pregs_averted_by_age_mnppy_minL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_cc_minL_boot_lb,
            num_pregs_averted_by_age_and_cause_minL[3,],
            num_pregs_averted_by_age_cc_minL_boot_ub),0)

round(cbind(num_pregs_averted_by_age_all_maxL_boot_lb,
            colSums(num_pregs_averted_by_age_and_cause_maxL),
            num_pregs_averted_by_age_all_maxL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_debut_maxL_boot_lb,
            num_pregs_averted_by_age_and_cause_maxL[1,],
            num_pregs_averted_by_age_debut_maxL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_mnppy_maxL_boot_lb,
            num_pregs_averted_by_age_and_cause_maxL[2,],
            num_pregs_averted_by_age_mnppy_maxL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_cc_maxL_boot_lb,
            num_pregs_averted_by_age_and_cause_maxL[3,],
            num_pregs_averted_by_age_cc_maxL_boot_ub),0)

round(cbind(num_pregs_averted_by_age_all_medL_boot_lb,
            colSums(num_pregs_averted_by_age_and_cause_medL),
            num_pregs_averted_by_age_all_medL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_debut_medL_boot_lb,
            num_pregs_averted_by_age_and_cause_medL[1,],
            num_pregs_averted_by_age_debut_medL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_mnppy_medL_boot_lb,
            num_pregs_averted_by_age_and_cause_medL[2,],
            num_pregs_averted_by_age_mnppy_medL_boot_ub),0)
round(cbind(num_pregs_averted_by_age_cc_medL_boot_lb,
            num_pregs_averted_by_age_and_cause_medL[3,],
            num_pregs_averted_by_age_cc_medL_boot_ub),0)

# age costs

costs_averted_by_age_all_minL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_minL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_minL_obs_boot[[x]])))))

costs_averted_by_age_cc_minL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_minL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_minL_obs_cc_boot[[x]])))))

costs_averted_by_age_sex_minL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_minL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_minL_obs_sex_boot[[x]])))))

costs_averted_by_age_debut_minL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_minL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_minL_obs_debut_boot[[x]])))))

costs_averted_by_age_mnppy_minL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_minL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_minL_obs_mnppy_boot[[x]])))))

costs_averted_by_age_all_minL_boot_mean <- colMeans(costs_averted_by_age_all_minL_boot, na.rm=TRUE)
costs_averted_by_age_all_minL_boot_ub <- costs_averted_by_age_and_cause_minL[1,] + apply(costs_averted_by_age_all_minL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_all_minL_boot_lb <- costs_averted_by_age_and_cause_minL[1,] - apply(costs_averted_by_age_all_minL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_debut_minL_boot_mean <- colMeans(costs_averted_by_age_debut_minL_boot, na.rm=TRUE)
costs_averted_by_age_debut_minL_boot_ub <- costs_averted_by_age_and_cause_minL[2,] + apply(costs_averted_by_age_debut_minL_boot, 2, sd)*1.96/length(converged)^0.25
costs_averted_by_age_debut_minL_boot_lb <- costs_averted_by_age_and_cause_minL[2,] - apply(costs_averted_by_age_debut_minL_boot, 2, sd)*1.96/length(converged)^0.25

costs_averted_by_age_mnppy_minL_boot_mean <- colMeans(costs_averted_by_age_mnppy_minL_boot, na.rm=TRUE)
costs_averted_by_age_mnppy_minL_boot_ub <- costs_averted_by_age_and_cause_minL[3,] + apply(costs_averted_by_age_mnppy_minL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_mnppy_minL_boot_lb <- costs_averted_by_age_and_cause_minL[3,] - apply(costs_averted_by_age_mnppy_minL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_cc_minL_boot_mean <- colMeans(costs_averted_by_age_cc_minL_boot, na.rm=TRUE)
costs_averted_by_age_cc_minL_boot_ub <- costs_averted_by_age_and_cause_minL[4,] + apply(costs_averted_by_age_cc_minL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_cc_minL_boot_lb <- costs_averted_by_age_and_cause_minL[4,] - apply(costs_averted_by_age_cc_minL_boot, 2, sd)*1.96/length(converged)^0.5


costs_averted_by_age_all_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_maxL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_maxL_obs_boot[[x]])))))

costs_averted_by_age_cc_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_maxL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_maxL_obs_cc_boot[[x]])))))

costs_averted_by_age_sex_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_maxL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_maxL_obs_sex_boot[[x]])))))

costs_averted_by_age_debut_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_maxL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_maxL_obs_debut_boot[[x]])))))

costs_averted_by_age_mnppy_maxL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_maxL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_maxL_obs_mnppy_boot[[x]])))))

costs_averted_by_age_all_maxL_boot_mean <- colMeans(costs_averted_by_age_all_maxL_boot, na.rm=TRUE)
costs_averted_by_age_all_maxL_boot_ub <- costs_averted_by_age_and_cause_maxL[1,] + apply(costs_averted_by_age_all_maxL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_all_maxL_boot_lb <- costs_averted_by_age_and_cause_maxL[1,] - apply(costs_averted_by_age_all_maxL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_debut_maxL_boot_mean <- colMeans(costs_averted_by_age_debut_maxL_boot, na.rm=TRUE)
costs_averted_by_age_debut_maxL_boot_ub <- costs_averted_by_age_and_cause_maxL[2,] + apply(costs_averted_by_age_debut_maxL_boot, 2, sd)*1.96/length(converged)^0.25
costs_averted_by_age_debut_maxL_boot_lb <- costs_averted_by_age_and_cause_maxL[2,] - apply(costs_averted_by_age_debut_maxL_boot, 2, sd)*1.96/length(converged)^0.25

costs_averted_by_age_mnppy_maxL_boot_mean <- colMeans(costs_averted_by_age_mnppy_maxL_boot, na.rm=TRUE)
costs_averted_by_age_mnppy_maxL_boot_ub <- costs_averted_by_age_and_cause_maxL[3,] + apply(costs_averted_by_age_mnppy_maxL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_mnppy_maxL_boot_lb <- costs_averted_by_age_and_cause_maxL[3,] - apply(costs_averted_by_age_mnppy_maxL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_cc_maxL_boot_mean <- colMeans(costs_averted_by_age_cc_maxL_boot, na.rm=TRUE)
costs_averted_by_age_cc_maxL_boot_ub <- costs_averted_by_age_and_cause_maxL[4,] + apply(costs_averted_by_age_cc_maxL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_cc_maxL_boot_lb <- costs_averted_by_age_and_cause_maxL[4,] - apply(costs_averted_by_age_cc_maxL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_all_medL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_medL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_medL_obs_boot[[x]])))))

costs_averted_by_age_cc_medL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_medL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_medL_obs_cc_boot[[x]])))))

costs_averted_by_age_sex_medL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_medL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_medL_obs_sex_boot[[x]])))))

costs_averted_by_age_debut_medL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_medL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_medL_obs_debut_boot[[x]])))))

costs_averted_by_age_mnppy_medL_boot <- t(sapply(converged, function(x) 
  rowSums(t(costs*t(p_medL_nbc_boot[[x]]))) - rowSums(t(costs*t(p_medL_obs_mnppy_boot[[x]])))))

costs_averted_by_age_all_medL_boot_mean <- colMeans(costs_averted_by_age_all_medL_boot, na.rm=TRUE)
costs_averted_by_age_all_medL_boot_ub <- costs_averted_by_age_and_cause_medL[1,] + apply(costs_averted_by_age_all_medL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_all_medL_boot_lb <- costs_averted_by_age_and_cause_medL[1,] - apply(costs_averted_by_age_all_medL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_debut_medL_boot_mean <- colMeans(costs_averted_by_age_debut_medL_boot, na.rm=TRUE)
costs_averted_by_age_debut_medL_boot_ub <- costs_averted_by_age_and_cause_medL[2,] + apply(costs_averted_by_age_debut_medL_boot, 2, sd)*1.96/length(converged)^0.25
costs_averted_by_age_debut_medL_boot_lb <- costs_averted_by_age_and_cause_medL[2,] - apply(costs_averted_by_age_debut_medL_boot, 2, sd)*1.96/length(converged)^0.25

costs_averted_by_age_mnppy_medL_boot_mean <- colMeans(costs_averted_by_age_mnppy_medL_boot, na.rm=TRUE)
costs_averted_by_age_mnppy_medL_boot_ub <- costs_averted_by_age_and_cause_medL[3,] + apply(costs_averted_by_age_mnppy_medL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_mnppy_medL_boot_lb <- costs_averted_by_age_and_cause_medL[3,] - apply(costs_averted_by_age_mnppy_medL_boot, 2, sd)*1.96/length(converged)^0.5

costs_averted_by_age_cc_medL_boot_mean <- colMeans(costs_averted_by_age_cc_medL_boot, na.rm=TRUE)
costs_averted_by_age_cc_medL_boot_ub <- costs_averted_by_age_and_cause_medL[4,] + apply(costs_averted_by_age_cc_medL_boot, 2, sd)*1.96/length(converged)^0.5
costs_averted_by_age_cc_medL_boot_lb <- costs_averted_by_age_and_cause_medL[4,] - apply(costs_averted_by_age_cc_medL_boot, 2, sd)*1.96/length(converged)^0.5


round(cbind(costs_averted_by_age_all_minL_boot_lb,
            costs_averted_by_age_and_cause_minL[1,],
            costs_averted_by_age_all_minL_boot_ub),0)
round(cbind(costs_averted_by_age_debut_minL_boot_lb,
            costs_averted_by_age_and_cause_minL[2,],
            costs_averted_by_age_debut_minL_boot_ub),0)
round(cbind(costs_averted_by_age_mnppy_minL_boot_lb,
            costs_averted_by_age_and_cause_minL[3,],
            costs_averted_by_age_mnppy_minL_boot_ub),0)
round(cbind(costs_averted_by_age_cc_minL_boot_lb,
            costs_averted_by_age_and_cause_minL[4,],
            costs_averted_by_age_cc_minL_boot_ub),0)

round(cbind(costs_averted_by_age_all_maxL_boot_lb,
            costs_averted_by_age_and_cause_maxL[1,],
            costs_averted_by_age_all_maxL_boot_ub),0)
round(cbind(costs_averted_by_age_debut_maxL_boot_lb,
            costs_averted_by_age_and_cause_maxL[2,],
            costs_averted_by_age_debut_maxL_boot_ub),0)
round(cbind(costs_averted_by_age_mnppy_maxL_boot_lb,
            costs_averted_by_age_and_cause_maxL[3,],
            costs_averted_by_age_mnppy_maxL_boot_ub),0)
round(cbind(costs_averted_by_age_cc_maxL_boot_lb,
            costs_averted_by_age_and_cause_maxL[4,],
            costs_averted_by_age_cc_maxL_boot_ub),0)

round(cbind(costs_averted_by_age_all_medL_boot_lb,
            costs_averted_by_age_and_cause_medL[1,],
            costs_averted_by_age_all_medL_boot_ub),0)
round(cbind(costs_averted_by_age_debut_medL_boot_lb,
            costs_averted_by_age_and_cause_medL[2,],
            costs_averted_by_age_debut_medL_boot_ub),0)
round(cbind(costs_averted_by_age_mnppy_medL_boot_lb,
            costs_averted_by_age_and_cause_medL[3,],
            costs_averted_by_age_mnppy_medL_boot_ub),0)
round(cbind(costs_averted_by_age_cc_medL_boot_lb,
            costs_averted_by_age_and_cause_medL[4,],
            costs_averted_by_age_cc_medL_boot_ub),0)


###############################################################################
# Isolating LARC

p_onlyL_minL_all <- round(apply(a10_obs_cc_onlyLARC_minL_from_all$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_minL_wdl <- round(apply(a10_obs_cc_onlyLARC_minL_from_wdl$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_minL_cdm <- round(apply(a10_obs_cc_onlyLARC_minL_from_cdm$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_minL_pil <- round(apply(a10_obs_cc_onlyLARC_minL_from_pil$n_preg_total_f, 2:3, sum)[,3:12],0)

p_onlyL_maxL_all <- round(apply(a10_obs_cc_onlyLARC_maxL_from_all$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_maxL_wdl <- round(apply(a10_obs_cc_onlyLARC_maxL_from_wdl$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_maxL_cdm <- round(apply(a10_obs_cc_onlyLARC_maxL_from_cdm$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_maxL_pil <- round(apply(a10_obs_cc_onlyLARC_maxL_from_pil$n_preg_total_f, 2:3, sum)[,3:12],0)

p_onlyL_medL_all <- round(apply(a10_obs_cc_onlyLARC_medL_from_all$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_medL_wdl <- round(apply(a10_obs_cc_onlyLARC_medL_from_wdl$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_medL_cdm <- round(apply(a10_obs_cc_onlyLARC_medL_from_cdm$n_preg_total_f, 2:3, sum)[,3:12],0)
p_onlyL_medL_pil <- round(apply(a10_obs_cc_onlyLARC_medL_from_pil$n_preg_total_f, 2:3, sum)[,3:12],0)

p_onlyL_minL_all <- p_onlyL_minL_all[-1,] # Remove 13-yo
p_onlyL_minL_wdl <- p_onlyL_minL_wdl[-1,] 
p_onlyL_minL_cdm <- p_onlyL_minL_cdm[-1,]
p_onlyL_minL_pil <- p_onlyL_minL_pil[-1,] 

p_onlyL_maxL_all <- p_onlyL_maxL_all[-1,] # Remove 13-yo
p_onlyL_maxL_wdl <- p_onlyL_maxL_wdl[-1,] 
p_onlyL_maxL_cdm <- p_onlyL_maxL_cdm[-1,]
p_onlyL_maxL_pil <- p_onlyL_maxL_pil[-1,] 

p_onlyL_medL_all <- p_onlyL_medL_all[-1,] # Remove 13-yo
p_onlyL_medL_wdl <- p_onlyL_medL_wdl[-1,] 
p_onlyL_medL_cdm <- p_onlyL_medL_cdm[-1,]
p_onlyL_medL_pil <- p_onlyL_medL_pil[-1,] 


# Total averted
(num_pregs_averted_onlyL_minL_all <- sum(p_minL_nbc - p_onlyL_minL_all))
(num_pregs_averted_onlyL_minL_wdl <- sum(p_minL_nbc - p_onlyL_minL_wdl))
(num_pregs_averted_onlyL_minL_cdm <- sum(p_minL_nbc - p_onlyL_minL_cdm))
(num_pregs_averted_onlyL_minL_pil <- sum(p_minL_nbc - p_onlyL_minL_pil))

(num_pregs_averted_onlyL_maxL_all <- sum(p_maxL_nbc - p_onlyL_maxL_all))
(num_pregs_averted_onlyL_maxL_wdl <- sum(p_maxL_nbc - p_onlyL_maxL_wdl))
(num_pregs_averted_onlyL_maxL_cdm <- sum(p_maxL_nbc - p_onlyL_maxL_cdm))
(num_pregs_averted_onlyL_maxL_pil <- sum(p_maxL_nbc - p_onlyL_maxL_pil))

(num_pregs_averted_onlyL_medL_all <- sum(p_medL_nbc - p_onlyL_medL_all))
(num_pregs_averted_onlyL_medL_wdl <- sum(p_medL_nbc - p_onlyL_medL_wdl))
(num_pregs_averted_onlyL_medL_cdm <- sum(p_medL_nbc - p_onlyL_medL_cdm))
(num_pregs_averted_onlyL_medL_pil <- sum(p_medL_nbc - p_onlyL_medL_pil))

# % averted
(prop_pregs_averted_onlyL_minL_all <- sum(p_minL_nbc - p_onlyL_minL_all) / sum(p_minL_nbc))
(prop_pregs_averted_onlyL_minL_wdl <- sum(p_minL_nbc - p_onlyL_minL_wdl) / sum(p_minL_nbc))
(prop_pregs_averted_onlyL_minL_cdm <- sum(p_minL_nbc - p_onlyL_minL_cdm) / sum(p_minL_nbc))
(prop_pregs_averted_onlyL_minL_pil <- sum(p_minL_nbc - p_onlyL_minL_pil) / sum(p_minL_nbc))

(prop_pregs_averted_onlyL_maxL_all <- sum(p_maxL_nbc - p_onlyL_maxL_all) / sum(p_maxL_nbc))
(prop_pregs_averted_onlyL_maxL_wdl <- sum(p_maxL_nbc - p_onlyL_maxL_wdl) / sum(p_maxL_nbc))
(prop_pregs_averted_onlyL_maxL_cdm <- sum(p_maxL_nbc - p_onlyL_maxL_cdm) / sum(p_maxL_nbc))
(prop_pregs_averted_onlyL_maxL_pil <- sum(p_maxL_nbc - p_onlyL_maxL_pil) / sum(p_maxL_nbc))

(prop_pregs_averted_onlyL_medL_all <- sum(p_medL_nbc - p_onlyL_medL_all) / sum(p_medL_nbc))
(prop_pregs_averted_onlyL_medL_wdl <- sum(p_medL_nbc - p_onlyL_medL_wdl) / sum(p_medL_nbc))
(prop_pregs_averted_onlyL_medL_cdm <- sum(p_medL_nbc - p_onlyL_medL_cdm) / sum(p_medL_nbc))
(prop_pregs_averted_onlyL_medL_pil <- sum(p_medL_nbc - p_onlyL_medL_pil) / sum(p_medL_nbc))

# by year and by age

num_pregs_averted_by_age_onlyL_minL_all <- rowSums(p_minL_nbc) - rowSums(p_onlyL_minL_all)
num_pregs_averted_by_age_onlyL_minL_wdl <- rowSums(p_minL_nbc) - rowSums(p_onlyL_minL_wdl)
num_pregs_averted_by_age_onlyL_minL_cdm <- rowSums(p_minL_nbc) - rowSums(p_onlyL_minL_cdm)
num_pregs_averted_by_age_onlyL_minL_pil <- rowSums(p_minL_nbc) - rowSums(p_onlyL_minL_pil)
num_pregs_averted_by_age_onlyL_maxL_all <- rowSums(p_maxL_nbc) - rowSums(p_onlyL_maxL_all)
num_pregs_averted_by_age_onlyL_maxL_wdl <- rowSums(p_maxL_nbc) - rowSums(p_onlyL_maxL_wdl)
num_pregs_averted_by_age_onlyL_maxL_cdm <- rowSums(p_maxL_nbc) - rowSums(p_onlyL_maxL_cdm)
num_pregs_averted_by_age_onlyL_maxL_pil <- rowSums(p_maxL_nbc) - rowSums(p_onlyL_maxL_pil)
num_pregs_averted_by_age_onlyL_medL_all <- rowSums(p_medL_nbc) - rowSums(p_onlyL_medL_all)
num_pregs_averted_by_age_onlyL_medL_wdl <- rowSums(p_medL_nbc) - rowSums(p_onlyL_medL_wdl)
num_pregs_averted_by_age_onlyL_medL_cdm <- rowSums(p_medL_nbc) - rowSums(p_onlyL_medL_cdm)
num_pregs_averted_by_age_onlyL_medL_pil <- rowSums(p_medL_nbc) - rowSums(p_onlyL_medL_pil)

num_pregs_averted_by_year_onlyL_minL_all <- colSums(p_minL_nbc) - colSums(p_onlyL_minL_all)
num_pregs_averted_by_year_onlyL_minL_wdl <- colSums(p_minL_nbc) - colSums(p_onlyL_minL_wdl)
num_pregs_averted_by_year_onlyL_minL_cdm <- colSums(p_minL_nbc) - colSums(p_onlyL_minL_cdm)
num_pregs_averted_by_year_onlyL_minL_pil <- colSums(p_minL_nbc) - colSums(p_onlyL_minL_pil)
num_pregs_averted_by_year_onlyL_maxL_all <- colSums(p_maxL_nbc) - colSums(p_onlyL_maxL_all)
num_pregs_averted_by_year_onlyL_maxL_wdl <- colSums(p_maxL_nbc) - colSums(p_onlyL_maxL_wdl)
num_pregs_averted_by_year_onlyL_maxL_cdm <- colSums(p_maxL_nbc) - colSums(p_onlyL_maxL_cdm)
num_pregs_averted_by_year_onlyL_maxL_pil <- colSums(p_maxL_nbc) - colSums(p_onlyL_maxL_pil)
num_pregs_averted_by_year_onlyL_medL_all <- colSums(p_medL_nbc) - colSums(p_onlyL_medL_all)
num_pregs_averted_by_year_onlyL_medL_wdl <- colSums(p_medL_nbc) - colSums(p_onlyL_medL_wdl)
num_pregs_averted_by_year_onlyL_medL_cdm <- colSums(p_medL_nbc) - colSums(p_onlyL_medL_cdm)
num_pregs_averted_by_year_onlyL_medL_pil <- colSums(p_medL_nbc) - colSums(p_onlyL_medL_pil)

sum(costs * num_pregs_averted_by_year_onlyL_minL_all)
sum(costs * num_pregs_averted_by_year_onlyL_minL_wdl)
sum(costs * num_pregs_averted_by_year_onlyL_minL_cdm)
sum(costs * num_pregs_averted_by_year_onlyL_minL_pil)
sum(costs * num_pregs_averted_by_year_onlyL_maxL_all)
sum(costs * num_pregs_averted_by_year_onlyL_maxL_wdl)
sum(costs * num_pregs_averted_by_year_onlyL_maxL_cdm)
sum(costs * num_pregs_averted_by_year_onlyL_maxL_pil)
sum(costs * num_pregs_averted_by_year_onlyL_medL_all)
sum(costs * num_pregs_averted_by_year_onlyL_medL_wdl)
sum(costs * num_pregs_averted_by_year_onlyL_medL_cdm)
sum(costs * num_pregs_averted_by_year_onlyL_medL_pil)

num_pregs_averted_onlyL_minL_all / num_pregs_averted_minL
num_pregs_averted_onlyL_minL_wdl / num_pregs_averted_minL
num_pregs_averted_onlyL_minL_cdm / num_pregs_averted_minL
num_pregs_averted_onlyL_minL_pil / num_pregs_averted_minL
num_pregs_averted_onlyL_maxL_all / num_pregs_averted_maxL
num_pregs_averted_onlyL_maxL_wdl / num_pregs_averted_maxL
num_pregs_averted_onlyL_maxL_cdm / num_pregs_averted_maxL
num_pregs_averted_onlyL_maxL_pil / num_pregs_averted_maxL
num_pregs_averted_onlyL_medL_all / num_pregs_averted_medL
num_pregs_averted_onlyL_medL_wdl / num_pregs_averted_medL
num_pregs_averted_onlyL_medL_cdm / num_pregs_averted_medL
num_pregs_averted_onlyL_medL_pil / num_pregs_averted_medL

colSums(costs*t(p_minL_nbc - p_onlyL_minL_all))
colSums(costs*t(p_minL_nbc - p_onlyL_minL_wdl))
colSums(costs*t(p_minL_nbc - p_onlyL_minL_cdm))
colSums(costs*t(p_minL_nbc - p_onlyL_minL_pil))
colSums(costs*t(p_maxL_nbc - p_onlyL_maxL_all))
colSums(costs*t(p_maxL_nbc - p_onlyL_maxL_wdl))
colSums(costs*t(p_maxL_nbc - p_onlyL_maxL_cdm))
colSums(costs*t(p_maxL_nbc - p_onlyL_maxL_pil))
colSums(costs*t(p_medL_nbc - p_onlyL_medL_all))
colSums(costs*t(p_medL_nbc - p_onlyL_medL_wdl))
colSums(costs*t(p_medL_nbc - p_onlyL_medL_cdm))
colSums(costs*t(p_medL_nbc - p_onlyL_medL_pil))

tiff("../output/Fig4.tif", height = 5*1200, 10*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")

par(mfrow=c(1,2))
plot(1:10, num_pregs_averted_by_year_onlyL_medL_all, type='b', ylim=c(0,3e4), 
     xaxt = 'n', lty=1, xlab = "Year", lwd=1.5, ylab = "Num. pregnancies averted")
axis(1, at = 1:10, labels=2008:2017)
   points(1:6, num_pregs_averted_by_year_onlyL_minL_all[1:6], type='b', col='red', lty=1, lwd=1.5)
points(6:10, num_pregs_averted_by_year_onlyL_minL_all[6:10], type='b', col='red', lty=2, lwd=1.5)

points(1:10, num_pregs_averted_by_year_onlyL_medL_wdl, type='b', col='black', lty=1, lwd=1.5)
points(1:6, num_pregs_averted_by_year_onlyL_minL_wdl[1:6], type='b', col='blue', lty=1, lwd=1.5)
points(6:10, num_pregs_averted_by_year_onlyL_minL_wdl[6:10], type='b', col='blue', lty=2, lwd=1.5)

points(1:10, num_pregs_averted_by_year_onlyL_medL_cdm, type='b', col='black', lty=1, lwd=1.5)
points(1:6, num_pregs_averted_by_year_onlyL_minL_cdm[1:6], type='b', col='blue', lty=1, lwd=1.5)
points(6:10, num_pregs_averted_by_year_onlyL_minL_cdm[6:10], type='b', col='blue', lty=2, lwd=1.5)

points(1:10, num_pregs_averted_by_year_onlyL_medL_pil, type='b', col='black', lty=1, lwd=1.5)
points(1:6, num_pregs_averted_by_year_onlyL_minL_pil[1:6], type='b', col='green', lty=1, lwd=1.5)
points(6:10, num_pregs_averted_by_year_onlyL_minL_pil[6:10], type='b', col='green', lty=2, lwd=1.5)



points(1:6, num_pregs_averted_by_year_onlyL_maxL_cdm[1:6], type='b', col='brown', lty=1, lwd=1.5)
points(6:10, num_pregs_averted_by_year_onlyL_maxL_cdm[6:10], type='b', col='brown', lty=2, lwd=1.5)
legend(1,8e4, c('min-LARC (no LARC use before 2012), LARC use replaces withdrawal',
                'max-LARC (no LARC use before 2008), LARC use replaces withdrawal',
                'min-LARC (no LARC use before 2012), LARC use replaces condoms',
                'max-LARC (no LARC use before 2008), LARC use replaces condoms'),
      col=c('black','red','blue','brown'), lty=1
       )
mtext('A)', side=3, line=1.0, at=-0.2, cex=1.5)

plot(num_pregs_averted_by_age_onlyL_minL_wdl, 
     type='b', ylim=c(0,8e4), xaxt = 'n', xlab = "Age", lwd=1.5, 
     ylab = "Num. pregnancies averted")
axis(1, at = 1:5, labels=14:18)
points(num_pregs_averted_by_age_onlyL_maxL_wdl, type='b', col='red', lwd=1.5)
points(num_pregs_averted_by_age_onlyL_minL_cdm, type='b', col='blue', lwd=1.5)
points(num_pregs_averted_by_age_onlyL_maxL_cdm, type='b', col='brown', lwd=1.5)
legend(1,8e4, c('min-LARC (no LARC use before 2012), LARC use replaces withdrawal',
                'max-LARC (no LARC use before 2008), LARC use replaces withdrawal',
                'min-LARC (no LARC use before 2012), LARC use replaces condoms',
                'max-LARC (no LARC use before 2008), LARC use replaces condoms'),
       col=c('black','red','blue','brown'), lty=1
)
mtext('B)', side=3, line=1.0, at=0.45, cex=1.5)

dev.off()

## Comparison to actual births

nvss_births <- c(273795, 266438, 246720, 217722, 192137,
                 175405, 154058, 138934, 127192, 115755, 104677)

model_pregs <- colSums(round(apply(
  a10_obs_medLARC$n_preg_total_f, 2:3, sum)[,2:12],0)) 
  # Same as p_maxL_obs but with 2007 included

btp_ratio_flat <- rep(nvss_births[1]/model_pregs[1], 11)
btp_ratio_gutt <- c(0.59, 0.59, 0.59, 0.59, 0.59, 
                          0.60, 0.61, 0.61, 0.61, 0.61, 0.61)

model_births_btp_flat <- model_pregs * btp_ratio_flat
model_births_btp_gutt  <- model_pregs * btp_ratio_gutt *
                            (btp_ratio_flat[1]/btp_ratio_gutt[1])

model_pregs_coital_decline <- colSums(round(apply(
  a10_coital_decline$n_preg_total_f, 2:3, sum)[,2:12],0)) 
model_births_btp_coit  <- model_pregs_coital_decline * btp_ratio_gutt *
  (btp_ratio_flat[1]/btp_ratio_gutt[1])

num_avert_nvss <- nvss_births[1] - nvss_births
num_avert_obs_behav <- nvss_births[1] - model_births_btp_gutt
num_avert_obs_coital <- nvss_births[1] - model_births_btp_coit

sum(num_avert_nvss) /  
sum(num_avert_obs_behav)
sum(num_avert_obs_coital)




tiff("../output/Fig5.tif", height = 5*1200, 5*1200,
     units = "px", res = 1200, pointsize = 8,  compression = "lzw")
plot(2007:2017, nvss_births, ylim=c(0,3e5), type='b', 
     xlab="Year", ylab = "Births", xaxt='n')
abline(h=model_births_btp_flat[1], lty=2, col='grey80')
axis(1, 2007:2017, 2007:2017)
points(2007:2017, model_births_btp_gutt, type='b', col = 'red')
points(2007:2017, model_births_btp_coit, type='b', col = 'blue')

legend(2007, 5e4, c('Reported births (NVSS)',
                   'Predicted births (YRBS behavioral data)',
                   'Predicted births (with additional 50% decline per decade in sexual acts per partner)'),
        cex=0.9, text.col=c('black','red', 'blue'),
        col=c('black','red', 'blue'), pch = 1, ncol=1)

dev.off()

1-nvss_births[11]/nvss_births[1]
1-model_births_btp_gutt[11]/model_births_btp_gutt[1]

save.image("a10_preg_full_run.rda")

# Figure for paper
(sum(model_births_btp_coit) - sum(nvss_births)) / sum(nvss_births)

# Percent explained

obs_decline_births <- nvss_births[1] - nvss_births
mod_decline_births <- nvss_births[1] - model_births_btp_gutt

mod_decline_births / obs_decline_births
sum(mod_decline_births) / sum(obs_decline_births)


