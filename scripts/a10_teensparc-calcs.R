setwd("C:/git/CAMP_10yr_pregnancy/scripts/")  # Change depending on machine
#setwd("H:/_goodreau/git/CAMP.10yr.pregnancy/scripts/")  # Change depending on machine

library(MASS)
library(dplyr)
library(tidyverse)
library(magrittr)
library(nnet)

########################################################################
### Common tasks across scenarios

source("a10_import.R")

bctypes_teensparc <- bctype_in_wts[,,5,]
bctypes_teensparc <- bctypes_teensparc[,,-c(3,5,8:10)]
bctypes_teensparc <- apply(bctypes_teensparc,2:3,sum)
bctypes_teensparc <- rbind(
                          colSums(bctypes_teensparc[1:3,]),
                          colSums(bctypes_teensparc[4:5,]),
                          bctypes_teensparc[6,]
                          )
bctypes_teensparc <- prop.table(bctypes_teensparc, 1)
colnames(bctypes_teensparc) <- c('no method', 'condoms', 'pills',
                              'LARC', 'other hormonal', 'withdrawal/other')

bctypes_teensparc

