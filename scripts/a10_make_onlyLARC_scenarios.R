
###### only LARC changes, replacing withdrawal ###

pred_bctype_onlyLARC_minL_from_wdl <- pred_bctype_minLARC
pred_bctype_onlyLARC_minL_from_wdl$LARC <- pred_bctype_minLARC_dyn$LARC
pred_bctype_onlyLARC_minL_from_wdl$withdrawal_other <- 
  pred_bctype_onlyLARC_minL_from_wdl$withdrawal_other - pred_bctype_minLARC_dyn$LARC

pred_bctype_onlyLARC_maxL_from_wdl <- pred_bctype_maxLARC
pred_bctype_onlyLARC_maxL_from_wdl$LARC <- pred_bctype_maxLARC_dyn$LARC
pred_bctype_onlyLARC_maxL_from_wdl$withdrawal_other <- 
  pred_bctype_onlyLARC_maxL_from_wdl$withdrawal_other - pred_bctype_maxLARC_dyn$LARC

# if any withdrawals go below zero, reduce no method down
for (year in 1:11) {
  whichneg <- pred_bctype_onlyLARC_minL_from_wdl$withdrawal_other[,,year] < 0
  pred_bctype_onlyLARC_minL_from_wdl$no_method[,,year][whichneg] <- 
    pred_bctype_onlyLARC_minL_from_wdl$no_method[,,year][whichneg] + pred_bctype_onlyLARC_minL_from_wdl$withdrawal_other[,,year][whichneg]
  pred_bctype_onlyLARC_minL_from_wdl$withdrawal_other[,,year][whichneg] <- 0
}

for (year in 1:11) {
  whichneg <- pred_bctype_onlyLARC_maxL_from_wdl$withdrawal_other[,,year] < 0
  pred_bctype_onlyLARC_maxL_from_wdl$no_method[,,year][whichneg] <- 
    pred_bctype_onlyLARC_maxL_from_wdl$no_method[,,year][whichneg] + pred_bctype_onlyLARC_maxL_from_wdl$withdrawal_other[,,year][whichneg]
  pred_bctype_onlyLARC_maxL_from_wdl$withdrawal_other[,,year][whichneg] <- 0
}

# Check to see it's all 1s
temp <- pred_bctype_onlyLARC_minL_from_wdl[[1]] + pred_bctype_onlyLARC_minL_from_wdl[[2]] + pred_bctype_onlyLARC_minL_from_wdl[[3]] + 
  pred_bctype_onlyLARC_minL_from_wdl[[4]] + pred_bctype_onlyLARC_minL_from_wdl[[5]] + pred_bctype_onlyLARC_minL_from_wdl[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0

temp <- pred_bctype_onlyLARC_maxL_from_wdl[[1]] + pred_bctype_onlyLARC_maxL_from_wdl[[2]] + pred_bctype_onlyLARC_maxL_from_wdl[[3]] + 
  pred_bctype_onlyLARC_maxL_from_wdl[[4]] + pred_bctype_onlyLARC_maxL_from_wdl[[5]] + pred_bctype_onlyLARC_maxL_from_wdl[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0


###### only LARC changes, replacing condoms ###

pred_bctype_onlyLARC_minL_from_cdm <- pred_bctype_minLARC
pred_bctype_onlyLARC_minL_from_cdm$LARC <- pred_bctype_minLARC_dyn$LARC
pred_bctype_onlyLARC_minL_from_cdm$condoms <- 
  pred_bctype_onlyLARC_minL_from_cdm$condoms - pred_bctype_minLARC_dyn$LARC

pred_bctype_onlyLARC_maxL_from_cdm <- pred_bctype_maxLARC
pred_bctype_onlyLARC_maxL_from_cdm$LARC <- pred_bctype_maxLARC_dyn$LARC
pred_bctype_onlyLARC_maxL_from_cdm$condoms <- 
  pred_bctype_onlyLARC_maxL_from_cdm$condoms - pred_bctype_maxLARC_dyn$LARC

# if any condoms go below zero, reduce no method down
for (year in 1:11) {
  whichneg <- pred_bctype_onlyLARC_minL_from_cdm$condoms[,,year] < 0
  pred_bctype_onlyLARC_minL_from_cdm$no_method[,,year][whichneg] <- 
    pred_bctype_onlyLARC_minL_from_cdm$no_method[,,year][whichneg] + pred_bctype_onlyLARC_minL_from_cdm$condoms[,,year][whichneg]
  pred_bctype_onlyLARC_minL_from_cdm$condoms[,,year][whichneg] <- 0
}

for (year in 1:11) {
  whichneg <- pred_bctype_onlyLARC_maxL_from_cdm$condoms[,,year] < 0
  pred_bctype_onlyLARC_maxL_from_cdm$no_method[,,year][whichneg] <- 
    pred_bctype_onlyLARC_maxL_from_cdm$no_method[,,year][whichneg] + pred_bctype_onlyLARC_maxL_from_cdm$condoms[,,year][whichneg]
  pred_bctype_onlyLARC_maxL_from_cdm$condoms[,,year][whichneg] <- 0
}

# Check to see it's all 1s
temp <- pred_bctype_onlyLARC_minL_from_cdm[[1]] + pred_bctype_onlyLARC_minL_from_cdm[[2]] + pred_bctype_onlyLARC_minL_from_cdm[[3]] + 
  pred_bctype_onlyLARC_minL_from_cdm[[4]] + pred_bctype_onlyLARC_minL_from_cdm[[5]] + pred_bctype_onlyLARC_minL_from_cdm[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0

temp <- pred_bctype_onlyLARC_maxL_from_cdm[[1]] + pred_bctype_onlyLARC_maxL_from_cdm[[2]] + pred_bctype_onlyLARC_maxL_from_cdm[[3]] + 
  pred_bctype_onlyLARC_maxL_from_cdm[[4]] + pred_bctype_onlyLARC_maxL_from_cdm[[5]] + pred_bctype_onlyLARC_maxL_from_cdm[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0

