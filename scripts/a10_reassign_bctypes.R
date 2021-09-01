
#########################################################################
# teen-SPARC 10-year pregnancy paper - reassign bc types
# 
# Notes: 
#   1. Change the setwd() line if needed

#########################################################################
### Basics
#setwd("C:/git/CAMP_10yr_pregnancy/scripts/")

#Decide which model to use
pred_bctype_in <- pred_bctype_agefac
#pred_bctype_in <- pred_bctype_ageasq

# Create object 
pred_bctype <- list(no_method           = array(dim=c(3,6,11)),
                    condoms             = array(dim=c(3,6,11)),
                    pills               = array(dim=c(3,6,11)),
                    LARC                = array(dim=c(3,6,11)),
                    other_hormonal      = array(dim=c(3,6,11)),
                    withdrawal_other    = array(dim=c(3,6,11))
)

# Reassign the ones that are straightforward across all years
pred_bctype$no_method <- pred_bctype_in$no_method
pred_bctype$condoms <- pred_bctype_in$condoms
pred_bctype$pills <- pred_bctype_in$pills

# Reassign the ones that are straightforward for some years

# 2007, 2009
for (year in c(1,3)) {
  pred_bctype$other_hormonal[,,year] <- pred_bctype_in$injection[,,year]
  pred_bctype$withdrawal_other[,,year] <- pred_bctype_in$withdrawal[,,year]
}

# 2011
for (year in c(5)) {
  pred_bctype$withdrawal_other[,,year] <- pred_bctype_in$other1[,,year] +
                                          pred_bctype_in$withdrawal[,,year]
}

# 2013, 2015, 2017
for (year in c(7,9,11)) {
  pred_bctype$LARC[,,year] <- pred_bctype_in$LARC[,,year]
  pred_bctype$other_hormonal[,,year] <- pred_bctype_in$other_hormonal[,,year]
  pred_bctype$withdrawal_other[,,year] <- pred_bctype_in$withdrawal_other[,,year]
}

##### Set up different models based on assumptions

###### minimum LARC ###

pred_bctype_minLARC <- pred_bctype

for (year in c(1,3,5)) {
  pred_bctype_minLARC$LARC[,,year] <- 0
}

for (year in c(1,3)) {
  pred_bctype_minLARC$other_hormonal[,,year] <- pred_bctype$other_hormonal[,,year] + pred_bctype_in$other79[,,year]
}

for (year in c(5)) {
  pred_bctype_minLARC$other_hormonal[,,year] <- pred_bctype_in$other_hormonal_LARC[,,year]
}

# Check to see it's all 1s
temp <- pred_bctype_minLARC[[1]] + pred_bctype_minLARC[[2]] + pred_bctype_minLARC[[3]] + 
  pred_bctype_minLARC[[4]] + pred_bctype_minLARC[[5]] + pred_bctype_minLARC[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0

###### maximum LARC ###

pred_bctype_maxLARC <- pred_bctype

pred_bctype_maxLARC$LARC[,,1] <- 0
pred_bctype_maxLARC$LARC[,,3] <- pred_bctype_maxLARC$LARC[,,7]
pred_bctype_maxLARC$LARC[,,5] <- pred_bctype_maxLARC$LARC[,,7]

for (year in c(1)) {
  pred_bctype_maxLARC$other_hormonal[,,year] <- 
    pred_bctype$other_hormonal[,,year] + pred_bctype_in$other79[,,year]
}

for (year in c(3)) {
  pred_bctype_maxLARC$other_hormonal[,,year] <- 
    pred_bctype$other_hormonal[,,year] + pred_bctype_in$other79[,,year] - pred_bctype_maxLARC$LARC[,,year]
}

for (year in c(5)) {
  pred_bctype_maxLARC$other_hormonal[,,year] <- 
    pred_bctype_in$other_hormonal_LARC[,,year] - pred_bctype_maxLARC$LARC[,,year]
}

# if any other hormonals go below zero, reduce LARC down so that other hormonals = 0
for (year in c(1,3,5)) {
  whichneg <- pred_bctype_maxLARC$other_hormonal[,,year] < 0
  pred_bctype_maxLARC$LARC[,,year][whichneg] <- 
    pred_bctype_maxLARC$LARC[,,year][whichneg] + pred_bctype_maxLARC$other_hormonal[,,year][whichneg]
  pred_bctype_maxLARC$other_hormonal[,,year][whichneg] <- 0
  }

# Check to see it's all 1s
temp <- pred_bctype_maxLARC[[1]] + pred_bctype_maxLARC[[2]] + pred_bctype_maxLARC[[3]] + 
  pred_bctype_maxLARC[[4]] + pred_bctype_maxLARC[[5]] + pred_bctype_maxLARC[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0

###### medium LARC ###

pred_bctype_medLARC <- pred_bctype

pred_bctype_medLARC$LARC[,,1] <- 0
pred_bctype_medLARC$LARC[,,3] <- pred_bctype_medLARC$LARC[,,7]
pred_bctype_medLARC$LARC[,,5] <- pred_bctype_medLARC$LARC[,,7]

for (year in c(1)) {
  pred_bctype_medLARC$other_hormonal[,,year] <- 
    pred_bctype$other_hormonal[,,year] + pred_bctype_in$other79[,,year]
}

for (year in c(3)) {
  pred_bctype_medLARC$other_hormonal[,,year] <- 
    pred_bctype$other_hormonal[,,year] + pred_bctype_in$other79[,,year] - pred_bctype_medLARC$LARC[,,year]
}

for (year in c(5)) {
  pred_bctype_medLARC$other_hormonal[,,year] <- 
    pred_bctype_in$other_hormonal_LARC[,,year] - pred_bctype_medLARC$LARC[,,year]
}

# if any other hormonals go below zero, reduce LARC down so that other hormonals = 0
for (year in c(1,3,5)) {
  whichneg <- pred_bctype_medLARC$other_hormonal[,,year] < 0
  pred_bctype_medLARC$LARC[,,year][whichneg] <- 
    pred_bctype_medLARC$LARC[,,year][whichneg] + pred_bctype_medLARC$other_hormonal[,,year][whichneg]
  pred_bctype_medLARC$other_hormonal[,,year][whichneg] <- 0
}

# Check to see it's all 1s
temp <- pred_bctype_medLARC[[1]] + pred_bctype_medLARC[[2]] + pred_bctype_medLARC[[3]] + 
  pred_bctype_medLARC[[4]] + pred_bctype_medLARC[[5]] + pred_bctype_medLARC[[6]]
temp <- round(temp,5)  
sum(temp[,,c(1,3,5,7,9,11)]!=1)  # should equal 0

#######################################################################
### NOTES

save.image("../output/a10_reassigned_bctypes.rda")

