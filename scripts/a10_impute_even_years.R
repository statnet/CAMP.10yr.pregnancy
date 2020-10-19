
#########################################################################
# teen-SPARC 10-year pregnancy paper - reassign bc types
# 
# Notes: 
#   1. Change the setwd() line if needed

#########################################################################
### Basics
#setwd("C:/git/CAMP_10yr_pregnancy/scripts/")

# Create object 
for (year in c(2,4,6,8,10)) {
  for (method in 1:6) {
    pred_bctype_minLARC[[method]][,,year] <- (pred_bctype_minLARC[[method]][,,year-1] + 
                                              pred_bctype_minLARC[[method]][,,year+1]) /
                                              2
    pred_bctype_maxLARC[[method]][,,year] <- (pred_bctype_maxLARC[[method]][,,year-1] + 
                                                pred_bctype_maxLARC[[method]][,,year+1]) /
      2
  }
}



