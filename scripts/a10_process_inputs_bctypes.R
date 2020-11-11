#########################################################################
### Birth control

# Create a full data frame of all of the data across all years
# Revised  strategy involves running a separate multinomial regression for each year given
#    the changing categories, so we end up subsetting out the data again. But it is still 
#    useful to have it all assembled in a single df for bookkeeping

bctype_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(bctype_df) <- c('ethn', 'age', 'year')

bctype_df$wts <- as.vector(apply(bctype_in_wts, 1:3, sum))
bctype_df$agefac <- relevel(as.factor(bctype_df$age), ref='16')

bctype_df[,6:16] <- sapply(1:11, function(x) as.vector(bctype_in_prob[,,,x]))
names(bctype_df)[6:16] <- bctypes_in

bctype_df <- bctype_df %>% replace(is.na(.), 0)

## Create partitions

included_cols_by_year <- list()
included_cols_by_year[[1]] <- included_cols_by_year[[2]] <- 
  c("no method", "condoms", "withdrawal", "pills", "injection", "other79")
included_cols_by_year[[3]] <- 
  c("no method", "condoms", "withdrawal", "pills", "other hormonal+LARC", "other1")
included_cols_by_year[[4]] <- included_cols_by_year[[5]] <- included_cols_by_year[[6]] <-  
  c("no method", "condoms", "pills", "LARC", "other hormonal", "withdrawal/other")

skipped_cols_by_year <- list()
for (i in 1:6) skipped_cols_by_year[[i]] <- setdiff(bctypes_in, included_cols_by_year[[i]])

bctype_by_year <- list()
for (i in 1:6) {
  bctype_by_year[[i]] <- bctype_df %>% filter(year==years[i])
  bctype_by_year[[i]] <- bctype_by_year[[i]][,!names(bctype_by_year[[i]]) %in% 
    skipped_cols_by_year[[i]]]
  bctype_by_year[[i]][rowSums(bctype_by_year[[i]][,-(1:5)])==0,"no method"] <- 1
  # allows for fitting but does not matter since wts = 0
}

bctype_reg_agefac <- bctype_reg_agefac_byrace <- bctype_reg_agenum <- bctype_reg_ageasq <- list()

for (i in 1:6) {
  bctype_reg_agefac[[i]] <-         multinom(as.matrix(bctype_by_year[[i]][,-(1:5)])~ 
                                      bctype_by_year[[i]]$agefac + bctype_by_year[[i]]$ethn, 
                                      weights=bctype_by_year[[i]]$wts)
        
  bctype_reg_agefac_byrace[[i]] <-  multinom(as.matrix(bctype_by_year[[i]][,-(1:5)])~ 
                                      bctype_by_year[[i]]$agefac + bctype_by_year[[i]]$ethn +
                                      bctype_by_year[[i]]$agefac*bctype_by_year[[i]]$ethn, 
                                      weights=bctype_by_year[[i]]$wts)
  
  bctype_reg_agenum[[i]] <-         multinom(as.matrix(bctype_by_year[[i]][,-(1:5)])~ 
                                      bctype_by_year[[i]]$age + bctype_by_year[[i]]$ethn, 
                                      weights=bctype_by_year[[i]]$wts)
  
  bctype_reg_ageasq[[i]] <-         multinom(as.matrix(bctype_by_year[[i]][,-(1:5)])~ 
                                      I(bctype_by_year[[i]]$age-0) +
                                      I((bctype_by_year[[i]]$age-0)^2) + 
                                      bctype_by_year[[i]]$ethn, 
                                      weights=bctype_by_year[[i]]$wts)
  }

bctype_reg_agefac_AIC <- sapply(1:6, function(x) bctype_reg_agefac[[x]]$AIC)
bctype_reg_agefac_byrace_AIC <- sapply(1:6, function(x) bctype_reg_agefac_byrace[[x]]$AIC)
bctype_reg_agenum_AIC <- sapply(1:6, function(x) bctype_reg_agenum[[x]]$AIC)  
bctype_reg_ageasq_AIC <- sapply(1:6, function(x) bctype_reg_ageasq[[x]]$AIC)  

# Agenum are lower, but we are still proceeding with agefac because our goal here is not to
#   develop the most parsimonious model, but rather to develop the fullest model that smooths
#   the data observed across all multi-way categories using first principles

cbind(bctype_reg_agefac_AIC, bctype_reg_agefac_byrace_AIC, bctype_reg_agenum_AIC, bctype_reg_ageasq_AIC)

### assemble the predicted value matrices
### This could be done with four-dimenwsional arrays and less verbose code, 
###   but I like having 3 dims and explicit names to reduce room for confusion and error

pred_bctype_agefac <- pred_bctype_agefac_byrace <- 
  pred_bctype_agenum <- pred_bctype_ageasq <- list(
                            no_method           = array(dim=c(3,6,11)),
                            condoms             = array(dim=c(3,6,11)),
                            withdrawal          = array(dim=c(3,6,11)),
                            pills               = array(dim=c(3,6,11)),
                            injection           = array(dim=c(3,6,11)),
                            LARC                = array(dim=c(3,6,11)),
                            other_hormonal      = array(dim=c(3,6,11)),
                            other_hormonal_LARC = array(dim=c(3,6,11)),
                            other79             = array(dim=c(3,6,11)),
                            other1              = array(dim=c(3,6,11)),
                            withdrawal_other    = array(dim=c(3,6,11))
)


for (year in 1:6) {
  types <- which(bctypes_in%in%included_cols_by_year[[year]])
  for (type_index in 1:length(types)) {
      pred_bctype_agefac[[types[type_index]]][,,year*2-1] <- # Expands out to include even years
        matrix(bctype_reg_agefac[[year]]$fitted.values[,type_index], nrow=3)
      pred_bctype_agefac_byrace[[types[type_index]]][,,year*2-1] <- # Expands out to include even years
        matrix(bctype_reg_agefac_byrace[[year]]$fitted.values[,type_index], nrow=3)
      pred_bctype_agenum[[types[type_index]]][,,year*2-1] <- # Expands out to include even years
        matrix(bctype_reg_agenum[[year]]$fitted.values[,type_index], nrow=3)
      pred_bctype_ageasq[[types[type_index]]][,,year*2-1] <- # Expands out to include even years
        matrix(bctype_reg_ageasq[[year]]$fitted.values[,type_index], nrow=3)
  }
}

#matplot(t(pred_bctype_agefac[[1]][1,,]), type='b')


#########################################################################
### NOTES

save.image("../output/a10_inputs_processed.rda")

