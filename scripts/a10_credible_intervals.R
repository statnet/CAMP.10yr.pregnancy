
##########################################################
### Determining credible intervals for pregnancy model

### Bootstrapping

bctype_in_wts_boot <- array(dim=c(neths, nages, nyears, nbctypes_in, 100))
for (i in 1:length(years)) {
  filename <- paste(datapath, "/bctypes_ind_", years[i], ".csv", sep="")
  temp <- read.csv(filename)
  if(years[i] %in% c(2007, 2009))       temp$pregprev2 <- recode(temp$pregprev2, other="other79")
  if(years[i] %in% c(2011))             temp$pregprev2 <- recode(temp$pregprev2, other="other1")
  n <- nrow(temp)
  indices <- table(sample(1:n, 1e5, prob=temp$wts, replace=TRUE))
  for (s in 1:100) {
    resample[[i]][[j]] <- temp[indices,]
    for (j in 1:neths) {
      for (k in 1:nages) {
        for (m in 1:nbctypes_in) {
            bctype_in_wts_boot[j,k,i,m,s] <- nrow(temp %>% filter(sex=="female", race==eths_lc[j], age==ages[k], pregprev2==bctypes_in[m]))
        }
      }
    }
  }
}












###############################################
### Draw coefficient samples (NB: these can be used for both GC and CT)

coefs_eversex <- mvrnorm(n = 100, 
          mu = eversex_f_reg$coefficients,
          Sigma = vcov(eversex_f_reg))

coefs_mnppy <- mvrnorm(n = 100, 
          mu = mnppy_f_reg$coefficients,
          Sigma = vcov(mnppy_f_reg))

coefs_bctype <- list()
for (i in 1:6) {
  coefs_bctype[[i]] <- mvrnorm(n = 100, 
                          mu = as.vector(t(coef(bctype_reg_ageasq[[i]]))),
                          Sigma = vcov(bctype_reg_ageasq[[i]]))
}

##### Set up temporary objects

bctype_reg_ageasq_temp <- bctype_reg_ageasq             # glm objects for each coefficient 
eversex_f_reg_temp <- eversex_f_reg
mnppy_f_reg_temp <- mnppy_f_reg

a10_preg_obs_100 <- a10_preg_nbc_100 <- list()    # Lists of 100 outcomes

##### Run loop - each model over 100 coefficient sets

for (i in 1:100) {
  
  ## Determine predicted values for each of the six regressions
  condom_f_reg_temp$coefficients <- coefs_condom_f_gc[i,]
  pred_condom_f_temp_obs <- 
    array(predict(condom_f_reg_temp, type='response', 
    newdata= pred_condom_f_df_indep), dim=c(3,6,11))
  
  condom_m_reg_temp$coefficients <- coefs_condom_m_gc[i,]
  pred_condom_m_temp_obs <- 
    array(predict(condom_m_reg_temp, type='response', 
    newdata= pred_condom_m_df_indep), dim=c(3,6,11))
  
  eversex_f_reg_temp$coefficients <- coefs_eversex_f_gc[i,]
  pred_eversex_f_temp_obs <- 
    array(predict(eversex_f_reg_temp, type='response', 
    newdata= pred_eversex_f_df_indep), dim=c(3,6,11))
  
  eversex_m_reg_temp$coefficients <- coefs_eversex_m_gc[i,]
  pred_eversex_m_temp_obs <- 
    array(predict(eversex_m_reg_temp, type='response', 
    newdata= pred_eversex_m_df_indep), dim=c(3,6,11))
  
  mnppy_f_reg_temp$coefficients <- coefs_mnppy_f_gc[i,]
  pred_mnppy_f_temp_obs <- 
    array(predict(mnppy_f_reg_temp, type='response', 
    newdata= pred_mnppy_f_df_indep), dim=c(3,6,11))
  
  mnppy_m_reg_temp$coefficients <- coefs_mnppy_m_gc[i,]
  pred_mnppy_m_temp_obs <- 
    array(predict(mnppy_m_reg_temp, type='response', 
    newdata= pred_mnppy_m_df_indep), dim=c(3,6,11))
  
  ### Run GC obs model
  a10_gc_obs_100[[i]] <- a10(
      n_f = n_f, n_m = n_m,
      prop_eversex_f = pred_eversex_f_temp_obs,
      prop_eversex_m = pred_eversex_m_temp_obs,
      condom_use_f = pred_condom_f_temp_obs,
      condom_use_m = pred_condom_m_temp_obs,
      mean_new_part_f = pred_mnppy_f_temp_obs,
      mean_new_part_m = pred_mnppy_m_temp_obs,
      coital_acts_pp_f = capp_f,
      coital_acts_pp_m = capp_m,
      p_ethn_f = p_ethn_f,
      p_ethn_m = p_ethn_m,
      diag_init_f = dx_gc_init_tot_f,
      diag_init_m = dx_gc_init_tot_m,
      prop_diag_f = prop_diag_f_gc,
      prop_diag_m = prop_diag_m_gc,
      dur_inf_f = dur_f_gc,
      dur_inf_m = dur_m_gc,
      beta_f2m = beta_ipv_gc,
      beta_m2f = beta_rpv_gc,
      meanpop_tot_f = meanpop_13to18_f,
      meanpop_tot_m = meanpop_13to18_m,
      part_prev_ratio_f = part_prev_ratio_gc_f,
      part_prev_ratio_m = part_prev_ratio_gc_m
  )
  
  ## Assign predicted values of 2007 to all other years for NBC
  pred_condom_f_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_condom_f_temp_obs[,,1],11))
  pred_condom_m_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_condom_m_temp_obs[,,1],11))
  pred_eversex_f_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_eversex_f_temp_obs[,,1],11))
  pred_eversex_m_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_eversex_m_temp_obs[,,1],11))
  pred_mnppy_f_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_mnppy_f_temp_obs[,,1],11))
  pred_mnppy_m_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_mnppy_m_temp_obs[,,1],11))
  
  ### Run GC obs model
  a10_gc_nbc_100[[i]] <- a10(
    n_f = n_f, n_m = n_m,
    prop_eversex_f = pred_eversex_f_nbc_temp,
    prop_eversex_m = pred_eversex_m_nbc_temp,
    condom_use_f = pred_condom_f_nbc_temp,
    condom_use_m = pred_condom_m_nbc_temp,
    mean_new_part_f = pred_mnppy_f_nbc_temp,
    mean_new_part_m = pred_mnppy_m_nbc_temp,
    coital_acts_pp_f = capp_f,
    coital_acts_pp_m = capp_m,
    p_ethn_f = p_ethn_f,
    p_ethn_m = p_ethn_m,
    diag_init_f = dx_gc_init_tot_f,
    diag_init_m = dx_gc_init_tot_m,
    prop_diag_f = prop_diag_f_gc,
    prop_diag_m = prop_diag_m_gc,
    dur_inf_f = dur_f_gc,
    dur_inf_m = dur_m_gc,
    beta_f2m = beta_ipv_gc,
    beta_m2f = beta_rpv_gc,
    meanpop_tot_f = meanpop_13to18_f,
    meanpop_tot_m = meanpop_13to18_m,
    part_prev_ratio_f = part_prev_ratio_gc_f,
    part_prev_ratio_m = part_prev_ratio_gc_m
  )
}

#plot(sapply(1:100, function(x) sum(a10_gc_nbc_temp[[x]]$n_inc_insch_f[,,11])), 
#     sapply(1:100, function(x) sum(a10_gc_obs_100[[x]]$n_inc_insch_f[,,11])))

save(a10_gc_nbc_100, a10_gc_obs_100, file='../output/a100_gc_ci.rda')
