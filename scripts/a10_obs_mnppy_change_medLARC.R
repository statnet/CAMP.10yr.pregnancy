
#########################################################################
### Call main function

a10_obs_mnppy_medLARC <- a10_preg(n_f = n_f, 
                             prop_eversex_f = pred_eversex_f,
                             bc_use = pred_bctype_medLARC,
                             mean_new_part_f = pred_mnppy_f_dyn,
                             coital_acts_pp_f = capp_f,
                             preg_init_f = preg_init_f,             
                             prob_detpreg_f = prob_detpreg_medLARC,        
                             meanpop_tot_f = meanpop_13to18_f,
                             failure_rate = failure_rate
  )

#########################################################################
### Process results


