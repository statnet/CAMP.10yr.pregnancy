
#########################################################################
### Call main function

a10_minLARC_nbc <- a10_preg(n_f = n_f, 
                             prop_eversex_f = pred_eversex_f,
                             bc_use = pred_bctype_minLARC,
                             mean_new_part_f = pred_mnppy_f,
                             coital_acts_pp_f = capp_f,
                             preg_init_f = preg_init_f,             
                             prob_detpreg_f = prob_detpreg,        
                             meanpop_tot_f = meanpop_13to18_f,
                             failure_rate = failure_rate
  )

#########################################################################
### Process results

save(a10_minLARC_nbc, file='../output/a10_minLARC_nbc.rda')

#apply(a10_minLARC_nbc$n_preg_total_f, 2:3, sum)[,2] / preg_init
