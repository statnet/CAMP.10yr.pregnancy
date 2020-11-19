#########################################################################
### Call main function

a10_obs_cc_onlyLARC_from_cdm <- a10_preg(n_f = n_f, 
                                prop_eversex_f = pred_eversex_f,
                                bc_use = pred_bctype_onlyLARC_from_cdm,
                                mean_new_part_f = pred_mnppy_f,
                                coital_acts_pp_f = capp_f,
                                preg_init_f = preg_init_f,             
                                prob_detpreg_f = prob_detpreg_minLARC,        
                                meanpop_tot_f = meanpop_13to18_f,
                                failure_rate = failure_rate
)


#########################################################################
### Process results

save(a10_obs_cc_onlyLARC_from_cdm, file='../output/a10_obs_cc_onlyLARC_from_cdm.rda')

