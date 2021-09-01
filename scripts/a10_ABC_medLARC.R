
### The main ABC function. Ideally this would be in the R folder as a package function

a10_ABC_medLARC <- function(base_prob_detpreg) {
  a10_output <- a10_preg(n_f = n_f,
                    prop_eversex_f = pred_eversex_f,
                    #condom_use_f = pred_condom_f,
                    bc_use = pred_bctype_medLARC,
                    mean_new_part_f = pred_mnppy_f,
                    coital_acts_pp_f = capp_f,
                    #prop_diag_f = prop_diag_f,                # IMPT EDIT
                    preg_init_f = preg_init_f,                 # IMPT EDIT
                    prob_detpreg_f = base_prob_detpreg,        # IMPT EDIT
                    meanpop_tot_f = meanpop_13to18_f,
                    failure_rate = failure_rate                # IMPT EDIT
  )
  
  if(is.vector(preg_init) & length(preg_init)==6) {
    result <- sum(
      sapply(cal_times, function(x) {
        sum(abs(colSums(a10_output$n_preg_total[,,x]) - preg_init)) 
      })
    )
  }
  if(is.matrix(preg_init) & sum(dim(preg_init)==c(3,6))==2) {
    result <- sum(
      sapply(cal_times, function(x) {
        sum(abs(colSums(a10_output$n_preg_total[,,x]) - colSums(preg_init)))
      })
    )
  }
  return(result)
}
