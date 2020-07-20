
#############################################################
#'Main a10 function
#'
#' @param n_f A 3x6x11 array indicating the population size for females, by race/eth by age by year
#' @param prop_eversex_f A 3x6x11 matrix indicating the proportion of females debuted (as FSM), by race/eth by age by year
#' @param bc_use_f, A list of 6 3x6x11 matrices indicating birth control use for females, by race/eth by age by year
#' @param mean_new_part_f A 3x6x11 matrix indicating the mean new partners per year for debuted females, by race/eth by age
#' @param coital_acts_pp_f A 3x6x11 matrix indicating the mean coital acts per partner for females, by race/eth by age
#' @param preg_init_f A vector of length 3 or a 3x6 matrix, indicating the number of recent annual pregnancies among females, by race/eth (and optionally by age) -- in and out of school
#' @param prob_detpreg_f Per-act transmission probability from male to female
#' @param meanpop_tot_f Total female population in and out of school across the relevant ages
#' @param failure_rate A list of 6 3x6 matrices indicating the relative failure rate of the six different BC methods, by race/eth by age 
#' '
#' @return A list comprising an array of dimensions [3,6,12], containing the estimated number of
#'   pregnancies per age per year. The three rows represent the
#'   three race/ethnicity groups (B, H, W); the six columns represent the ages (13:18);
#'   the 11 layers represent the years (baseline, years 1:10).

#' @export
a10_preg <- function(n_f,
                prop_eversex_f,
                bc_use_f,
                mean_new_part_f,
                coital_acts_pp_f,
                preg_init_f,
                prob_detpreg_f,
                meanpop_tot_f,
                failure_rate
        ) {

  ##################################################
  # Notes to self

  ##########################################################################
  # Dimensional error checking
  if (sum(dim(n_f) == c(3,6,11)) <3) stop("n_f must be an array with dimensions c(3,6,11).")

  ##########################################################################
  # Init bookkeeping

  # Calc # who have sexually debuted
  n_eversex_f <- n_f * prop_eversex_f
  n_eversex_f_tot <- meanpop_tot_f * prop_eversex_f
  
  # Create arrays to store number of pregnancies per year **in school** and *total*
  n_preg_insch_f <- n_preg_total_f <- array(dim=c(3,6,12))
  n_preg_insch_f[,,1] <- n_preg_total_f[,,1] <- NA

  # Create arrays to store number of diagnoses per year *total* (in and out of HS)
  #n_diag_total_f <- array(dim=c(3,6,12))
  #if(is.matrix(diag_init_f) & sum(dim(diag_init_f)==c(3,6))==2) {
  #  n_diag_total_f[,,1] <- diag_init_f
  #  n_diag_total_m[,,1] <- diag_init_m
  #} else n_diag_total_f[,,1] <- n_diag_total_m[,,1] <- NA

  # Create arrays to store number of diagnoses per year *in HS*
  #n_diag_insch_f <- n_diag_insch_m <- array(dim=c(3,6,12))
  #n_diag_insch_f[,,1] <- NA
  #n_diag_insch_m[,,1] <- NA

  # Create arrays to store prevalence in the cross-section (value should be same in sch and tot)
  #prev_f <- prev_m <- array(dim=c(3,6,12))
  
  #if(is.vector(diag_init_f) & length(diag_init_f)==3) {
  #  ###prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / rowSums(prop_eversex_f[,,1]*meanpop_tot_f[,,1])
  #  prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / rowSums(meanpop_tot_f[,,1])
  #} else {
  #  if(is.matrix(diag_init_f) & sum(dim(diag_init_f)==c(3,6))==2) {
      ###prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / (prop_eversex_f[,,1]*meanpop_tot_f[,,1])
  #    prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / (meanpop_tot_f[,,1])
  #  } else {
  #    stop("diag_init_f must be either a vector of length 3 or a matrix of dim (3,6).")
  #  }
  #}
  
  #if(is.vector(diag_init_f) & length(diag_init_f)==3) {
  #  ###prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / rowSums(prop_eversex_m[,,1]*meanpop_tot_m[,,1])
  #  prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / rowSums(meanpop_tot_m[,,1])
  #} else {
  #  if(is.matrix(diag_init_f) & sum(dim(diag_init_f)==c(3,6))==2) {
  #    ###prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / (prop_eversex_m[,,1]*meanpop_tot_m[,,1])
  #    prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / (meanpop_tot_m[,,1])    } else {
  #    stop(" diag_init_m must be either a vector of length 3 or a matrix of dim (3,6).")
  #  }
  #}

  # Calculate the number of condomless acts per person
  #cl_acts_f <- mean_new_part_f * coital_acts_pp_f * (1-condom_use_f)
  #cl_acts_m <- mean_new_part_m * coital_acts_pp_m * (1-condom_use_m)
  coital_acts_f <- mean_new_part_f * coital_acts_pp_f
  
  ##########################################################################
  # Advancement

  for (i in 2:12) {
    
    # Get weighted avg of prevalence in age range among those eversex, in or out of school 
    #  Differs from in school bc age population weights are different, even though age-specific prevs are the same.
    #  This is all needed to make consistent with the tool.
    
    #overall_prev_f <- rowSums(prev_f[,,i-1]*meanpop_tot_f[,,i-1]) /   
    #                            rowSums(meanpop_tot_f[,,i-1])

    n_preg_total_f[,,i] <- 
      (n_eversex_f_tot[,,i-1]) * 
        (1-
          (1-prob_detpreg_f[,]*(failure_rate[[1]]))^(coital_acts_f[,,i-1]*bc_use_f[[1]][,,i-1]) * 
          (1-prob_detpreg_f[,]*(failure_rate[[2]]))^(coital_acts_f[,,i-1]*bc_use_f[[2]][,,i-1]) *
          (1-prob_detpreg_f[,]*(failure_rate[[3]]))^(coital_acts_f[,,i-1]*bc_use_f[[3]][,,i-1]) *
          (1-prob_detpreg_f[,]*(failure_rate[[4]]))^(coital_acts_f[,,i-1]*bc_use_f[[4]][,,i-1]) *
          (1-prob_detpreg_f[,]*(failure_rate[[5]]))^(coital_acts_f[,,i-1]*bc_use_f[[5]][,,i-1]) *
          (1-prob_detpreg_f[,]*(failure_rate[[6]]))^(coital_acts_f[,,i-1]*bc_use_f[[6]][,,i-1])
      )

    n_preg_insch_f[,,i] <- n_preg_total_f[,,i] * n_f[,,1] / meanpop_tot_f[,,1] 
    
    #n_preg_insch_f[,,i] <- (n_eversex_f[,,i-1]) * (1-(1-prob_detpreg_f[,])^(cl_acts_f[,,i-1]))
    #n_diag_insch_f[,,i] <- n_inc_insch_f[,,i] * prop_diag_f
    #n_diag_total_f[,,i] <- n_diag_insch_f[,,i] * meanpop_tot_f[,,1] / n_f[,,1]
    #prev_f[,,i] <- n_inc_insch_f[,,i]*dur_inf_f / n_f[,,i-1]                        
  }

  ##########################################################################
  # Final processing

  result <- list(n_preg_insch_f = n_preg_insch_f,
                 n_preg_total_f = n_preg_total_f,
                 #n_diag_insch_f = n_diag_insch_f,
                 #n_diag_total_f = n_diag_total_f,
                 n_eversex_f = n_eversex_f,
                 n_eversex_f_tot = n_eversex_f_tot
  )
  return(result)
}

