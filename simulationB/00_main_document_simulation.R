#-------------------------------------------------------------------------------
# Simulation Study -------------------------------------------------------------
#-------------------------------------------------------------------------------

#
# Pathways and directories -----------------------------------------------------
#

setwd(".../simulationA")                                                        # please set path to folder "SimulationB"
folder_main_code  <- ".../simulationB"                                          # please set path to folder "SimulationB"
path_results      <- paste0(folder_main_code, "/simulation_results/")
path_general_code <- " "                                                        # please set path to main folder of this project 

#
# Packages ---------------------------------------------------------------------
#

source(paste0(path_general_code, "/packages.R"))

#
# IV function ------------------------------------------------------------------
#

source(paste0(path_general_code, "/IV_functions.R"))
source(paste0(path_general_code, "/functions.R"))


#
# Decision on Scenarios --------------------------------------------------------
#

scenario               <- "2c"                                                  # please change name of scenario here

n_i_per_provider       <- c(24, 108, 408)[3]
missingness_indata     <- c("noNAs","MCAR","MNAR")[3]


#
# Set parameters for simulation ------------------------------------------------
#


Beta             <- 1                                                           # true treatment effect
S                <- 200                                                         # Simulation runs


#
# Results of the simulation ----------------------------------------------------
#

# simulation results for s

CaT_cc_est           <- CaT_cc_SE           <- CaT_cc_lower           <- CaT_cc_upper                                 <- NULL
IV_PP_cc_est         <- IV_PP_cc_SE         <- IV_PP_cc_lower         <- IV_PP_cc_upper         <- IV_PP_cc_F         <- NULL
IV_PP_est            <- IV_PP_SE            <- IV_PP_lower            <- IV_PP_upper            <- IV_PP_F            <- NULL
IV_ePP_est           <- IV_ePP_SE           <- IV_ePP_lower           <- IV_ePP_upper           <- IV_ePP_F           <- NULL
IVePP_rirs_est       <- IVePP_rirs_SE       <- IVePP_rirs_lower       <- IVePP_rirs_upper       <- IVePP_rirs_F       <- NULL
IV_star_est          <- IV_star_SE          <- IV_star_lower          <- IV_star_upper          <- IV_star_F          <- NULL
IV_prevpatient_est   <- IV_prevpatient_SE   <- IV_prevpatient_lower   <- IV_prevpatient_upper   <- IV_prevpatient_F   <- NULL 
IV_allprop_est       <- IV_allprop_SE       <- IV_allprop_lower       <- IV_allprop_upper       <- IV_allprop_F       <- NULL
IV_alldichmean_est   <- IV_alldichmean_SE   <- IV_alldichmean_lower   <- IV_alldichmean_upper   <- IV_alldichmean_F   <- NULL
IV_alldichmedian_est <- IV_alldichmedian_SE <- IV_alldichmedian_lower <- IV_alldichmedian_upper <- IV_alldichmedian_F <- NULL
IV_prev2prop_est     <- IV_prev2prop_SE     <- IV_prev2prop_lower     <- IV_prev2prop_upper     <- IV_prev2prop_F     <- NULL
IV_prev5prop_est     <- IV_prev5prop_SE     <- IV_prev5prop_lower     <- IV_prev5prop_upper     <- IV_prev5prop_F     <- NULL
IV_prev10prop_est    <- IV_prev10prop_SE    <- IV_prev10prop_lower    <- IV_prev10prop_upper    <- IV_prev10prop_F    <- NULL
IV_allprevprop_est   <- IV_allprevprop_SE   <- IV_allprevprop_lower   <- IV_allprevprop_upper   <- IV_allprevprop_F   <- NULL

prov_out_IVconstruction_ePP         <- NULL
prov_out_IVconstruction_ePP_rirs    <- NULL 
prov_out_IVconstruction_IV_star     <- NULL
prov_out_IVconstruction_prevpatient <- NULL
prov_out_IVconstruction_allprop     <- NULL
prov_out_IVconstruction_prev2prop   <- prov_out_IVconstruction_prev5prop <- prov_out_IVconstruction_prev10prop <- NULL
prov_out_IVconstruction_allprevprop <- NULL

missingness      <- NULL 
Z_j_ePP_all_same <- NULL

var_Y_nst <- X_1_prop <- NULL                       



#
# Start of the simulation ------------------------------------------------------
#


set.seed(55555)
for(s in 1:S){
#s <- 1
  
  #
  # Data generation --------------------------------------------------------------
  # 
  
  source("data_generation.R")
  
  missingness[s]   <- as.numeric((table(R)/length(R)*100)["1"])                 # percentages of missingness

  var_Y_nst[s]     <- var(Y)                                                    # outcome variance
  X_1_prop[s]      <- as.numeric((table(X)/length(X)*100)["1"])                 # proportion of treated patients 
  
  
  #
  # Data set ---------------------------------------------------------------------
  #
  
  data      <- data.frame(prov_ID, pat_ID, tx_order, tx_time, W1_wNA, R, W1, W2, U, X, Y, Y_st, v)
  data_ccW1 <- data[complete.cases(data[ ,c("W1_wNA")]), ]
  
  #
  # Exclude providers with too little data due to complete case analysis --------
  #
  
  # This step is specific to the ppIV construction method
  # output datasets will be used to construct the ppIV (data_cc_IVconstruction_"methodname")
  
  # IV_ePP, IV_ePP_rirs: data preparation in the respective R code
  data_cc_IVconstruction_IV_star     <- exclude_small_provider_fun(data_ccW1, min_nj = 5, prov_ID = "prov_ID")
  data_cc_IVconstruction_prevpatient <- exclude_small_provider_fun(data_ccW1, min_nj = 2, prov_ID = "prov_ID")
  data_cc_IVconstruction_allprop     <- exclude_small_provider_fun(data_ccW1, min_nj = 2, prov_ID = "prov_ID")
  data_cc_IVconstruction_prev2prop   <- exclude_small_provider_fun(data_ccW1, min_nj = 3, prov_ID = "prov_ID")
  data_cc_IVconstruction_prev5prop   <- exclude_small_provider_fun(data_ccW1, min_nj = 6, prov_ID = "prov_ID")
  data_cc_IVconstruction_prev10prop  <- exclude_small_provider_fun(data_ccW1, min_nj = 11, prov_ID = "prov_ID")
  data_cc_IVconstruction_allprevprop <- exclude_small_provider_fun(data_ccW1, min_nj = 2, prov_ID = "prov_ID")
  
  
  # some outputs of this step
  prov_out_IVconstruction_IV_star[s]     <- N_J - length(unique(data_cc_IVconstruction_IV_star$prov_ID))
  prov_out_IVconstruction_prevpatient[s] <- N_J - length(unique(data_cc_IVconstruction_prevpatient$prov_ID))
  prov_out_IVconstruction_allprop[s]     <- N_J - length(unique(data_cc_IVconstruction_allprop$prov_ID))
  prov_out_IVconstruction_prev2prop[s]   <- N_J - length(unique(data_cc_IVconstruction_prev2prop$prov_ID))
  prov_out_IVconstruction_prev5prop[s]   <- N_J - length(unique(data_cc_IVconstruction_prev5prop$prov_ID))
  prov_out_IVconstruction_prev10prop[s]  <- N_J - length(unique(data_cc_IVconstruction_prev10prop$prov_ID))
  prov_out_IVconstruction_allprevprop[s] <- N_J - length(unique(data_cc_IVconstruction_allprevprop$prov_ID))
  
  
  #
  # Construction of the Instrumental Variables  ----------------------------------
  #
  
  # construction of IV_ePP and IV_ePP_rirs in respective R code
  source(paste0(path_general_code, "/IVmethod_Abrahamowicz.R"))
  data_cc_IVconstruction_prevpatient$IV_prevpatient <- as.vector(unlist(tapply(data_cc_IVconstruction_prevpatient$X, data_cc_IVconstruction_prevpatient$prov_ID, IV_prevpatient_fun)))
  data_cc_IVconstruction_allprop$IV_allprop         <- as.vector(unlist(tapply(data_cc_IVconstruction_allprop$X,     data_cc_IVconstruction_allprop$prov_ID,     IV_allprop_fun)))
  
  mean_IV_allprop   <- mean(as.vector(tapply(data_cc_IVconstruction_allprop$IV_allprop,   data_cc_IVconstruction_allprop$prov_ID, function(x) x[1])))
  median_IV_allprop <- median(as.vector(tapply(data_cc_IVconstruction_allprop$IV_allprop, data_cc_IVconstruction_allprop$prov_ID, function(x) x[1])))
  
  data_cc_IVconstruction_allprop$IV_alldichmean     <- ifelse(data_cc_IVconstruction_allprop$IV_allprop >= mean_IV_allprop, 1, 0)
  data_cc_IVconstruction_allprop$IV_alldichmedian   <- ifelse(data_cc_IVconstruction_allprop$IV_allprop >= median_IV_allprop, 1, 0)
  
  data_cc_IVconstruction_prev2prop$IV_prev2prop     <- as.vector(unlist(tapply(data_cc_IVconstruction_prev2prop$X,   data_cc_IVconstruction_prev2prop$prov_ID,  function(X) IV_nprevprop_fun(X = X, n_prev = 2))))
  data_cc_IVconstruction_prev5prop$IV_prev5prop     <- as.vector(unlist(tapply(data_cc_IVconstruction_prev5prop$X,   data_cc_IVconstruction_prev5prop$prov_ID,  function(X) IV_nprevprop_fun(X = X, n_prev = 5))))
  data_cc_IVconstruction_prev10prop$IV_prev10prop   <- as.vector(unlist(tapply(data_cc_IVconstruction_prev10prop$X,  data_cc_IVconstruction_prev10prop$prov_ID, function(X) IV_nprevprop_fun(X = X, n_prev = 10))))
  data_cc_IVconstruction_allprevprop$IV_allprevprop <- as.vector(unlist(tapply(data_cc_IVconstruction_allprevprop$X, data_cc_IVconstruction_allprevprop$prov_ID, IV_allprevprop_fun)))
  
  
  #
  # Deletion of patients for which IV cannot be calculated -----------------------
  #
  
  data_cc_IVestimation_IV_star     <- data_cc_IVconstruction_IV_star[complete.cases(data_cc_IVconstruction_IV_star[ ,c("IV_star")]), ]
  data_cc_IVestimation_prevpatient <- data_cc_IVconstruction_prevpatient[complete.cases(data_cc_IVconstruction_prevpatient[ ,c("IV_prevpatient")]), ]
  data_cc_IVestimation_allprop     <- data_cc_IVconstruction_allprop
  data_cc_IVestimation_prev2prop   <- data_cc_IVconstruction_prev2prop[complete.cases(data_cc_IVconstruction_prev2prop[ ,c("IV_prev2prop")]), ]
  data_cc_IVestimation_prev5prop   <- data_cc_IVconstruction_prev5prop[complete.cases(data_cc_IVconstruction_prev5prop[ ,c("IV_prev5prop")]), ]
  data_cc_IVestimation_prev10prop  <- data_cc_IVconstruction_prev10prop[complete.cases(data_cc_IVconstruction_prev10prop[ ,c("IV_prev10prop")]), ]
  data_cc_IVestimation_allprevprop <- data_cc_IVconstruction_allprevprop[complete.cases(data_cc_IVconstruction_allprevprop[ ,c("IV_allprevprop")]), ]
  
  
  #
  # Observational estimate, complete case ----------------------------------------
  #
  
  source(paste0(path_general_code, "/observational_estimate_cc.R"))
  
  CaT_cc_est[s]     <- summary(CaT_fit_cc)$coef["X", "Estimate"]
  CaT_cc_SE[s]      <- summary(CaT_fit_cc)$coef["X", "Std. Error"]
  
  CaT_cc_lower[s]   <- CaT_cc_est[s] - 1.96*CaT_cc_SE[s]                          # CI boundaries
  CaT_cc_upper[s]   <- CaT_cc_est[s] + 1.96*CaT_cc_SE[s]
  
  
  #
  # TSLS estimation --------------------------------------------------------------
  #
  
  #
  ## TSLS, ppIV based on Ertefaie et al. 2017  (IVePP) ---------------------------
  #
  
  source(paste0(path_general_code, "/Ertefaie_IVePP.R"))
  
  #
  ## TSLS, ppIV based on the extension to Ertefaie et al. 2017  (IV_ePP_rirs) ----
  #
  
  source("Ertefaie_IVePP_rirs.R")
  
  
  #
  ## TSLS, ppIV based on Abrahamovicz et al. 2011  (IVePP) -----------------------
  #
  
  IV_star_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_IV_star, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_star")
  IV_star_est[s]     <- IV_star_results$estimate
  IV_star_SE[s]      <- IV_star_results$SE
  IV_star_lower[s]   <- IV_star_results$lower_CI
  IV_star_upper[s]   <- IV_star_results$upper_CI
  IV_star_F[s]       <- IV_star_results$F_statistics
  
  #
  ## TSLS, ppIV based on patients' prescription, complete case analysis ----------
  #
  
  IV_prevpatient_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_prevpatient, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_prevpatient")
  IV_prevpatient_est[s]     <- IV_prevpatient_results$estimate 
  IV_prevpatient_SE[s]      <- IV_prevpatient_results$SE
  IV_prevpatient_lower[s]   <- IV_prevpatient_results$lower_CI
  IV_prevpatient_upper[s]   <- IV_prevpatient_results$upper_CI
  IV_prevpatient_F[s]       <- IV_prevpatient_results$F_statistics
  
  
  #
  ## TSLS, ppIV based on all prescriptions, complete case analysis ---------------
  #
  
  IV_allprop_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_allprop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_allprop")
  IV_allprop_est[s]     <- IV_allprop_results$estimate 
  IV_allprop_SE[s]      <- IV_allprop_results$SE
  IV_allprop_lower[s]   <- IV_allprop_results$lower_CI
  IV_allprop_upper[s]   <- IV_allprop_results$upper_CI
  IV_allprop_F[s]       <- IV_allprop_results$F_statistics
  
  IV_alldichmean_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_allprop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_alldichmean")
  IV_alldichmean_est[s]     <- IV_alldichmean_results$estimate 
  IV_alldichmean_SE[s]      <- IV_alldichmean_results$SE
  IV_alldichmean_lower[s]   <- IV_alldichmean_results$lower_CI
  IV_alldichmean_upper[s]   <- IV_alldichmean_results$upper_CI
  IV_alldichmean_F[s]       <- IV_alldichmean_results$F_statistics
  
  IV_alldichmedian_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_allprop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_alldichmedian")
  IV_alldichmedian_est[s]     <- IV_alldichmedian_results$estimate 
  IV_alldichmedian_SE[s]      <- IV_alldichmedian_results$SE
  IV_alldichmedian_lower[s]   <- IV_alldichmedian_results$lower_CI
  IV_alldichmedian_upper[s]   <- IV_alldichmedian_results$upper_CI
  IV_alldichmedian_F[s]       <- IV_alldichmedian_results$F_statistics
  
  
  #
  ## TSLS, ppIV based previous b prescriptions, complete case analysis -----------
  #
  
  
  IV_prev2prop_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_prev2prop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_prev2prop")
  IV_prev2prop_est[s]     <- IV_prev2prop_results$estimate 
  IV_prev2prop_SE[s]      <- IV_prev2prop_results$SE
  IV_prev2prop_lower[s]   <- IV_prev2prop_results$lower_CI
  IV_prev2prop_upper[s]   <- IV_prev2prop_results$upper_CI
  IV_prev2prop_F[s]       <- IV_prev2prop_results$F_statistics
  
  
  IV_prev5prop_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_prev5prop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_prev5prop")
  IV_prev5prop_est[s]     <- IV_prev5prop_results$estimate 
  IV_prev5prop_SE[s]      <- IV_prev5prop_results$SE
  IV_prev5prop_lower[s]   <- IV_prev5prop_results$lower_CI
  IV_prev5prop_upper[s]   <- IV_prev5prop_results$upper_CI
  IV_prev5prop_F[s]       <- IV_prev5prop_results$F_statistics
  
  
  IV_prev10prop_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_prev10prop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_prev10prop")
  IV_prev10prop_est[s]     <- IV_prev10prop_results$estimate 
  IV_prev10prop_SE[s]      <- IV_prev10prop_results$SE
  IV_prev10prop_lower[s]   <- IV_prev10prop_results$lower_CI
  IV_prev10prop_upper[s]   <- IV_prev10prop_results$upper_CI
  IV_prev10prop_F[s]       <- IV_prev10prop_results$F_statistics
  
  
  #
  ## TSLS, ppIV based all previous prescriptions, complete case analysis ---------
  #
  
  IV_allprevprop_results    <- TSLS_continuousY_fun(data = data_cc_IVestimation_allprevprop, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_allprevprop")
  IV_allprevprop_est[s]     <- IV_allprevprop_results$estimate 
  IV_allprevprop_SE[s]      <- IV_allprevprop_results$SE
  IV_allprevprop_lower[s]   <- IV_allprevprop_results$lower_CI
  IV_allprevprop_upper[s]   <- IV_allprevprop_results$upper_CI
  IV_allprevprop_F[s]       <- IV_allprevprop_results$F_statistics
  
  #
  # TSLS benchmarkt estimation -------------------------------------------------
  #
  
  #
  ## TSLS, true PP ppIV, complete case analysis --------------------------------
  #
  
  IV_PP_cc_fitted         <- -1 + b_0[R==0] + 0.5*tx_time[R==0] + b_1_tx_time[R==0] 
  IV_PP_cc_fitted_median  <- median((exp(IV_PP_cc_fitted))/(1 + exp(IV_PP_cc_fitted)))
  IV_PP_cc_fitted_Z       <- ifelse((exp(IV_PP_cc_fitted))/(1 + IV_PP_cc_fitted) > IV_PP_cc_fitted_median, 1, 0)
  
  IV_PP_cc_results        <- TSLS_continuousY_fun(data = data_ccW1, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_PP_cc_fitted_Z")
  IV_PP_cc_est[s]         <- IV_PP_cc_results$estimate
  IV_PP_cc_SE[s]          <- IV_PP_cc_results$SE
  IV_PP_cc_lower[s]       <- IV_PP_cc_results$lower_CI
  IV_PP_cc_upper[s]       <- IV_PP_cc_results$upper_CI
  IV_PP_cc_F[s]           <- IV_PP_cc_results$F_statistics
  
  
  #
  # T2LS IV using true Z and full dataset (no missing values) ------------------
  #
  
  IV_PP_fitted         <- -1 + b_0 + 0.5*tx_time + b_1_tx_time 
  IV_PP_fitted_median  <- median((exp(IV_PP_fitted))/(1 + exp(IV_PP_fitted)))
  IV_PP_fitted_Z       <- ifelse((exp(IV_PP_fitted))/(1 + IV_PP_fitted) > IV_PP_fitted_median, 1, 0)
  
  IV_PP_results     <- TSLS_continuousY_fun(data = data, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_PP_fitted_Z")
  IV_PP_est[s]      <- IV_PP_results$estimate
  IV_PP_SE[s]       <- IV_PP_results$SE
  IV_PP_lower[s]    <- IV_PP_results$lower_CI
  IV_PP_upper[s]    <- IV_PP_results$upper_CI
  IV_PP_F[s]        <- IV_PP_results$F_statistics
  
  
  #
  # End of the simulation --------------------------------------------------------
  #
  
  print(paste0("Simulation run ",s)) 
  


}


#
# Simulation results -----------------------------------------------------------
#


#
## Collection of results -------------------------------------------------------
#

source(paste0(path_general_code, "/collection_results.R"))

save(estimates_sim, file = paste0(path_results, "Scenario", scenario, "/", "estimates_sim.Rdata", sep =""))
save(lower_CI_sim,  file = paste0(path_results, "Scenario", scenario, "/", "lower_CI_sim.Rdata", sep =""))
save(upper_CI_sim,  file = paste0(path_results, "Scenario", scenario, "/", "upper_CI_sim.Rdata", sep =""))
save(results,       file = paste0(path_results, "Scenario", scenario, "/", "results.Rdata", sep =""))

save.image(paste0(path_results, "Scenario", scenario, "/", "workspace.Rdata", sep =""))


#
# Calculation of the performance measures --------------------------------------
#

source(paste0(path_general_code, "/calculation_performance_measures.R"))

save(bias_results_sim,             file = paste0(path_results, "Scenario", scenario, "/", "bias_results_sim.Rdata", sep =""))
save(bias_results,                 file = paste0(path_results, "Scenario", scenario, "/", "bias_result.Rdata", sep =""))
save(SE_results,                   file = paste0(path_results, "Scenario", scenario, "/", "SE_results.Rdata", sep =""))
save(coverage_results,             file = paste0(path_results, "Scenario", scenario, "/", "coverage_results.Rdata", sep =""))
save(rmse_results,                 file = paste0(path_results, "Scenario", scenario, "/", "rmse_results.Rdata", sep =""))
save(results_performance_measures, file = paste0(path_results, "Scenario", scenario, "/", "results_performance_measures.Rdata", sep =""))



