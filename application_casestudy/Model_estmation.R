# ------------------------------------------------------------------------------
# Model Estimation -------------------------------------------------------------
# ------------------------------------------------------------------------------


#
# Set working directory and paths ----------------------------------------------
#

setwd(".../application_casestudy")                                              # please set path to folder "application_casestudy"

data_path      <- ".../application_casestudy/data/"                             # please set path to folder "data"
result_path    <- ".../application_casestudy/results/"                          # please set path to folder "results"
function_path  <- " "                                                           # please set path to main folder of this project with R code "functions.R" and "IV_functions.R


#
# Packages ---------------------------------------------------------------------
#

# install.packages("lme4")
library(lme4)

#
# Load dataset -----------------------------------------------------------------
#

load(paste0(data_path,"data_IVestimation_allprevprop.Rdata"))
load(paste0(data_path,"data_IVestimation_allprop.Rdata"))
load(paste0(data_path,"data_IVestimation_ePP.Rdata"))
load(paste0(data_path,"data_IVestimation_IV_star.Rdata"))
load(paste0(data_path,"data_IVestimation_prev2prop.Rdata"))
load(paste0(data_path,"data_IVestimation_prev5prop.Rdata"))
load(paste0(data_path,"data_IVestimation_prev10prop.Rdata"))
load(paste0(data_path,"data_IVestimation_prevpatient.Rdata"))
load(paste0(data_path,"data_ccWY.Rdata"))


#
# Load functions ---------------------------------------------------------------
#

source(paste0(function_path,"IV_functions.R"))


#
# Load Variables for HbA1c models ----------------------------------------------
#

source("variableset_HbA1c.R")


#
# CaT Model --------------------------------------------------------------------
#

CaT_fit_formula <- as.formula(paste(Y, " ~ ", paste0(c(X, all_W), collapse =  " + ")))
CaT_fit         <- lm(CaT_fit_formula, data = data_ccWY)

CaT_est     <- summary(CaT_fit)$coef["treatment", "Estimate"]
CaT_SE      <- summary(CaT_fit)$coef["treatment", "Std. Error"]
CaT_lower   <- CaT_est - 1.96*CaT_SE                         
CaT_upper   <- CaT_est + 1.96*CaT_SE


#
# TSLS estimation --------------------------------------------------------------
#


#
## TSLS, ppIV based on Ertefaie et al. 2017  (IVePP) ---------------------------
#

source("Ertefaie_IVePP.R")


#
## TSLS, ppIV based on the extension to Ertefaie et al. 2017  (IV_ePP_rirs) ----
#


source("Ertefaie_IVePP_rirs.R")


#
## TSLS, ppIV based on Abrahamovicz et al. 2011  (IVePP) -----------------------
#

IV_star_results <- TSLS_continuousY_fun(data = data_IVestimation_IV_star, Y = Y, W = all_W, X = X, Z = "IV_star")
IV_star_est     <- IV_star_results$estimate
IV_star_SE      <- IV_star_results$SE
IV_star_lower   <- IV_star_results$lower_CI
IV_star_upper   <- IV_star_results$upper_CI
IV_star_F       <- IV_star_results$F_statistics


#
## TSLS, ppIV based on patients' previous prescription, complete case analysis -------
#

IV_prevpatient_results <- TSLS_continuousY_fun(data = data_IVestimation_prevpatient, Y = Y, W = all_W, X = X, Z = "IV_prevpatient")
IV_prevpatient_est     <- IV_prevpatient_results$estimate 
IV_prevpatient_SE      <- IV_prevpatient_results$SE
IV_prevpatient_lower   <- IV_prevpatient_results$lower_CI
IV_prevpatient_upper   <- IV_prevpatient_results$upper_CI
IV_prevpatient_F       <- IV_prevpatient_results$F_statistics


#
## TSLS, ppIV based on all prescriptions, complete case analysis ---------------
#

IV_allprop_results          <- TSLS_continuousY_fun(data = data_IVestimation_allprop, Y = Y, W = all_W, X = X, Z = "IV_allprop")
IV_allprop_est              <- IV_allprop_results$estimate 
IV_allprop_SE               <- IV_allprop_results$SE
IV_allprop_lower            <- IV_allprop_results$lower_CI
IV_allprop_upper            <- IV_allprop_results$upper_CI
IV_allprop_F                <- IV_allprop_results$F_statistics

IV_alldichmean_results      <- TSLS_continuousY_fun(data = data_IVestimation_allprop, Y = Y, W = all_W, X = X, Z = "IV_alldichmean")
IV_alldichmean_est          <- IV_alldichmean_results$estimate 
IV_alldichmean_SE           <- IV_alldichmean_results$SE
IV_alldichmean_lower        <- IV_alldichmean_results$lower_CI
IV_alldichmean_upper        <- IV_alldichmean_results$upper_CI
IV_alldichmean_F            <- IV_alldichmean_results$F_statistics

IV_alldichmedian_results    <- TSLS_continuousY_fun(data = data_IVestimation_allprop, Y = Y, W = all_W, X = X, Z = "IV_alldichmedian")
IV_alldichmedian_est        <- IV_alldichmedian_results$estimate 
IV_alldichmedian_SE         <- IV_alldichmedian_results$SE
IV_alldichmedian_lower      <- IV_alldichmedian_results$lower_CI
IV_alldichmedian_upper      <- IV_alldichmedian_results$upper_CI
IV_alldichmedian_F          <- IV_alldichmedian_results$F_statistics


#
## TSLS, ppIV based previous b prescriptions, complete case analysis -----------
#


IV_prev2prop_results     <- TSLS_continuousY_fun(data = data_IVestimation_prev2prop, Y = Y, W = all_W, X = X, Z = "IV_prev2prop")
IV_prev2prop_est         <- IV_prev2prop_results$estimate 
IV_prev2prop_SE          <- IV_prev2prop_results$SE
IV_prev2prop_lower       <- IV_prev2prop_results$lower_CI
IV_prev2prop_upper       <- IV_prev2prop_results$upper_CI
IV_prev2prop_F           <- IV_prev2prop_results$F_statistics


IV_prev5prop_results     <- TSLS_continuousY_fun(data = data_IVestimation_prev5prop, Y = Y, W = all_W, X = X, Z = "IV_prev5prop")
IV_prev5prop_est         <- IV_prev5prop_results$estimate 
IV_prev5prop_SE          <- IV_prev5prop_results$SE
IV_prev5prop_lower       <- IV_prev5prop_results$lower_CI
IV_prev5prop_upper       <- IV_prev5prop_results$upper_CI
IV_prev5prop_F           <- IV_prev5prop_results$F_statistics


IV_prev10prop_results    <- TSLS_continuousY_fun(data = data_IVestimation_prev10prop, Y = Y, W = all_W, X = X, Z = "IV_prev10prop")
IV_prev10prop_est        <- IV_prev10prop_results$estimate 
IV_prev10prop_SE         <- IV_prev10prop_results$SE
IV_prev10prop_lower      <- IV_prev10prop_results$lower_CI
IV_prev10prop_upper      <- IV_prev10prop_results$upper_CI
IV_prev10prop_F          <- IV_prev10prop_results$F_statistics



#
## TSLS, ppIV based all previous prescriptions, complete case analysis ---------
#

IV_allprevprop_results   <- TSLS_continuousY_fun(data = data_IVestimation_allprevprop, Y = Y, W = all_W, X = X, Z = "IV_allprevprop")
IV_allprevprop_est       <- IV_allprevprop_results$estimate 
IV_allprevprop_SE        <- IV_allprevprop_results$SE
IV_allprevprop_lower     <- IV_allprevprop_results$lower_CI
IV_allprevprop_upper     <- IV_allprevprop_results$upper_CI
IV_allprevprop_F         <- IV_allprevprop_results$F_statistics





#
# Collect result in data frame -------------------------------------------------
#


methods      <- c("observational estimate", "IV ePP", "IV ePP (rirs)", "IV star", "IV allprop", "IV alldichmean", "IV alldichmedian", 
                  "IV prevpatient", "IV prev2patient", "IV prev5patient", "IV prev10patient", "IV allprevprop")

estimate     <- c(CaT_est, IV_ePP_est, IVePP_rirs_est, IV_star_est, IV_allprop_est, IV_alldichmean_est, IV_alldichmedian_est, 
                  IV_prevpatient_est, IV_prev2prop_est, IV_prev5prop_est, IV_prev10prop_est, IV_allprevprop_est)

lower_CI     <- c(CaT_lower, IV_ePP_lower, IVePP_rirs_lower, IV_star_lower, IV_allprop_lower, IV_alldichmean_lower, IV_alldichmedian_lower, 
                  IV_prevpatient_lower, IV_prev2prop_lower, IV_prev5prop_lower, IV_prev10prop_lower, IV_allprevprop_lower)

upper_CI     <- c(CaT_upper, IV_ePP_upper, IVePP_rirs_upper, IV_star_upper, IV_allprop_upper, IV_alldichmean_upper, IV_alldichmedian_upper, 
                  IV_prevpatient_upper, IV_prev2prop_upper, IV_prev5prop_upper, IV_prev10prop_upper, IV_allprevprop_upper)

se_estimates <- c(CaT_SE, IV_ePP_SE, IVePP_rirs_SE, IV_star_SE, IV_allprop_SE, IV_alldichmean_SE, IV_alldichmedian_SE, 
                  IV_prevpatient_SE, IV_prev2prop_SE, IV_prev5prop_SE, IV_prev10prop_SE, IV_allprevprop_SE)


results           <- data.frame(methods, estimate, lower_CI, upper_CI, se_estimates)


#
# Save results dataframe -------------------------------------------------------
#


save(results, file = paste0(result_path,"results.Rdata"))




