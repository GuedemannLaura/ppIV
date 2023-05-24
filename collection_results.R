#
# Collect the results - table --------------------------------------------------
#


estimates_sim    <- as.data.frame(cbind(CaT_cc_est,   IV_PP_est,      IV_PP_cc_est, IV_ePP_est,   IVePP_rirs_est,        IV_star_est,       IV_allprop_est,    IV_alldichmean_est,    IV_alldichmedian_est,   IV_prevpatient_est,   IV_prev2prop_est,   IV_prev5prop_est,   IV_prev10prop_est,   IV_allprevprop_est))
SE_sim           <- as.data.frame(cbind(CaT_cc_SE,    IV_PP_SE,       IV_PP_cc_SE,  IV_ePP_SE,    IVePP_rirs_SE,         IV_star_SE,        IV_allprop_SE,     IV_alldichmean_SE,     IV_alldichmedian_SE,    IV_prevpatient_SE,    IV_prev2prop_SE,    IV_prev5prop_SE,    IV_prev10prop_SE,    IV_allprevprop_SE))
lower_CI_sim     <- as.data.frame(cbind(CaT_cc_lower, IV_PP_cc_lower, IV_PP_lower,  IV_ePP_lower, IVePP_rirs_lower,      IV_star_lower,     IV_allprop_lower,  IV_alldichmean_lower,  IV_alldichmedian_lower, IV_prevpatient_lower, IV_prev2prop_lower, IV_prev5prop_lower, IV_prev10prop_lower, IV_allprevprop_lower))
upper_CI_sim     <- as.data.frame(cbind(CaT_cc_upper, IV_PP_cc_upper, IV_PP_upper,  IV_ePP_upper, IVePP_rirs_upper,      IV_star_upper,     IV_allprop_upper,  IV_alldichmean_upper,  IV_alldichmedian_upper, IV_prevpatient_upper, IV_prev2prop_upper, IV_prev5prop_upper, IV_prev10prop_upper, IV_allprevprop_upper))
F_statistics_sim <- as.data.frame(cbind(              IV_PP_cc_F,     IV_PP_F,      IV_ePP_F,     IVePP_rirs_F,          IV_star_F,         IV_allprop_F,      IV_alldichmean_F,      IV_alldichmedian_F,     IV_prevpatient_F,     IV_prev2prop_F,     IV_prev5prop_F,     IV_prev10prop_F,     IV_allprevprop_F))

estimates     <- colMeans(x = estimates_sim,     na.rm = TRUE)
SE            <- colMeans(x = SE_sim,            na.rm = TRUE)
lower_CI      <- colMeans(x = lower_CI_sim,      na.rm = TRUE)
upper_CI      <- colMeans(x = upper_CI_sim,      na.rm = TRUE)
F_statistics  <- c(NA, colMeans(x = F_statistics_sim,  na.rm = TRUE))

results       <- as.data.frame(t(rbind(estimates, SE, lower_CI, upper_CI, F_statistics)))










