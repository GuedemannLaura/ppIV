#
# Calculation of the performance measures --------------------------------------
#

#
# bias -------------------------------------------------------------------------
#

bias_results_sim           <- estimates_sim - Beta
bias_results               <- as.data.frame(apply(bias_results_sim, 2, mean))
colnames(bias_results)     <- "bias"

#
# SE ---------------------------------------------------------------------------
# Morris et al. 2019, empSE


SE_results                 <- apply(estimates_sim, 2, function(x) (sd(x))/(sqrt(S)))

#
# coverage (in %) --------------------------------------------------------------
#

coverage_test              <- as.data.frame(Beta > lower_CI_sim & Beta < upper_CI_sim)
coverage_test              <- as.data.frame(apply(coverage_test, 2, as.numeric))

coverage_results           <- apply(coverage_test, 2, function(x) round(mean(x)*100, 2))

#
# RMSE -------------------------------------------------------------------------
#

rmse_results               <- apply(estimates_sim, 2, function(x) sqrt(mean((x - Beta)^2)))  

#
# Collection of the results - performance measures -----------------------------
#


results_performance_measures           <- as.data.frame(t(rbind(bias_results$bias, SE_results, coverage_results, rmse_results)))
colnames(results_performance_measures) <- c("bias", "SE", "coverage", "RMSE") 







