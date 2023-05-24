#-------------------------------------------------------------------------------
# Results Plot -----------------------------------------------------------------
#-------------------------------------------------------------------------------

#
# Pathways and directories -----------------------------------------------------
#

setwd(".../simulationB/simulation_results")                                     # please set path to folder "simulation_results"

#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
library(ggplot2)

# install.packages("ggridges")
library(ggridges)

# install.packages("Cairo")
library(Cairo)


#
# Needed information -----------------------------------------------------------
#


labels_methods     <- c("obs. estimate", "IV(PP)", "IV(PP) cc", "IV ePP", "IV ePP (rirs)", "IV star")
labels_methods     <- factor(labels_methods, levels = labels_methods)
ridgetline_colors2 <- c("#3e7551", "#a66d9c", "#f7cc6f")
colors_lines       <- c("#117733", "#aa4499", "#ffb000")
Beta               <- 1

S           <- 200
n_methods   <- length(labels_methods)
n_scenarios <- 3


#
# Load data --------------------------------------------------------------------
# 

load(".../Scenario2a/estimates_sim.Rdata")                                      # load respective results
estimate_simA <- estimates_sim

load(".../Scenario2b/estimates_sim.Rdata")                                      # load respective results
estimate_simB <- estimates_sim

load(".../Scenario2c/estimates_sim.Rdata")                                      # load respective results
estimate_simC <- estimates_sim

rm(estimates_sim)


results_A <- c(estimate_simA$IV_star_est,  estimate_simA$IVePP_rirs_est, estimate_simA$IV_ePP_est, 
               estimate_simA$IV_PP_cc_est, estimate_simA$IV_PP_est,      estimate_simA$CaT_cc_est)

results_B <- c(estimate_simB$IV_star_est,  estimate_simB$IVePP_rirs_est, estimate_simB$IV_ePP_est, 
               estimate_simB$IV_PP_cc_est, estimate_simB$IV_PP_est,      estimate_simB$CaT_cc_est)

results_C <- c(estimate_simC$IV_star_est,  estimate_simC$IVePP_rirs_est, estimate_simC$IV_ePP_est, 
               estimate_simC$IV_PP_cc_est, estimate_simC$IV_PP_est,      estimate_simC$CaT_cc_est)

#
# Results plot -----------------------------------------------------------------
#

methods   <- as.factor(rep(rep(labels_methods, each = S), times = n_scenarios))
methods   <- factor(methods, levels = labels_methods)

estimates <- c(results_A, results_B, results_C)
scenario  <- as.factor(c(rep("no NAs", times = S*n_methods), rep("MCAR", times = S*n_methods), rep("MNAR", times = S*n_methods)))
scenario  <- factor(scenario, levels = c("no NAs", "MCAR", "MNAR")) 

data_plot <- data.frame(methods, estimates, scenario)







Cairo(file = "Scenario2_simulationB_resultplot_subset_modelbasedmethods.png", 
      type = "png",
      units = "in", 
      width = 20, #10
      height = 16, # 16 
      pointsize = 12, 
      dpi = 72)


ggplot(data_plot) + 
  geom_density_ridges(aes(x = estimates, y = methods, group = interaction(methods,scenario),fill = scenario, color = scenario),
                      alpha = 0.5, rel_min_height = 0.01,
                      quantile_lines=TRUE, 
                      quantile_fun=function(estimates,...)mean(estimates))+
  theme_bw() +
  labs(x = "Distribution of estimates", y = "Density") +
  geom_vline(xintercept = Beta, col = "black") +
  
  scale_fill_manual(values = ridgetline_colors2, name = "missing mechanism", labels = c("no NAs", "MCAR", "MNAR")) + 
  
  scale_y_discrete(labels = rev(labels_methods), expand = expansion(mult = c(0.01, 0.25))) +
  scale_color_cyclical(values = colors_lines, guide = "legend", name = "missing mechanism", labels = c("no NAs", "MCAR", "MNAR")) +
  #+
  #ggtitle("Scenario 2 (simulation B): Missing mechanism")
  theme(
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.text = element_text(size = 25), 
    legend.title = element_text(size = 25),
    legend.text.align = 0) +
  xlim(-4.5,4.5)


dev.off()




