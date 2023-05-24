#-------------------------------------------------------------------------------
# Simulation Study - evaluation of the algorithm by Abrahamowicz et al. 2011 ---
#-------------------------------------------------------------------------------

#
# Paths and directories --------------------------------------------------------
#


setwd(".../simulationA")                                                        # please set path to folder "SimulationA"

folder_main_code  <- ".../simulationA"                                          # please set path to folder "SimulationA"
path_results      <- paste0(folder_main_code, "/simulation_results_evalutation_Abrahamowicz_etal_2011/")
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


PP_change_type                     <- c("abrupt", "smooth")[1]                  # decision on the simulated change type 

n_i_per_provider_simulation       <- c(10, 50, 100, 500, 1000)                  # provider sizes for which simulation will run 

beta_PP_same                      <- c(TRUE)
change_in_PP                      <- "some"
missingness_indata                <- "noNAs"

#
# Set parameters for simulation ------------------------------------------------
#


Beta                     <- 1                                                   # true treatment effect
K                        <- 200                                                 # Simulation runs
switch_probabilities     <- c(0.7, 0.4)


#
# Simulation results -----------------------------------------------------------
#

resultstable_temp <- data.frame(n_j10 = numeric(), n_j50 = numeric(), n_j100 = numeric(), n_j500 = numeric(), n_j1000 = numeric())

FP <- resultstable_temp                                                         # False positive
FN <- resultstable_temp                                                         # False negative
TP <- resultstable_temp                                                         # True positive 
TN <- resultstable_temp                                                         # True negative 

N_realpositives <- N_realnegatives <- resultstable_temp                         # number of provider simulated to have/ not have a change in PP


accuracy   <- resultstable_temp                                                 # accuracy performance measure
TPR <- TNR <- resultstable_temp                                                 # true positive rate and true negative rate
FPR <- FNR <- resultstable_temp                                                 # false positive rate and false negative rate
PPV <- NPV <- resultstable_temp                                                 # positive predicted value and negative predicted value 

TP_i_star_sim_cal_diff <- resultstable_temp                                     # difference of i* simulated and i* calculated within the subgroup of correctly identified providers with a change


#
# Start of the outer for loop - scenarios --------------------------------------
#


#set.seed(22) 
set.seed(456789) 
for(N in 1:length(n_i_per_provider_simulation)){
  # N <- 1
  
  
  n_i_per_provider <- n_i_per_provider_simulation[N]
  
  
  #
  # Start of the inner loop - simulation -----------------------------------------
  #
  
  for(k in 1:K){
    # k <- 1
    
    #
    # Data generation --------------------------------------------------------------
    # 
    
    source("data_generation.R") 
    
    #
    # Data set ---------------------------------------------------------------------
    #
    
    data                               <- data.frame(prov_ID, pat_ID, tx_order, W1_wNA, R, PP, W1, W2, U, X, Y, Y_st)
    data_cc_IVconstruction_IV_star     <- data
    
    
    #
    # Construction of IV star with Abrahamowicz et al. 2011 algorithm ------------
    #
    
    source(paste0(path_general_code, "/IVmethod_Abrahamowicz.R"))
    
    data <- data_cc_IVconstruction_IV_star
    
    #
    # Calculation of performance measures ------------------------------------------
    #
    
    change_simulated                <- rep(0, times = N_J)                                           # which providers have a change simulated?
    change_simulated[ID_changes_PP] <- 1

    change_identified               <- as.vector(tapply(data$i_star_calculated, data$prov_ID, sum))  # which providers have a change identified?
    ID_changes_identified           <- c(1:N_J)[change_identified == 1]
    
    N_realpositives[k,N]        <- length(change_simulated[change_simulated == 1])
    N_realnegatives[k,N]        <- length(change_simulated[change_simulated == 0])
    
    TP[k,N]                     <- sum(as.numeric(change_identified == 1 & change_simulated == 1))
    TN[k,N]                     <- sum(as.numeric(change_identified == 0 & change_simulated == 0))
    FP[k,N]                     <- sum(as.numeric(change_identified == 1 & change_simulated == 0))
    FN[k,N]                     <- sum(as.numeric(change_identified == 0 & change_simulated == 1))
    
    
    accuracy[k,N]               <- (TP[k,N] + TN[k,N])/(TP[k,N] + TN[k,N] + FP[k,N] + FN[k,N])
    
    TPR[k,N]                    <- TP[k,N]/N_realpositives[k,N] 
    TNR[k,N]                    <- TN[k,N]/N_realnegatives[k,N] 
    
    FPR[k,N]                    <- FP[k,N]/N_realnegatives[k,N]
    FNR[k,N]                    <- FN[k,N]/N_realpositives[k,N]
    
    PPV[k,N]                    <- TP[k,N]/(TP[k,N] + FP[k,N])
    NPV[k,N]                    <- TN[k,N]/(TN[k,N] + FN[k,N])
    
    
    # for provider with correctly identified change (TP), how close is calculated i* to simulated i*?
    
    
    if(PP_change_type == "abrupt"){
      
      subset_TP <- data[data$prov_ID%in%ID_changes_PP[ID_changes_PP%in%ID_changes_identified], ]
      
      TP_i_star_sim  <- data$tx_order[i_star_simulated == 1][ID_changes_PP%in%ID_changes_identified]
      TP_i_star_cal  <- subset_TP$tx_order[subset_TP$i_star_calculated == 1] 
      
      TP_i_star_sim_cal_diff[k,N] <- mean(abs(TP_i_star_sim - TP_i_star_cal))
      
      
    }else{
      
      subset_TP <- data[data$prov_ID%in%ID_changes_PP[ID_changes_PP%in%ID_changes_identified], ]
      
      TP_i_star_sim  <- L_j[ID_changes_PP[ID_changes_PP%in%ID_changes_identified]] 
      TP_i_star_cal  <- subset_TP$tx_order[subset_TP$i_star_calculated == 1] 
      
      TP_i_star_sim_cal_diff[k,N] <- mean(abs(TP_i_star_sim - TP_i_star_cal))
      
    }
    
    
    #
    # End of inner for loop - simulation -------------------------------------------
    #
    
    print(paste0("Simulation run ", k, ", n_j = ", mean(n_j), sep = ""))  
    
  }  
  
  #
  # End of outer for loop - scenarios --------------------------------------------
  #
  
}



#
# Collection of simulation results ---------------------------------------------
#


performance_results_sim        <- list(N_realpositives, N_realnegatives, TP, TN, FP, FN, accuracy, TPR, TNR, FPR, FNR, PPV, NPV, TP_i_star_sim_cal_diff)
names(performance_results_sim) <- c("N_realpositives", "N_realnegatives", "TP", "TN", "FP", "FN", "accuracy", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV", "TP_i_star_sim_cal_diff") 




performance_results    <-  sapply(performance_results_sim, function(x) round(colMeans(x), 2))
performance_results[, c("accuracy", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV")] <- performance_results[ , c("accuracy", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV")]*100

rownames(performance_results) <- c("10", "50", "100", "500", "1000")  

#
# Create result plot -----------------------------------------------------------
#

colours_plot <- c("#1C3B54", "#6FA496", "#E29044", "#37769B", "#E6C85F", "#B13A2B", "#6B3074")


## graph for performance measures - rates ----

Cairo(file = paste0(path_results, "simulation_results_performance_measure_rates_",PP_change_type,"_change.png"), 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


matplot(performance_results[ , c("accuracy", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV")], 
        type = c("b"), 
        col = colours_plot[1:7], 
        lty = 1, 
        pch = c(19,2,8,4,23,6,15),
        lwd = 3, 
        cex = 1,#1.2
        xlab="Number of patients treated by each provider", 
        ylab="Performance measures (%)", 
        cex.axis = 1.5, 
        cex.lab = 1.5,
        xaxt='n')
axis(side=1, at=1:length(n_i_per_provider_simulation), labels = rownames(performance_results), cex.axis = 1.5)

dev.off()


Cairo(file = paste0(path_results, "simulation_results_performance_measure_legend.png"), 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)
plot.new()
legend("center", c("accuracy", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV"), #5,58 # "topright"
       col = colours_plot[1:7], 
       lty = 1,
       pch = c(19,2,8,4,23,6,15),
       lwd = 2, 
       cex = 1.2, #1.2
       seg.len = 4#,
       #x.intersp = 0.2, 
       #y.intersp = 0.1,
       #bty = "n"
) 

dev.off()




#
# Save the results -------------------------------------------------------------
#

save.image(paste0(path_results, "workspace_Abrahamowicz_etal11_",                     PP_change_type,".R"))
save(performance_results_sim, file = paste0(path_results, "performance_results_sim_", PP_change_type,".R"))
save(performance_results,     file = paste0(path_results, "performance_results_",     PP_change_type,".R"))




