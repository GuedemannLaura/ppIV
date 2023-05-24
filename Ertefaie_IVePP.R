# ------------------------------------------------------------------------------
# Ertefaie et al. 2017  (IV_ePP) ----------------------------------------------
# ------------------------------------------------------------------------------


# Step 1: 

IVePP_fit1         <- glmer(X ~ W1_wNA + W2  + (1|prov_ID), family = binomial("logit"), data = data_ccW1)            

b_j_hat             <- ranef(IVePP_fit1)$prov_ID$"(Intercept)"
b_j_hat_median      <- median((exp(b_j_hat))/(1 + exp(b_j_hat)))

Z_j_ePP             <- ifelse((exp(b_j_hat))/(1 + exp(b_j_hat)) > b_j_hat_median, 1, 0)
Z_j_ePP_all_same[s] <- ifelse(length(unique(Z_j_ePP)) == 1, 1, 0)

# Data cleaning step:

# Step 2 is calculated on the full dataset but we need to exclude provider which are excluded 
# because all of their patient data on W1 is NA
# for these patients no Z_j_ePP was estimated


if(length(Z_j_ePP) == N_J){
  
  data_ePP2 <- data
  
  
}else{
  
  data_ePP2 <- data[-which(data$prov_ID%in%data_ccW1$prov_ID == FALSE),  ]
  
}



# Step 2: 

IV_ePP            <- rep(Z_j_ePP, times = as.vector(table(data_ePP2$prov_ID)))

if(sum(is.na(data$W1_wNA)) == 0){
  
  W_ePP <- c("W1", "W2")
  
}else{
  
  W_ePP <- c("W2")
  
} 
  
IV_ePP_results    <- TSLS_continuousY_fun(data = data_ePP2, Y = "Y", W = W_ePP, X = "X", Z = "IV_ePP")
IV_ePP_est[s]     <- IV_ePP_results$estimate
IV_ePP_SE[s]      <- IV_ePP_results$SE
IV_ePP_lower[s]   <- IV_ePP_results$lower_CI
IV_ePP_upper[s]   <- IV_ePP_results$upper_CI
IV_ePP_F[s]       <- IV_ePP_results$F_statistics  


prov_out_IVconstruction_ePP[s] <- N_J - length(unique(data_ePP2$prov_ID))








