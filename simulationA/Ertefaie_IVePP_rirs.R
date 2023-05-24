# ------------------------------------------------------------------------------
# Extension of Ertefaie et al. 2017  (IV_ePP_rirs) -----------------------------
# ------------------------------------------------------------------------------


# Step 1: 

IVePP_rirs_fit1 <- glmer(X ~ W1_wNA + W2 + tx_order + (1+tx_order|prov_ID), family = binomial("logit"), data = data_ccW1)

b_intercept_rirs    <- summary(IVePP_rirs_fit1)$coef["(Intercept)", "Estimate"]
b_T_rirs            <- summary(IVePP_rirs_fit1)$coef["tx_order", "Estimate"]
b0_j_hat_rirs       <- ranef(IVePP_rirs_fit1)$prov_ID$"(Intercept)"
b1_j_hat_rirs       <- ranef(IVePP_rirs_fit1)$prov_ID$"tx_order"


# Data cleaning step:

# Step 2 is calculated on the full dataset but we need to exclude provider which are excluded 
# because all of their patient data on W1 is NA
# for these patients no random effect was estimated

if(length(b0_j_hat_rirs) == N_J){
  
  data_ePP2_rirs <- data
  
  
}else{
  
  data_ePP2_rirs <- data[-which(data$prov_ID%in%data_ccW1$prov_ID == FALSE),  ]
  
}


# Construct the IV as fitted values using the estimated random intercepts and random slopes

data_ePP2_rirs$b_int  <- rep(b_intercept_rirs, times = dim(data_ePP2_rirs)[1])
data_ePP2_rirs$b_T    <- rep(b_T_rirs,         times = dim(data_ePP2_rirs)[1])
data_ePP2_rirs$b0_hat <- rep(b0_j_hat_rirs,    times = as.vector(table(data_ePP2_rirs$prov_ID)))
data_ePP2_rirs$b1_hat <- rep(b1_j_hat_rirs,    times = as.vector(table(data_ePP2_rirs$prov_ID)))

data_ePP2_rirs$fitted         <- data_ePP2_rirs$b_int + data_ePP2_rirs$b0_hat + (data_ePP2_rirs$b_T + data_ePP2_rirs$b1_hat)*data_ePP2_rirs$tx_order 
IVePP_rirs_fitted_median      <- median((exp(data_ePP2_rirs$fitted))/(1 + exp(data_ePP2_rirs$fitted)))
data_ePP2_rirs$IV_ePP_rirs    <- ifelse((exp(data_ePP2_rirs$fitted))/(1 + exp(data_ePP2_rirs$fitted)) > IVePP_rirs_fitted_median, 1, 0) 


# Step 2: 


if(sum(is.na(data_ePP2_rirs$W1_wNA)) == 0){
  
  W_ePP <- c("W1", "W2")
  
}else{
  
  W_ePP <- c("W2")
  
} 
  

# TSLS estimation 

IVePP_rirs_results    <- TSLS_continuousY_fun(data = data_ePP2_rirs, Y = "Y", W = W_ePP, X = "X", Z = "IV_ePP_rirs")
IVePP_rirs_est[s]     <- IVePP_rirs_results$estimate
IVePP_rirs_SE[s]      <- IVePP_rirs_results$SE
IVePP_rirs_lower[s]   <- IVePP_rirs_results$lower_CI
IVePP_rirs_upper[s]   <- IVePP_rirs_results$upper_CI
IVePP_rirs_F[s]       <- IVePP_rirs_results$F_statistics  

prov_out_IVconstruction_ePP_rirs[s] <- N_J - length(unique(data_ePP2_rirs$prov_ID))








