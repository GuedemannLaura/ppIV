# ------------------------------------------------------------------------------
# Extension of Ertefaie et al. 2017  (IV_ePP_rirs) -----------------------------
# ------------------------------------------------------------------------------


# Step 1: 

IVePP_rirs_fit1_formula <- as.formula(paste(X, " ~ ", paste0(c(all_W, "tx_order","(1+tx_order|pracid)"), collapse =  " + ")))
IVePP_rirs_fit1         <- glmer(IVePP_rirs_fit1_formula, family = binomial("logit"), data = data_IVestimation_ePP)

b_intercept_rirs    <- summary(IVePP_rirs_fit1)$coef["(Intercept)", "Estimate"]
b_T_rirs            <- summary(IVePP_rirs_fit1)$coef["tx_order", "Estimate"]
b0_j_hat_rirs       <- ranef(IVePP_rirs_fit1)$pracid$"(Intercept)"
b1_j_hat_rirs       <- ranef(IVePP_rirs_fit1)$pracid$"tx_order"


# Data cleaning step:

# did step 1 calculate random intercept for all provider?
# if all rows for a provider have NAs, no random effect will be estimated
# these provider need to be excluded from further analysis

length(unique(data_IVestimation_ePP$pracid)) == length(b0_j_hat_rirs)
length(unique(data_IVestimation_ePP$pracid)) == length(b1_j_hat_rirs)
# some (8) provider need to be excluded! 

pracid_raneff_rirs <- as.numeric(rownames(ranef(IVePP_rirs_fit1)$pracid))       # these are the provider for which a random intercept has been estimated

data_ePP2_rirs     <- data_IVestimation_ePP[data_IVestimation_ePP$pracid%in%pracid_raneff_rirs,  ] # we lose 170 data points of 8 provider 

#table(pracid_raneff_rirs%in%unique(data_ePP_rirs2$pracid))



# Construct the IV as fitted values using the estimated random intercepts and random slopes

data_ePP2_rirs$b_int  <- rep(b_intercept_rirs, times = dim(data_ePP2_rirs)[1])
data_ePP2_rirs$b_T    <- rep(b_T_rirs,         times = dim(data_ePP2_rirs)[1])
data_ePP2_rirs$b0_hat <- rep(b0_j_hat_rirs,    times = as.vector(table(data_ePP2_rirs$pracid)))
data_ePP2_rirs$b1_hat <- rep(b1_j_hat_rirs,    times = as.vector(table(data_ePP2_rirs$pracid)))

data_ePP2_rirs$fitted         <- data_ePP2_rirs$b_int + data_ePP2_rirs$b0_hat + (data_ePP2_rirs$b_T + data_ePP2_rirs$b1_hat)*data_ePP2_rirs$tx_order 
IVePP_rirs_fitted_median      <- median((exp(data_ePP2_rirs$fitted))/(1 + exp(data_ePP2_rirs$fitted)))
data_ePP2_rirs$IV_ePP_rirs    <- ifelse((exp(data_ePP2_rirs$fitted))/(1 + exp(data_ePP2_rirs$fitted)) > IVePP_rirs_fitted_median, 1, 0) 


# Step 2: 


# TSLS estimation 

IVePP_rirs_results    <- TSLS_continuousY_fun(data = data_ePP2_rirs, Y = Y, W = W_noNAs, X = X, Z = "IV_ePP_rirs")
IVePP_rirs_est        <- IVePP_rirs_results$estimate
IVePP_rirs_SE         <- IVePP_rirs_results$SE
IVePP_rirs_lower      <- IVePP_rirs_results$lower_CI
IVePP_rirs_upper      <- IVePP_rirs_results$upper_CI
IVePP_rirs_F          <- IVePP_rirs_results$F_statistics  









