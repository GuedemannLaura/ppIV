# ------------------------------------------------------------------------------
# Ertefaie et al. 2017  (IV_ePP) ----------------------------------------------
# ------------------------------------------------------------------------------


# Step 1: 

IVePP_fit1_formula <- as.formula(paste(X, " ~ ", paste0(c(all_W, "(1|pracid)"), collapse =  " + ")))
IVePP_fit1         <- glmer(IVePP_fit1_formula, family = binomial("logit"), data = data_IVestimation_ePP)        # uses complete case data on W (and Y)    

b_j_hat             <- ranef(IVePP_fit1)$pracid$"(Intercept)"
b_j_hat_median      <- median((exp(b_j_hat))/(1 + exp(b_j_hat)))

Z_j_ePP             <- ifelse((exp(b_j_hat))/(1 + exp(b_j_hat)) > b_j_hat_median, 1, 0)

# Data cleaning step:

# did step 1 calculate random intercept for all provider?
# if all rows for a provider have NAs, no random effect will be estimated
# these provider need to be excluded from further analysis

length(unique(data_IVestimation_ePP$pracid)) == length(Z_j_ePP)
# some (8) provider need to be excluded!

pracid_raneff <- as.numeric(rownames(ranef(IVePP_fit1)$pracid))                          # these are the provider for which a random intercept has been estimated

data_ePP2     <- data_IVestimation_ePP[data_IVestimation_ePP$pracid%in%pracid_raneff,  ] # we lose 170 data points of 8 provider 

#table(pracid_raneff%in%unique(data_ePP2$pracid))



# Step 2: 

data_ePP2$IV_ePP  <- rep(Z_j_ePP, times = as.vector(table(data_ePP2$pracid)))


IV_ePP_results    <- TSLS_continuousY_fun(data = data_ePP2, Y = Y, W = W_noNAs, X = X, Z = "IV_ePP")
IV_ePP_est        <- IV_ePP_results$estimate
IV_ePP_SE         <- IV_ePP_results$SE
IV_ePP_lower      <- IV_ePP_results$lower_CI
IV_ePP_upper      <- IV_ePP_results$upper_CI
IV_ePP_F          <- IV_ePP_results$F_statistics  










