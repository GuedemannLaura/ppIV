#-------------------------------------------------------------------------------
# IV functions -----------------------------------------------------------------
#-------------------------------------------------------------------------------


#
# IV constructed as the proportion of X=1 of all previous patients (Brookhart et al. 2006) ---- 
#

IV_allprevprop_fun <- function(X){
  
  if(length(X) <=1) stop("Error: IV can not be calculated. Patient data on more than one patient is necessary.")
  
  IV_allprevprop    <- c(0, cumsum(X)[-length(X)])/(seq(from = 0, to = (length(X)-1)))
  IV_allprevprop[1] <- NA
    
  return(IV_allprevprop)
  
} 

# Description:
## Function calculating the IV by Brokkhart et al. 2006
## as the proportion of X=1 of all previous patients

# Input: 
## X vector with binary treatment variable (X=0,1), numeric

# Output: 
## function returns a vector with NA as first element, 
## because IV can not be calculated for the first patient treated by provider

# Example:
## A <- c(0,1,0,1,0,1,1,0)
## IV_allprevprop_fun(A)
## application for more than one provider
## tapply(data$X, data$prov_ID, IV_allprevprop_fun)
## output in a list --> as.vector(unlist())

#
# IV constructed as proportion of all patients treated by the provider ---------
#

IV_allprop_fun <- function(X){
  
  if(length(X) <=1) stop("Error: IV can not be calculated. Patient data on more than one patient is necessary.")
  
  IV_allprop    <- rep(prop(data.frame(X), X == 1, na.rm = TRUE), times = length(X))

  return(IV_allprop)
  
}

# Description:
## Function calculating the IV by Brokkhart et al. 2006
## as the proportion of X=1 of all patients treated by provider

# Input: 
## X vector with binary treatment variable (X=0,1), numeric

# Output: 
## function returns a vector of the length X (no NAs if last patient has X recorded), 

# Example:
## A <- c(0,1,0,1,0,1,1,0)
## IV_allproportion_fun(A)


#
# IV constructed as the % of n_prev patients treated by provider ---------------
#

IV_nprevprop_fun <- function(X, n_prev){
  
  if(length(X) <= n_prev) stop("Error: IV can not be calculated. Patient data n_prev + 1 patients is necessary.")
  
  IV_nprevproportion <- rep(NA, times = n_prev)
  
  for(a in (n_prev+1):length(X)){
    
    temp                  <- X[(a-n_prev):(a-1)]
    IV_nprevproportion[a] <- prop(data.frame(temp), temp == 1, na.rm = TRUE)
    
  }
  
  return(IV_nprevproportion)
  
}

# Description:
## Function calculating the IV by Hennessey et al. 2008 Uddin et al. 2016
## as the proportion of X=1 of n_prev previously treated patients 

# Input: 
## X vector with binary treatment variable (X=0,1), numeric
## n_prev: number of previous patients to consider in IV calculation

# Output: 
## function returns a vector of the length X (NAs for the first n_prev patients), 

# Example:
## A <- c(0,1,0,1,0,1,1,0)
## n_prev  <- c(2,5,10)
## IV_nprevproportion_fun(X = A, n_prev = n_prev[1])
## IV_nprevproportion_fun(X = A, n_prev = n_prev[2])
## IV_nprevproportion_fun(X = A, n_prev = n_prev[3])



#
# IV constructed based on the previous patients' prescription (Brookhart et al. 2006) ---- 
#

IV_prevpatient_fun <- function(X){
  
  if(length(X) <=1) stop("Error: IV can not be calculated. Patient data on more than one patient is necessary.")
  
  IV_prevpatient <- c(NA, X[-length(X)])
  
  return(IV_prevpatient)
}


# Description:
## Function calculating the IV by Brokkhart et al. 2006
## as the prescription of the previous patient 

# Input: 
## X vector with binary treatment variable (X=0,1), numeric

# Output: 
## function returns a vector with NA as first element, 
## because IV can not be calculated for the first patient treated by provider

# Example:
## A <- c(0,1,0,1,0,1,1,0)
## IV_prevpatient_fun(A)


#
# TSLS IV estimation (continuous Y) --------------------------------------------
#

TSLS_continuousY_fun <- function(data, Y, W, X, Z){
  
  # Model formulas
  TSLS_formula1   <- as.formula(paste(X, " ~ ", paste0(c(W, Z),      collapse =  " + ")))
  TSLS_formula2   <- as.formula(paste(Y, " ~ ", paste0(c("Xhat", W), collapse =  " + ")))
  
  # Model estimation
  TSLS_model1         <- glm(TSLS_formula1, family = binomial("logit"), data = data)
  TSLS_model1_summary <- summary(TSLS_model1)

  data$Xhat    <- fitted(TSLS_model1)
  
  # for Ertefaie et al. 2017 method: singulartiy of first stage model can lead to 
  # IV_ePP not varying over j, no coefficient will be estimated and
  # hence F statistics can not be calculated
  if(is.na(as.vector(TSLS_model1$coefficients[Z]))){
    
    F_statistics <- NA}else{
    
    F_statistics <- (summary(TSLS_model1)$coefficients[Z,"z value"])^2
    
  }
  
  TSLS_model2  <- lm(TSLS_formula2, data = data)
  TSLS_model2_summary <- summary(TSLS_model2)
  
  # extracting results
  estimate     <- summary(TSLS_model2)$coefficients["Xhat","Estimate"]
  SE           <- summary(TSLS_model2)$coefficients["Xhat","Std. Error"]
  lower_CI     <- estimate - 1.96*SE
  upper_CI     <- estimate + 1.96*SE
  
  # data clearing
  data$Xhat    <- NULL
  
  return(list(TSLS_model1_summary = TSLS_model1_summary, TSLS_model2_summary = TSLS_model2_summary, 
              estimate = estimate, SE = SE, 
              lower_CI = lower_CI, upper_CI = upper_CI, F_statistics = F_statistics)) 
  
}

# Description:
## Function executing the TSLS IV estimation
## using a predefined set of measured confounders, outcome, 
## treatment variable and IV

# Input: 
## data: dataframe including all needed variables for the models
## Y: name of the outcome variable in data (continuous)
## X: name of the treatment variable in data (binary)
## W: vector with names of measured confounder in data
## Z: name of the Instrumental Variable in data

# Output: 
## TSLS_model1: model object from glm of the TSLS first stage model
## TSLS_model2: model object from lm of the TSLS second stage model
## estimate: TSLS IV treatment effect estimate  
## SE: standard error of estimate
## lower_CI: lower confidence interval (alpha = 5%) of estimate
## upper_CI: upper confidence interval (alpha = 5%) of estimate

# Example:
## W <- c("W1", "W2")
## Z <- "IV_star"
## Y <- "Y"
## X <- "X"

## results <- TSLS_continuousY_fun(data = data, Y = "Y", W = c("W1", "W2"), X = "X", Z = "IV_star") 
## results$estimate






