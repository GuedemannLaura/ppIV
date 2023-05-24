#-------------------------------------------------------------------------------
# Data generation --------------------------------------------------------------
#-------------------------------------------------------------------------------

#
# Model parameter --------------------------------------------------------------
#

beta_W1_Z        <- 0.25                                                        # influence of W1 on Z (IV)
beta_W2_Z        <- 0.25                                                        # influence of W2 on Z (IV)

beta_Z_X         <- 1                                                           # influence of Z on X

mu_W             <- rnorm(1,0,0.5)
mu_W1            <- mu_W
mu_W2            <- mu_W
sd_W1 <- sd_W2   <- 2


#
# Study population -------------------------------------------------------------
#

N_J      <- 100                                                                 # total number of providers in population
n_j      <- rep(n_i_per_provider, times = N_J)                                  # number of patients treated by each provider
N_I      <- sum(n_j)                                                            # total number of patients in population
prov_ID  <- rep(1:N_J, times = n_j)                                             # ID variable for the providers (group)
pat_ID   <- seq(from = 1,  to = N_I)                                            # ID variable for the patient (over the whole dataset)
tx_order <- rowid(prov_ID, prefix = NULL)                                       
tx_time  <- rep(rep(c(1:12), each = (n_i_per_provider/12)), times = 100)        # time point of treatment


#
# Patient characteristics (W, U) -----------------------------------------------
# 

W1               <- rnorm(N_I,mu_W1, sd_W1)                                     # measured confounders
W2               <- rnorm(N_I,mu_W2, sd_W2)
U                <- rnorm(N_I,0,1)                                              # unmeasured confounder


# Random effects (b0 and b1) ----

sigma_raneff <- matrix(c(2.5, 1, 1, 2.5), nrow = 2)
raneff       <- rmvnorm(N_J, mean = c(0, 0), sigma = sigma_raneff)

b_0          <- rep(raneff[ , 1], times = n_i_per_provider)
b_1          <- rep(raneff[ , 2], times = n_i_per_provider)


# treatment decision (X, binary) ----

b_1_tx_time      <- b_1*tx_time 

eta_X            <- -1 + b_0 + 0.5*tx_time + b_1_tx_time + U + W1 + W2          # X is generated using a random intercept random slope model 

prop_X           <- exp(eta_X)/(1+exp(eta_X))
X                <- rbinom(length(prop_X),1,prop_X)                             


# outcome (Y, continuous) ----

Y          <- 0.5*W1 + 0.5*W2 + Beta*X + 2*U + rnorm(N_I, 0, 0.5)               
Y_st       <- (Y-mean(Y))/sd(Y)                                                                 


# providers influence on missingness (v) ----

v_j           <- sample(seq(-2, 2, 0.001), N_J)                                 # practitioners contribution to missingness
v             <- rep(v_j, times = n_j)


# Missingness (R) ----
# 1: no missingness, 2: MCAR, 3: MNAR 

if(missingness_indata == "noNAs"){
  
  prop_R   <- rep(0, times = N_I)
  R        <- rbinom(N_I,1,prop_R) 
  
}else if(missingness_indata == "MCAR"){
  
  prop_R   <- rep(0.4, times = N_I)                                             # Missingness R = 1 ~ 40%
  R        <- rbinom(N_I,1,prop_R)                                              # missingness indicator (1: value is missing)
  
}else if(missingness_indata == "MNAR"){
  
  #prop_R   <- ((exp(1 + W1 + W2 + U + 0.6*Y_st))/(1 + (exp(1 + W1 + W2 + U + 0.6*Y_st)))) * ((exp(1 + v + W1*v + W2*v))/(1+exp(1 + v + W1*v + W2*v))) 
  prop_R   <- ((exp(2 + W1 + W2 + U + 20*Y_st))/(1 + (exp(2 + W1 + W2 + U + 20*Y_st)))) * ((exp(2 + v + W1*v + W2*v))/(1 + exp(2 + v + W1*v + W2*v))) 
  R        <- rbinom(N_I,1,prop_R)                              
  
}



# introduction of missingness (in X1) ----

W1_wNA       <- ifelse(R == 1 , NA, W1)







