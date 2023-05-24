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
prov_ID  <- rep(1:N_J, times = n_j)                                             # ID variable for the provider (group)
pat_ID   <- seq(from = 1,  to = N_I)                                            # ID variable for the patient (over the whole dataset)
tx_order <- rowid(prov_ID, prefix = NULL)                                       # order of the treatment prescription within each provider, this is also an Id variable for patient within provider

#
# Patient characteristics (W, U) -----------------------------------------------
# 

W1               <- rnorm(N_I,mu_W1, sd_W1)                                     # measured confounders
W2               <- rnorm(N_I,mu_W2, sd_W2)   

U                <- rnorm(N_I,0,1)                                              # unmeasured confounder


#
# Providers' preference -------------------------------------------------------
#


if(change_in_PP == "none"){
  
  PP_initial_j     <- rbinom(N_J, 1, 0.4)                                       # provider preference (0: preference for drug A, 1: preference for drug B)
  PP_initial       <- rep(PP_initial_j, times = n_j)                            # variable for initial provider preference (0: preference for drug A, 1: preference for drug B)
  PP_initial_drugA <- as.numeric(table(PP_initial_j)["0"])                      # number of providers who initially preferred drug A
  PP_initial_drugB <- as.numeric(table(PP_initial_j)["1"])                      # number of providers who initially preferred drug B
  
  switch_fromAtoB  <- rep(0, times = PP_initial_drugA)                          # providers who switch their initially preference from A -> B (1: switch, 0: no switch)
  switch_fromBtoA  <- rep(0, times = PP_initial_drugB)                          # providers who switch their initially preference from B -> A (1: switch, 0: no switch)
  
  PP_afterswitch_j <- PP_initial_j                                              # coding the switch of preferences
  PP_afterswitch   <- rep(PP_afterswitch_j, times = n_j)                        # variable for provider preference after the switch (0: preference for drug A, 1: preference for drug B)
  
  ID_changes_PP    <- which(PP_initial_j != PP_afterswitch_j)                   # IDs of provider who change preferences (both ways)
  n_changes_PP     <- length(ID_changes_PP)                                     # total number of  provider who change preferences (both ways)
  
  changes_PP       <- rep(FALSE, times = N_I)                                   # variable indicating if provider changes preference
  
}else if(change_in_PP == "some"){
  
  PP_initial_j     <- rbinom(N_J, 1, 0.4)                                       # provider preference (0: preference for drug A, 1: preference for drug B)
  PP_initial       <- rep(PP_initial_j, times = n_j)                            # variable for initial provider preference (0: preference for drug A, 1: preference for drug B)
  PP_initial_drugA <- as.numeric(table(PP_initial_j)["0"])                      # number of providers who initially preferred drug A
  PP_initial_drugB <- as.numeric(table(PP_initial_j)["1"])                      # number of providers who initially preferred drug B
  
  switch_fromAtoB  <- rbinom(PP_initial_drugA, 1, switch_probabilities[1])      # providers who switch their initially preference from A -> B (1: switch, 0: no switch)
  switch_fromBtoA  <- rbinom(PP_initial_drugB, 1, switch_probabilities[2])      # providers who switch their initially preference from B -> A (1: switch, 0: no switch)
  
  PP_afterswitch_j                      <- PP_initial_j                         # coding the switch of preferences
  PP_afterswitch_j[PP_initial_j == 0]   <- ifelse(switch_fromAtoB == 1, 1, 0)     
  PP_afterswitch_j[PP_initial_j == 1]   <- ifelse(switch_fromBtoA == 1, 0, 1)
  PP_afterswitch                        <- rep(PP_afterswitch_j, times = n_j)   # variable for provider preference after the switch (0: preference for drug A, 1: preference for drug B)
  
  ID_changes_PP    <- which(PP_initial_j != PP_afterswitch_j)                   # IDs of provider who change preferences (both ways)
  n_changes_PP     <- length(ID_changes_PP)                                     # total number of  provider who change preferences (both ways)
  
  changes_PP       <- prov_ID%in%ID_changes_PP
  
}else if(change_in_PP == "all"){
  
  PP_initial_j     <- rbinom(N_J, 1, 0.4)                                       # provider preference (0: preference for drug A, 1: preference for drug B)
  PP_initial       <- rep(PP_initial_j, times = n_j)                            # variable for initial provider preference (0: preference for drug A, 1: preference for drug B)
  PP_initial_drugA <- as.numeric(table(PP_initial_j)["0"])                      # number of providers who initially preferred drug A
  PP_initial_drugB <- as.numeric(table(PP_initial_j)["1"])                      # number of providers who initially preferred drug B
  
  switch_fromAtoB  <- rep(1, times = PP_initial_drugA)                          # providers who switch their initially preference from A -> B (1: switch, 0: no switch)
  switch_fromBtoA  <- rep(1, times = PP_initial_drugB)                          # providers who switch their initially preference from B -> A (1: switch, 0: no switch)
  
  PP_afterswitch_j <- 1-PP_initial_j                                            # coding the switch of preferences
  PP_afterswitch   <- rep(PP_afterswitch_j, times = n_j)                        # variable for provider preference after the switch (0: preference for drug A, 1: preference for drug B)
  
  ID_changes_PP    <- which(PP_initial_j != PP_afterswitch_j)                   # IDs of provider who change preferences (both ways)
  n_changes_PP     <- length(ID_changes_PP)                                     # total number of  provider who change preferences (both ways)
  
  changes_PP       <- rep(TRUE, times = N_I)
  
}


if(PP_change_type == "abrupt"){
  
  source("abrupt_PP_change.R")
  
}else{
  
  source("smooth_PP_change.R")
  
}

# treatment decision (X, binary) ----



eta_X            <- -1 + beta_PP*PP + U + W1 + W2

prop_X           <- exp(eta_X)/(1+exp(eta_X))
X                <- rbinom(length(prop_X),1,prop_X)                             



# outcome (Y, continuous) ----



Y      <- 0.5*W1 + 0.5*W2 + Beta*X + 2*U + rnorm(N_I, 0, 0.5)              
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
  
  prop_R   <- ((exp(1 + W1 + W2 + U + 0.6*Y_st))/(1 + (exp(1 + W1 + W2 + U + 0.6*Y_st)))) * ((exp(1 + v + W1*v + W2*v))/(1+exp(1 + v + W1*v + W2*v))) 
  R        <- rbinom(N_I,1,prop_R)                              
  
}

# introduction of missingness (in X1) ----

W1_wNA       <- ifelse(R == 1 , NA, W1)








