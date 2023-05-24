#-------------------------------------------------------------------------------
# Set parameters for simulation ------------------------------------------------
#-------------------------------------------------------------------------------


# vector with methods names compared in this simulation 
Beta             <- 1                                                           # true treatment effect


S                <- 200                                                         # Simulation runs

beta_W1_Z        <- 0.25                                                        # influence of W1 on Z (IV)
beta_W2_Z        <- 0.25                                                        # influence of W2 on Z (IV)
  
beta_Z_X         <- 1                                                           # influence of Z on X

mu_W             <- rnorm(1,0,0.5)
mu_W1            <- mu_W
mu_W2            <- mu_W
sd_W1 <- sd_W2   <- 2
