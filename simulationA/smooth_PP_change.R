#
# Generating smooth preference change ------------------------------------------
#

# in our simulation S_j and L_j where generated in a way to ensure that the time of preference change 
# was within the observed time period. this differs from the data generation of Abrahamowicz et al. 2011.
# From their supplementary material it is clear that the change can start/ finish before/after the observed period with
# S_j ~ U(-0.5*n_j, 0.9*n_j) and L_j ~ U(0.6*n_j, 1.2*n_j) 
# while this is general possible, it might lead to change that is unobserved and therefore generate less change of preference
# in the simulation.
# We decided to deviate from the data generation process of Abrahamowicz et al. 2011, to control the amount of change in the simulation 
# The main aim is to generate smooth change in contrast to abrupt change, which will be achieved with the following code. 


# index of patient after whom preference of provider j starts to change 
S_j                <- rep(NA, times = N_J)
S_j[ID_changes_PP] <- round(runif(length(ID_changes_PP), 0.4*n_j, 0.7*n_j),0)    

# length of the interval during which the change in preference occurred
time_left_to_change <- n_j - S_j

L_j                <- rep(NA, times = N_J)
L_j[ID_changes_PP] <- round(time_left_to_change[ID_changes_PP] * runif(length(ID_changes_PP), 0.6, 0.8), 0)


# final or starting strength of provider j's preference for drug B
P_j                <- rep(NA, times = N_J)
P_j[ID_changes_PP] <- runif(length(ID_changes_PP), 0.5, 0.9)



# PP:      variable indicating the preference for X = 1 for each j and i
# beta_PP: variable indicating the preference strength for X = 1 for each j and i

vector_change_indication <- rep("no_change", times = N_J)                       # this vector indicates how the provider change their preference
vector_change_indication[PP_initial_j == 1 & PP_afterswitch_j == 0] <- "BtoA"
vector_change_indication[PP_initial_j == 0 & PP_afterswitch_j == 1] <- "AtoB"

PP      <- rep(NA, times = length(pat_ID))
beta_PP <- rep(NA, times = length(pat_ID))

for(h in 1:length(vector_change_indication)){
  if(vector_change_indication[h] == "no_change"){
    
    PP[prov_ID == h]      <- PP_initial_j[h]
    beta_PP[prov_ID == h] <- 0.7                                                # taken from the case of abrupt change
    
  }else if(vector_change_indication[h] == "AtoB"){
    
    # PP
    PP[prov_ID == h][tx_order[prov_ID == h] <= S_j[h]]                 <- 0
    PP[prov_ID == h][tx_order[prov_ID == h] > S_j[h]]                  <- 1
    
    # beta_PP
    beta_PP[prov_ID == h][tx_order[prov_ID == h] <= S_j[h]]            <- 0
    beta_PP[prov_ID == h][tx_order[prov_ID == h] >= (S_j[h] + L_j[h])] <- P_j[h]
    beta_PP[prov_ID == h][is.na(beta_PP[prov_ID == h])]                <- P_j[h]*((which(is.na(beta_PP[prov_ID == h])) - S_j[h])/L_j[h])
    
  }else if(vector_change_indication[h] == "BtoA"){
    
    # PP
    PP[prov_ID == h][tx_order[prov_ID == h] < (S_j[h] + L_j[h])]    <- 1
    PP[prov_ID == h][tx_order[prov_ID == h] >= (S_j[h] + L_j[h])]   <- 0
    
    # beta_PP
    beta_PP[prov_ID == h][tx_order[prov_ID == h] <= S_j[h]]              <- P_j[h]
    beta_PP[prov_ID == h][tx_order[prov_ID == h] >= (S_j[h] + L_j[h])]   <- 0
    beta_PP[prov_ID == h][is.na(beta_PP[prov_ID == h])]                  <- P_j[h]*(1-((which(is.na(beta_PP[prov_ID == h])) - S_j[h])/L_j[h]))
    
  }
  
}









