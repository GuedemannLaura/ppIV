#
# Generating abrupt preference change ------------------------------------------
#

# for each provider select a tx_order as i* in between n_j*0.4 and n_j*0.7


min_sample_txorder <- round(mean(n_j)*0.4,0)
max_sample_txorder <- round(mean(n_j)*0.7,0)


sample_txorder_foristar <- sample(min_sample_txorder:max_sample_txorder, N_J, replace = TRUE)

i_star_simulated <- rep(0, times = N_I)

for(g in 1:N_J){
  
  i_star_simulated[which(prov_ID == g)][sample_txorder_foristar[g]] <- 1 
  
  
}

i_star_simulated[changes_PP == FALSE]     <- 0

patientID_i_star_simulated <- pat_ID[i_star_simulated == 1]



if(change_in_PP == "none"){
  
  change_indicator <- rep(0, times = N_I)
  
  
}else{
  
  change_indicator <- rep(0, times = N_I)
  
  for(g in 1:length(ID_changes_PP)){
    
    
    provider <- ID_changes_PP[g]
    change_indicator[which(prov_ID == provider)][tx_order[which(prov_ID == provider)] > which(i_star_simulated[which(prov_ID == provider)] == 1)] <- 1
    
  }
  
}


PP <- PP_initial
PP <- ifelse(change_indicator == 1, PP_afterswitch, PP)                         # indicator of preference at each point in time/ for each i



if(beta_PP_same == TRUE){
  
  beta_PP_j            <-  rep(0.7, times = N_J)
  
}else{
  
  beta_PP_j            <-  runif(N_J, 0.4, 0.7)
  
}


beta_PP          <- rep(beta_PP_j, times = n_j)



# check the results: View(data.frame(prov_ID, tx_order, i_star_simulated, change_indicator, PP))
