#-------------------------------------------------------------------------------
# Abrahamowicz et al. 2011 -----------------------------------------------------
#-------------------------------------------------------------------------------


Data <- data_cc_IVconstruction_IV_star

# some data preparation before the loop starts

Data$dji_calculated              <- NA                                          # variable indicates if algorithm was able to indicate dji for provider i (step 3.1)
Data$dji                         <- NA                                          # calculated dji (NA if provider is assumed to have constant preference, or first and last patients) 
Data$PP_constant                 <- NA                                          # variable indicates if we assume that provider preference is constant, based on the algorithm results (step 3 and 4)
Data$patientID_i_star_calculated <- 0                                           # patient ID of the i* calculated from the algorithm/ Variable indicating time point (tx order) i*
Data$i_star_calculated           <- 0                                           # indicator variable of the patient at i*
Data$IV_star_calculated          <- NA                                          # Variable indicates if IV* is calculated for respective patients
Data$IV_star                     <- NA                                          # IV according constructed with Abrahamowicz et al. 2011
Data$I_ki                        <- NA                                          # helper variable needed for step 3.2


Data_providers  <- unique(Data$prov_ID)

# algorithm is applied for each provider in the dataset in a for loop

for(j in 1:length(Data_providers)){
#j <- 2
    
#
# Step 1 ----
#

# patients are already ranked based on how the data is simulated
# see variable Data$tx_order
data_row_provider <- which(Data$prov_ID == Data_providers[j])                 # indications of the data rows for corresponding provider

#
# Step 2 ----
#

nochange_txmodel    <- glm(X ~ W1 + W2, family=binomial(link="logit"), data = Data[data_row_provider, ])
D_nochange_tx_model <- summary(nochange_txmodel)$deviance


#
# Step 3 ---- 
#


#
# Step 3.1 ---
#


if(length(data_row_provider) >= 5){
  
  Data$dji_calculated[data_row_provider[3:(length(data_row_provider)-2)]]                                         <- TRUE
  Data$dji_calculated[data_row_provider[c(1,2, length(data_row_provider)-1, length(data_row_provider))]]         <- FALSE          # no calculation of dji for the first and last two patient within provider
  
  proportion_all_earlier <- cumsum(Data[data_row_provider, ]$X)/1:length(Data[data_row_provider, ]$X)
  proportion_earlier     <- proportion_all_earlier[3:(length(proportion_all_earlier)-2)]
  
  proportion_all_later  <- ifelse(Data[data_row_provider, ]$X==1,revcumsum(Data[data_row_provider, ]$X)-1,revcumsum(Data[data_row_provider, ]$X))/(rev(1:length(Data[data_row_provider, ]$X))-1)
  proportion_later      <- proportion_all_later[3:(length(proportion_all_later)-2)]
  
  Data$dji[data_row_provider[3:(length(data_row_provider)-2)]]                                     <- proportion_earlier - proportion_later
  Data$dji[data_row_provider[c(1,2, (length(data_row_provider)-1), length(data_row_provider))]]   <- NA          
  
  
}else{
  
  print(paste0("Warning: algorithm is not able to calculate dji for provider ", j, ", because n_j < 5", sep = ""))        # for providers with n_j < 5, we will assume no change in preference
  Data$dji_calculated[data_row_provider] <- FALSE
  Data$dji[data_row_provider]            <- 0 
  
  }

#
# Step 3.2 ----
#

if(max(abs(as.vector(na.omit(Data$dji[data_row_provider])))) < 0.2){
  
  Data$PP_constant[data_row_provider]  <- TRUE
  
}else{
  
  Data$PP_constant[data_row_provider]  <- FALSE
  
  Di_change_tx_model <- NULL
  
  subset_patients_dji   <- subset(Data[data_row_provider, ], abs(Data[data_row_provider, ]$dji) >= 0.2)
  
  for(i in 1:dim(subset_patients_dji)[1]){
    
    Data$I_ki[data_row_provider]   <- ifelse(Data$tx_order[data_row_provider] <= subset_patients_dji$tx_order[i], 0, 1)
    
    change_txmodel        <- glm(X ~ W1 + W2 + I_ki, family=binomial(link="logit"), data = Data[data_row_provider, ])   # can lead to warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 
    Di_change_tx_model[i] <- summary(change_txmodel)$deviance
    
  }
  

  #
  # Step 3.3 ---- 
  #
  
  D_ji_star <- min(Di_change_tx_model)                                                                      # deviance of the optimal change model 
  
  # add information of this step to the data
  Data$patientID_i_star_calculated[data_row_provider]                                                       <- min(subset_patients_dji$pat_ID[which(Di_change_tx_model == D_ji_star)])                              # patient ID of the i* calculated from the algorithm 
  
  Data$i_star_calculated[which(Data$pat_ID == unique(Data$patientID_i_star_calculated[data_row_provider]))] <- 1                                                                                                    # indicator variable of the patient at i*

}


#
# Step 4 and 5 ----
#

# Definition of the change in time IV (IV_star)

if(unique(Data$PP_constant[data_row_provider]) == TRUE){
  
  
  #
  # Step 5.1 ----
  #
  
  
  # calculate IV* with Brookhart et al. 2006 (from patient 1 (IV* = NA) to nj)
  Data$IV_star[data_row_provider] <- IV_allprevprop_fun(Data[data_row_provider, ]$X)
  Data$IV_star_calculated[data_row_provider] <- c(FALSE, rep(TRUE, times = (length(data_row_provider)-1)))          # Variable indicates if IV* is calculated for respective patients
  
}else{
  
  #
  # Step 5.2 ---- 
  #
  
  if(D_nochange_tx_model < (D_ji_star + 4)){
    
    Data$PP_constant[data_row_provider] <- TRUE
    
    # calculate IV* with Brookhart et al. 2006 (from patient 1 (IV* = NA) to nj)
    Data$IV_star[data_row_provider] <- IV_allprevprop_fun(Data[data_row_provider, ]$X)
    Data$IV_star_calculated[data_row_provider] <- c(FALSE, rep(TRUE, times = (length(data_row_provider)-1)))          # Variable indicates if IV* is calculated for respective patients
    
    
    Data$patientID_i_star_calculated[data_row_provider] <- 0
    Data$i_star_calculated[data_row_provider]           <- 0    

  }else{
    
    #
    # Step 5.2 ---- 
    #
    
    # new calculation of IV* if i* is not found for the last or second last patient treated by the provider
    # otherwise IV* is calculated as in step 5.1
    
    i_star_temp     <- unique(Data$patientID_i_star_calculated[data_row_provider])
    i_star_row_temp <- data_row_provider[which(Data$pat_ID[data_row_provider] == i_star_temp)]
    
    if(i_star_row_temp == data_row_provider[1] | i_star_row_temp == tail(data_row_provider, n = 2)[1] | i_star_row_temp == tail(data_row_provider, n = 2)[2]){
      
      # calculate IV* with Brookhart et al. 2006 (from patient 1 (IV* = NA) to nj)
      Data$IV_star[data_row_provider] <- IV_allprevprop_fun(Data[data_row_provider, ]$X)
      Data$IV_star_calculated[data_row_provider] <- c(FALSE, rep(TRUE, times = (length(data_row_provider)-1)))          # Variable indicates if IV* is calculated for respective patients
      
      # here: patientID_i_star_calculated and i_star_calculated != 0 because provider change was detected but IV* could not be
      # calculated with Abrahamovicz et al. 2011 algorithm (too little data to split it into "before and after i*")
      # we need at least one data point before i* and two data points after i* star to split the data for the algorithm  
      
    }else{
      
      
      first_patient_row_temp  <- data_row_provider[1]

      IV_star_earlier <- IV_allprevprop_fun(Data$X[first_patient_row_temp:i_star_row_temp])                                       # IV* calculation for all patient up to i*
      IV_star_later   <- IV_allprevprop_fun(Data$X[(i_star_row_temp+1): tail(data_row_provider, n = 1)])                        # IV* calculation for all patient after i* (from i*+1)
      
      Data$IV_star[data_row_provider] <-  c(IV_star_earlier, IV_star_later) 
      Data$IV_star_calculated[data_row_provider] <- is.na(c(IV_star_earlier, IV_star_later) ) == FALSE             # Variable indicates if IV* is calculated for respective patients
      
      
    }
  
  }

}


#print(paste0("calculation of IV* for provider ", j, " of ", N_J))

}

Data$I_ki <- NULL                                                               # we do not need this variable anymore

data_cc_IVconstruction_IV_star <- Data 













                 
                 



