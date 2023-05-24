# ------------------------------------------------------------------------------
# Data preparation - Generation of the ppIVs -----------------------------------
# ------------------------------------------------------------------------------


#
# Set working directory and paths ----------------------------------------------
#

setwd(".../application_casestudy")                                              # please set path to folder "application_casestudy"

data_path      <- ".../application_casestudy/data/"                             # please set path to folder "data"
result_path    <- ".../application_casestudy/results/"                          # please set path to folder "results"
function_path  <- " "                                                           # please set path to main folder of this project with R code "functions.R" and "IV_functions.R


#
# Load dataset -----------------------------------------------------------------
#

load(paste0(data_path,"study_cohort_3year.Rdata"))


#
# Create ordered dataset and treatment order variable --------------------------
#

# order patients within provider according to their prescription date and give them order ID (ascending order to most recent prescription)

study_cohort$tx_order       <- rep(NA, times = dim(study_cohort)[1])
pracid_loop                 <- unique(study_cohort$pracid)

for(i in 1:length(pracid_loop)){
  
  subset_pracid  <- subset(study_cohort, study_cohort$pracid == pracid_loop[i])
  columns_pracid <- which(study_cohort$pracid == pracid_loop[i])
  ordered_dates  <- order(subset_pracid$dstartdate)
  
  study_cohort$tx_order[columns_pracid][ordered_dates] <- 1:dim(subset_pracid)[1]
  
  
  
  print(i)
}

data_ordered    <- study_cohort[with(study_cohort, order(study_cohort$pracid, study_cohort$tx_order)), ]

#View(data_ordered[,c("pracid", "patid", "dstartdate","tx_order")])

rm(study_cohort)

#
# Load functions ---------------------------------------------------------------
#

source(paste0(function_path,"IV_functions.R"))
source(paste0(function_path,"functions.R"))


#
# Load Variables for HbA1c models ----------------------------------------------
#

source("variableset_HbA1c.R")


#
# complete case dataset for all measured confounder and  the outcome variable ----
#


data_ccWY <- data_ordered[complete.cases(data_ordered[ ,c(all_variables)]), ]   # most ppIV construction methods are applied on complete case data for TSLS estimation


#
# complete case dataset for the outcome variable -------------------------------
#


data_ccY <- data_ordered[complete.cases(data_ordered[ ,c(Y)]), ]                # for method of Ertefaie et al. 2017 and its extension method


#
# Exclusion of provider which are too small ------------------------------------
#

# this method is specific to each ppIV construction method

data_cc_IVconstruction_IV_star     <- exclude_small_provider_fun(data_ccWY, min_nj = 5,  prov_ID = "pracid")
data_cc_IVconstruction_prevpatient <- exclude_small_provider_fun(data_ccWY, min_nj = 2,  prov_ID = "pracid")
data_cc_IVconstruction_allprop     <- exclude_small_provider_fun(data_ccWY, min_nj = 2,  prov_ID = "pracid")
data_cc_IVconstruction_prev2prop   <- exclude_small_provider_fun(data_ccWY, min_nj = 3,  prov_ID = "pracid")
data_cc_IVconstruction_prev5prop   <- exclude_small_provider_fun(data_ccWY, min_nj = 6,  prov_ID = "pracid")
data_cc_IVconstruction_prev10prop  <- exclude_small_provider_fun(data_ccWY, min_nj = 11, prov_ID = "pracid")
data_cc_IVconstruction_allprevprop <- exclude_small_provider_fun(data_ccWY, min_nj = 2,  prov_ID = "pracid")

data_cc_IVconstruction_ePP         <- exclude_small_provider_fun(data_ccY,  min_nj = 2,  prov_ID = "pracid")



#
# Construction of the ppIVs ----------------------------------------------------
#

# construction of IV_ePP and IV_ePP_rirs in respective R code                   
source("IVmethod_Abrahamowicz.R")           

data_cc_IVconstruction_prevpatient$IV_prevpatient <- as.vector(unlist(tapply(data_cc_IVconstruction_prevpatient$treatment, data_cc_IVconstruction_prevpatient$pracid, IV_prevpatient_fun)))
data_cc_IVconstruction_allprop$IV_allprop         <- as.vector(unlist(tapply(data_cc_IVconstruction_allprop$treatment,     data_cc_IVconstruction_allprop$pracid,     IV_allprop_fun)))

mean_IV_allprop   <- mean(as.vector(tapply(data_cc_IVconstruction_allprop$IV_allprop,   data_cc_IVconstruction_allprop$pracid, function(x) x[1])))
median_IV_allprop <- median(as.vector(tapply(data_cc_IVconstruction_allprop$IV_allprop, data_cc_IVconstruction_allprop$pracid, function(x) x[1])))

data_cc_IVconstruction_allprop$IV_alldichmean     <- ifelse(data_cc_IVconstruction_allprop$IV_allprop >= mean_IV_allprop, 1, 0)
data_cc_IVconstruction_allprop$IV_alldichmedian   <- ifelse(data_cc_IVconstruction_allprop$IV_allprop >= median_IV_allprop, 1, 0)

data_cc_IVconstruction_prev2prop$IV_prev2prop     <- as.vector(unlist(tapply(data_cc_IVconstruction_prev2prop$treatment,   data_cc_IVconstruction_prev2prop$pracid,  function(X) IV_nprevprop_fun(X = X, n_prev = 2))))
data_cc_IVconstruction_prev5prop$IV_prev5prop     <- as.vector(unlist(tapply(data_cc_IVconstruction_prev5prop$treatment,   data_cc_IVconstruction_prev5prop$pracid,  function(X) IV_nprevprop_fun(X = X, n_prev = 5))))
data_cc_IVconstruction_prev10prop$IV_prev10prop   <- as.vector(unlist(tapply(data_cc_IVconstruction_prev10prop$treatment,  data_cc_IVconstruction_prev10prop$pracid, function(X) IV_nprevprop_fun(X = X, n_prev = 10))))
data_cc_IVconstruction_allprevprop$IV_allprevprop <- as.vector(unlist(tapply(data_cc_IVconstruction_allprevprop$treatment, data_cc_IVconstruction_allprevprop$pracid, IV_allprevprop_fun)))


# View(data_cc_IVconstruction_allprevprop[,c("pracid", "patid", "dstartdate","tx_order", "treatment","IV_allprevprop")])



#
# Deletion of patients for which IV cannot be calculated -----------------------
#

# data preparation for the TSLS estimation

data_IVestimation_IV_star     <- data_cc_IVconstruction_IV_star[complete.cases(data_cc_IVconstruction_IV_star[ ,c("IV_star")]), ]
data_IVestimation_prevpatient <- data_cc_IVconstruction_prevpatient[complete.cases(data_cc_IVconstruction_prevpatient[ ,c("IV_prevpatient")]), ]
data_IVestimation_allprop     <- data_cc_IVconstruction_allprop
data_IVestimation_prev2prop   <- data_cc_IVconstruction_prev2prop[complete.cases(data_cc_IVconstruction_prev2prop[ ,c("IV_prev2prop")]), ]
data_IVestimation_prev5prop   <- data_cc_IVconstruction_prev5prop[complete.cases(data_cc_IVconstruction_prev5prop[ ,c("IV_prev5prop")]), ]
data_IVestimation_prev10prop  <- data_cc_IVconstruction_prev10prop[complete.cases(data_cc_IVconstruction_prev10prop[ ,c("IV_prev10prop")]), ]
data_IVestimation_allprevprop <- data_cc_IVconstruction_allprevprop[complete.cases(data_cc_IVconstruction_allprevprop[ ,c("IV_allprevprop")]), ]
data_IVestimation_ePP         <- data_cc_IVconstruction_ePP



#
# Save the datasets for TSTL estimation ----------------------------------------
#

save(data_IVestimation_IV_star,     file = paste0(data_path,"data_IVestimation_IV_star.Rdata"))
save(data_IVestimation_prevpatient, file = paste0(data_path,"data_IVestimation_prevpatient.Rdata"))
save(data_IVestimation_allprop,     file = paste0(data_path,"data_IVestimation_allprop.Rdata"))
save(data_IVestimation_prev2prop,   file = paste0(data_path,"data_IVestimation_prev2prop.Rdata"))
save(data_IVestimation_prev5prop,   file = paste0(data_path,"data_IVestimation_prev5prop.Rdata"))
save(data_IVestimation_prev10prop,  file = paste0(data_path,"data_IVestimation_prev10prop.Rdata"))
save(data_IVestimation_allprevprop, file = paste0(data_path,"data_IVestimation_allprevprop.Rdata"))
save(data_IVestimation_ePP,         file = paste0(data_path,"data_IVestimation_ePP.Rdata"))
save(data_ccWY,                     file = paste0(data_path,"data_ccWY.Rdata"))












