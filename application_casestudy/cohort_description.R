# ------------------------------------------------------------------------------
# Cohort Description -----------------------------------------------------------
# ------------------------------------------------------------------------------


#
# Set working directory and paths ----------------------------------------------
#

setwd(".../application_casestudy")                                              # please set path to folder "application_casestudy"

data_path      <- ".../application_casestudy/data/"                             # please set path to folder "data"


#
# Packages ---------------------------------------------------------------------
#


# install.packages("table1")
library(table1)


#
# Load dataset -----------------------------------------------------------------
#

load(paste0(data_path,"study_cohort_3year.Rdata"))

load(paste0(data_path,"data_IVestimation_allprevprop.Rdata"))
load(paste0(data_path,"data_IVestimation_allprop.Rdata"))
load(paste0(data_path,"data_IVestimation_ePP.Rdata"))
load(paste0(data_path,"data_IVestimation_IV_star.Rdata"))
load(paste0(data_path,"data_IVestimation_prev2prop.Rdata"))
load(paste0(data_path,"data_IVestimation_prev5prop.Rdata"))
load(paste0(data_path,"data_IVestimation_prev10prop.Rdata"))
load(paste0(data_path,"data_IVestimation_prevpatient.Rdata"))
load(paste0(data_path,"data_ccWY.Rdata"))



#dim(data_IVestimation_allprevprop)
#length(unique(data_IVestimation_allprevprop$pracid))

round(table(data_IVestimation_allprop$drugclass)/length(data_IVestimation_allprop$drugclass)*100,2)


#
# Load Variables for HbA1c models ----------------------------------------------
#

source("variableset_HbA1c.R")


#
# Table 1 for CPRD cohort ------------------------------------------------------
#

formula_table1  <- as.formula(paste("~ ", paste0(c(all_W), collapse =  " + "), "| drugclass "))
table_1         <- table1(formula_table1, droplevels = FALSE, data = study_cohort)
print(table_1)


subset_SGLT2 <- subset(study_cohort, study_cohort$drugclass == "SGLT2")
subset_DPP4  <- subset(study_cohort, study_cohort$drugclass == "DPP4")


table(subset_DPP4$tx_startyear)
round(table(subset_DPP4$tx_startyear)/length(subset_DPP4$tx_startyear)*100,2)

table(subset_DPP4$INS)/length(subset_DPP4$INS)*100

mean(as.numeric(subset_SGLT2$deprivation), na.rm = T)
sd(as.numeric(subset_SGLT2$deprivation), na.rm = T)



#
# Description of missing data --------------------------------------------------
#


# missing data for variables: ethnicity_cat, deprivation, smoking_cat, , preegfr, prebmi, prealt, outcome_hba1c
# apply(study_cohort[, c(all_W, Y)], 2, function(x) table(is.na(x)))


subset_SGLT2 <- subset(study_cohort, study_cohort$drugclass == "SGLT2")
subset_DPP4  <- subset(study_cohort, study_cohort$drugclass == "DPP4")

table(is.na(subset_DPP4$outcome_hba1c))
table(is.na(subset_DPP4$outcome_hba1c))/length(subset_DPP4$outcome_hba1c)*100

table(is.na(study_cohort$outcome_hba1c))
table(is.na(study_cohort$outcome_hba1c))/length(study_cohort$outcome_hba1c)*100












