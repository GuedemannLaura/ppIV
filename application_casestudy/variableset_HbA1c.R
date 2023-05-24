#
# Variable set for osmotic symptoms models -------------------------------------
#


X                   <- "treatment"
Y                   <- "outcome_hba1c"
W_general           <- c("ethnicity_cat", "deprivation", "smoking_cat" , "dstartdate_age",
                         "gender_chr", "tx_startyear", "dstartdate_dm_dur_all", 
                         "prehba1c", "preegfr", "prebmi", "prealt", "drugline_all", "ncurrtx", "INS")

all_W               <- c(W_general)

W_noNAs             <- c(all_W[!all_W%in%c("ethnicity_cat", "deprivation", "smoking_cat", "preegfr", "prebmi", "prealt", "prehba1c")])
all_variables       <- c(X, Y,  W_general)





