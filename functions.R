#-------------------------------------------------------------------------------
# Functions --------------------------------------------------------------------
#-------------------------------------------------------------------------------




#
# Function to exclude providers with too little data after complete case ------ 
#


exclude_small_provider_fun <- function(df, min_nj, prov_ID){
  
  exclude_prov_ID <- as.numeric(names(table(df[, prov_ID]))[table(df[, prov_ID]) < min_nj])
  df_new         <- df[!df[, prov_ID]%in%exclude_prov_ID,  ]
  
  return(df_new)
  
}


# Description:
## Function to exclude provider which are too small for IV construction/estimation

# Input: 
## df: data frame
## min_nj: minimum provider size needed for respective construction method
## prov_ID provider ID variable

# Output: 
## function returns df without provider that are too small

