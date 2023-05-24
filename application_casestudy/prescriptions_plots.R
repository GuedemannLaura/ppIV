#
# Applied Case study - Graphs --------------------------------------------------
# 


#
# Path -------------------------------------------------------------------------
#


setwd(".../application_casestudy")                                              # please set path to folder "application_casestudy"
data_path      <- ".../application_casestudy/data/"                             # please set path to folder "data"
result_path    <- ".../application_casestudy/results/"                          # please set path to folder "results"


#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
library(ggplot2)

# install.packages(tidyverse)
library(tidyverse)

# install.packages("Cairo")
library(Cairo)

library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)


#
# Time trend plot --------------------------------------------------------------
# 


## Load data -------------------------------------------------------------------

# Data is copied from slade server (file: "application_case_study_graphs.R")
load(paste0(data_path,"prescription.Rdata"))

data <- data.frame(prescription)
dim(data)
rm(prescription)

## Make plot -------------------------------------------------------------------



data$other    <- data$Acarbose + data$Glinide + data$INS
data$Acarbose <- NULL
data$Glinide  <- NULL
data$INS      <- NULL

N_prescription <- as.numeric(apply(data, 1, sum))

data_percent       <- data.frame(apply(data, 2, function(x) x/N_prescription)*100)
years <- 2013:2020


df_reshaped <- data.frame(x = years,                           
                          y = c(data_percent$MFN, data_percent$SU, data_percent$TZD,
                                data_percent$DPP4, data_percent$GLP1, data_percent$SGLT2, 
                                data_percent$other),
                          group = c(rep("MFN", nrow(data_percent)),
                                    rep("SU", nrow(data_percent)),
                                    rep("TZD", nrow(data_percent)),
                                    rep("DPP4", nrow(data_percent)),
                                    rep("GLP1", nrow(data_percent)),
                                    rep("SGLT2", nrow(data_percent)),
                                    rep("other", nrow(data_percent))))

df_reshaped$group <- factor(df_reshaped$group, levels = c("MFN", "SU", "TZD", "DPP4", "GLP1", "SGLT2", "other"))


group_colors <- c("yellow3", "darkseagreen4", "firebrick", "dodgerblue4", "orange1", "maroon3","grey")


Cairo(file = paste0(result_path, "prescription_time_trend.png"), 
      type = "png",
      units = "in", 
      width = 12, #10 
      height = 8, 
      pointsize = 12, 
      dpi = 72)

ggplot(data_percent, aes(x = years)) +
  theme_bw() + 
  ylim(0,50) +
  ylab("% prescription") +
  geom_line(aes( y = MFN,   color = "MFN"),   size  = 1.1) +
  geom_point(aes(y = MFN,   color = "MFN",    shape = "MFN"), size = 4) +
  geom_line(aes( y = SU,    color = "SU"),    size  = 1.1) +
  geom_point(aes(y = SU,    color = "SU",     shape = "SU"), size = 4) +
  geom_line(aes( y = TZD,   color = "TZD"),   size  = 1.1) +
  geom_point(aes(y = TZD,   color = "TZD",    shape = "TZD"), size = 4) +
  geom_line(aes( y = DPP4,  color = "DPP4i"), size  = 1.1) +
  geom_point(aes(y = DPP4,  color = "DPP4i",  shape = "DPP4i"), size = 4) +
  geom_line(aes( y = GLP1,  color = "GLP1"), size  = 1.1) +
  geom_point(aes(y = GLP1,  color = "GLP1",  shape = "GLP1"), size = 4) +
  geom_line(aes( y = SGLT2, color = "SGLT2i"),size  = 1.1) +
  geom_point(aes(y = SGLT2, color = "SGLT2i", shape = "SGLT2i"), size = 4) +
  geom_line(aes( y = other, color = "other"),size  = 1.1) +
  geom_point(aes(y = other, color = "other", shape = "other"), size = 4) +
  
  scale_colour_manual(values = c("MFN" = group_colors[1], 
                                 "SU" = group_colors[2], 
                                 "TZD" = group_colors[3],
                                 "GLP1" = group_colors[4],
                                 "DPP4i" = group_colors[5], 
                                 "SGLT2i" = group_colors[6],
                                 "other" = group_colors[7]),
                      name = "") +
  scale_shape_manual(values = c("MFN" = 2, 
                                "SU" = 4, 
                                "TZD" = 8,
                                "GLP1" = 15,
                                "DPP4i" = 19, 
                                "SGLT2i" = 17, 
                                "other" = 23), 
                     name = "") + 
  theme(legend.position="bottom") + 
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(name ="years", breaks = c(2013:2020),
                     labels = as.character(c(2013:2020)), expand = c(0.025,0.025)) +
  theme(legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(4, 'cm'),
        legend.text=element_text(size=18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18), 
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18)) 
  
  

dev.off()


#
# regional prescription differences --------------------------------------------
#

# http://stavrakoudis.econ.uoi.gr/r-eurostat/drawing-maps-of-europe.html
# https://en.wikipedia.org/wiki/First-level_NUTS_of_the_European_Union#United_Kingdom

## Load data -------------------------------------------------------------------

# Data is copied from slade server (file: "application_case_study_graphs.R")
load(paste0(data_path,"prescriptions_region.Rdata"))

datar <- data.frame(prescriptions_region)
dim(datar)
rm(prescriptions_region)

N_region        <- as.numeric(apply(datar, 1, sum))
datar_percent   <- data.frame(apply(datar, 2, function(x) x/N_region)*100)

datar_percent[11, ] <- datar_percent[8,] + datar_percent[10, ]                  # combine: South central and south east


NUTS_name_data <- c("North East", "North West", "Yorkshire and the Humber", "East Midlands",
                    "West Midlands", "East of England", "South West", "Greater London",
                    "South East") 

# dataset for SGLT2 

SGLT2_data <- data.frame(datar_percent[c(1,2,3,4,5,6,7,9,11), "SGLT2"], NUTS_name_data)
colnames(SGLT2_data) <- c("value", "NUTS_name")


## Make plot -------------------------------------------------------------------



SHP_1      <- get_eurostat_geospatial(resolution = 10, nuts_level = 1, year = 2021)  # get map data on NUTS 1 level
UK_country <- tibble(geo = "UK", name = "United Kingdom")                            # selection of UK


UK_alone1 <- SHP_1 %>% 
  select(geo = CNTR_CODE, geometry, NUTS_ID) %>% 
  inner_join(UK_country, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

UK_alone1$NUTS_name <- c("South West", "Wales", "Northern Ireland", "North East", 
                         "North West", "Yorkshire and the Humber", "East Midlands", 
                         "West Midlands", "East of England", "Greater London", 
                         "South East", "Scotland")

UK_data_merged      <- merge(UK_alone1, SGLT2_data)                             # data including the percentages of SGLT2i prescriptions

UK_data_merged %>% 
  ggplot(aes(fill = value)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme_void()




#
# Differences within region of different provider ------------------------------
#


## Load data -------------------------------------------------------------------

# Data is copied from slade server (file: "application_case_study_graphs.R")
load(paste0(data_path,"prescription_SGLT2_percentage_regionpracid.Rdata"))

datarp <- prescription_SGLT2_percentage_regionpracid
rm(prescription_SGLT2_percentage_regionpracid)

labels_region <- c("North East", "North West", "Yorkshire and the Humber", "East Midlands",
                   "West Midlands", "East of England", "South West", "South Central", 
                   "General London", "South East Coast")


par(mar=c(5,6+3,4,1)+2)

Cairo(file = paste0(result_path, "SGLT2_prescription_pracidregion.png"), 
      type = "png",
      units = "in", 
      width = 10, #10 
      height = 6, #8
      pointsize = 12, 
      dpi = 72)

plot(datarp$SGLT2_perscription_percentage, datarp$region,
     ylab = "", xlab = "Proportion of SGLT2i prescriptions relative to all other antidiabetic agents (%)",
     yaxt="n", pch = 1, cex = 1.5)

axis(2, at=c(1,2,3,4,5,6,7,8,9,10),labels=labels_region, col.axis="black", las=2)


dev.off()


#par(mar=c(5,6,4,1))





















