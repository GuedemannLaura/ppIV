# ------------------------------------------------------------------------------
# Result plot ------------------------------------------------------------------
# ------------------------------------------------------------------------------


#
# Set working directory and paths ----------------------------------------------
#

setwd(".../application_casestudy")                                              # please set path to folder "application_casestudy"
result_path    <- ".../application_casestudy/results/"                          # please set path to folder "results"


#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
library(ggplot2)

# install.packages("Cairo")
library(Cairo)


#
# load results dataframe -------------------------------------------------------
#

load(paste0(result_path,"results.Rdata"))


#
# Some data preparation --------------------------------------------------------
#


results$methods <- factor(results$method, levels = rev(c("observational estimate", "IV ePP", "IV ePP (rirs)", 
                                                          "IV star", "IV allprop", "IV alldichmean", "IV alldichmedian", 
                                                          "IV prevpatient", "IV prev2patient", "IV prev5patient", 
                                                          "IV prev10patient", "IV allprevprop")))

#
# Forstplot --------------------------------------------------------------------
#

Cairo(file = paste0(result_path, "point_estimates_CI.pdf"), #  # point_estimates_CI_completecase
      type = "pdf",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


ggplot(results, aes(x = estimate, y =  methods)) +
  geom_point(size = 5) +
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI, height = 0.3), size = 0.7) +
  geom_vline(aes(xintercept = 0), colour = "black")+
  ylab("") + 
  xlab("Difference in HbA1c (mmol/mol)") +
  theme_bw() + 
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20))

dev.off()









