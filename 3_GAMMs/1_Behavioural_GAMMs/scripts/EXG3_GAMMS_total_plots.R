library(tidyverse)
library(ggpubr)

miss_dir = ("~/Documents/Merv/final_GAMMS/scripts/total/missing_plots")
setwd("~/Documents/Merv/final_GAMMS/EXG3_GAMMS/Total_sample/output/fullmodel")

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# go trials measures

gort <- loadRData("GoRT_best_plot.RData")  + theme_pubr()
mut <- loadRData("mu.true_best_plot.RData")  + theme_pubr()
sigt <- loadRData("sigma.true_best_plot.RData") + theme_pubr()

plot = ggarrange(gort,                                                 # First row with scatter plot
          ggarrange(mut, sigt, ncol = 2, labels = c("B", "C"),legend = 'none'), # Second row with box and dot plots
          nrow = 2, 
          labels = "A",
          common.legend = T,legend = 'bottom' # Labels of the scatter plot
) 

plot


# stop trial measures

ssrt <- loadRData("SSRT_best_plot.RData") + theme_pubr()
mu <- loadRData("~/Documents/Merv/final_GAMMS/scripts/total/missing_plots/muS_best_plot.RData") + theme_pubr()
sig <- loadRData("sigmaS_best_plot.RData") + theme_pubr()
tau <- loadRData("tauS_best_plot.RData") + theme_pubr()

plot2 = ggarrange(ssrt,                                                 # First row with scatter plot
                 ggarrange(mu, sig, tau, ncol = 3, labels = c("B", "C", "D"),legend = 'none'), # Second row with box and dot plots
                 nrow = 2, 
                 labels = "A",
                 common.legend = T, legend = 'bottom' # Labels of the scatter plot
) 

plot2


# attention measures

gf <- loadRData("~/Documents/Merv/final_GAMMS/scripts/total/missing_plots/prob_GF_best_plot.RData")  + theme_pubr()
tf <- loadRData("~/Documents/Merv/final_GAMMS/scripts/total/missing_plots/prob_TF_best_plot.RData") + theme_pubr()

plot3 = ggarrange(ggarrange(gf, tf, ncol = 2, labels = c("A"), common.legend = T, legend = 'bottom'))
plot3




setwd("~/Documents/Merv/final_GAMMS/EXG3_GAMMS")
ggsave('EXG3_GAMMS_GO_total.png',plot=plot, width=7.5,height=5)
ggsave('EXG3_GAMMS_STOP_total.png',plot=plot2, width=8,height=5)
ggsave('EXG3_GAMMS_ATTN_total.png',plot=plot3, width=7.5,height=5)
