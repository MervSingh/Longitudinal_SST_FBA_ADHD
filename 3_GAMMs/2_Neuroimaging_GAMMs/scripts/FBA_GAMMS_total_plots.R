library(tidyverse)
library(ggpubr)

setwd("~/Desktop/final_GAMMS/FBA_GAMMS/Total_sample/output/fullmodel")

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}




# RIGHT HEMI
R_IFG_PRESMA <- loadRData("LOGFC_RH_IFG_PRESMA_best_plot.RData")
R_IFG_PRESMA =  R_IFG_PRESMA + ylab("IFG-preSMA logFC") + theme(axis.text.y  = element_text(size = 12)) + facet_wrap(~group) + theme_pubr()

R_PRESMA_STN <- loadRData("LOGFC_RH_PRESMA_STN_best_plot.RData")
R_PRESMA_STN= R_PRESMA_STN + ylab("preSMA-STN logFC") + theme(axis.text.y = element_text(size = 12))  + facet_wrap(~group) + theme_pubr()

R_IFG_STN <- loadRData("LOGFC_RH_IFG_STN_best_plot.RData")
R_IFG_STN = R_IFG_STN + ylab("IFG-STN logFC") + theme(axis.text.y = element_text(size = 12))  + facet_wrap(~group) + theme_pubr()


# LEFT HEMI 
L_IFG_PRESMA <- loadRData("LOGFC_LH_IFG_PRESMA_best_plot.RData")
L_IFG_PRESMA = L_IFG_PRESMA + ylab("IFG-preSMA logFC") + theme(axis.text.y = element_text(size = 12))  + facet_wrap(~group) + theme_pubr()

L_PRESMA_STN <- loadRData("LOGFC_LH_PRESMA_STN_best_plot.RData")
L_PRESMA_STN = L_PRESMA_STN + ylab("preSMA-STN logFC") + theme(axis.text.y = element_text(size = 12))  + facet_wrap(~group) + theme_pubr()

L_IFG_STN <- loadRData("LOGFC_LH_IFG_STN_best_plot.RData")
L_IFG_STN = L_IFG_STN + ylab("IFG-STN logFC") + theme(axis.text.y = element_text(size = 12))  + facet_wrap(~group) + theme_pubr()



p = ggarrange(R_IFG_PRESMA,R_PRESMA_STN,R_IFG_STN,
              common.legend = T,
              labels = c("D","E","F"),nrow=3,ncol=1,align = "v",legend="bottom")
p = annotate_figure(p, top = text_grob("Right", 
                                      color = "black", face = "bold", size = 14))
p

p2 = ggarrange(L_IFG_PRESMA,L_PRESMA_STN,L_IFG_STN,
              common.legend = T,
              labels = c("A","B","C"),nrow=3,ncol=1, align = "v",legend="bottom")
p2 = annotate_figure(p2, top = text_grob("Left", 
                                       color = "black", face = "bold", size = 14))
p2
