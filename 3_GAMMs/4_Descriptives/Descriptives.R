# Load packages
rpkgs <- c("reshape2", "Hmisc", "ggplot2", "skimr",
           "cowplot", "ggpubr", "mgcv", "broom",
           "dplyr", "plyr", "tidyr", "mice", "boot",
           "SemiPar", "readxl", "gridExtra", "grid", "lattice",'lme4','reshape2')

if (length(setdiff(rpkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(rpkgs, rownames(installed.packages())))
}
lapply(rpkgs, library, character.only = TRUE)

# group comparisons - data prep

#set working directory

wd = "~/Desktop/Merv/Study3-Desc/DESC"
setwd(wd)

rm(list=ls())

## BEHAVIOURAL SAMPLE
# Load the behavioural data
dat = read_csv('exg3_total_longform.csv')%>% mutate_at(vars('SID','timepoint','sex','hand','med','group'),as.factor)
conn= readxl::read_xlsx('CONNERS.xlsx')%>% mutate_at(vars('SID','timepoint'),as.factor)
dat = left_join(dat,conn)
write.csv(dat,'exg3_total_longform_conners.csv')

# how many timepoints did each subject complete?
dat_waves_per_SID = reshape2::dcast(dat, SID ~ timepoint, value.var = c('timepoint'))
# Convert to wide format
dat_wide = dcast(setDT(dat), SID ~ timepoint, value.var=c('sex','hand','med','age','IQ','SES','group','GoRT','mu.true','mu.false','sigma.true','sigma.false','tau.true','tau.false','SSRT', 'muS', 'sigmaS', 'tauS', 'prob_TF','prob_GF', 'CPADHDI'))
dat_merge = left_join(dat_wide,dat_waves_per_SID)
write.csv(dat_merge,'exg3_total_wide.csv')
# Summary stats by Wave x group: TOTAL DATASET
dat_desc <- dat %>% dplyr::group_by(timepoint) %>% dplyr::group_by(group, .add = T) %>% skimr::skim_without_charts()
write.csv(dat_desc,'exg3_total_desc.csv')

# Group comparisons
names(dat_wide)
vars1 = names(dat_wide)[c(11,17,62)]
vars2 = names(dat_wide)[c(12,18,63)]
vars3 = names(dat_wide)[c(13,19,64)]

ttests <- lapply(vars1, function(x) wilcox.test(reformulate("group_1", x), data = dat_wide))
ttests2 <- lapply(vars2, function(x) wilcox.test(reformulate("group_2", x), data = dat_wide))
ttests3 <- lapply(vars3, function(x) wilcox.test(reformulate("group_3", x), data = dat_wide))
names(ttests) <- c(vars1)
names(ttests2) <- c(vars2)
names(ttests3) <- c(vars3)



save.image('Study3_Behavioural_Desc.RData')
# -----------------------------------------------


## IMAGING SAMPLE
rm(list=ls())
# Load the fba subsample data
neuro = read_csv('FBAsample_total_final.csv') %>% mutate_at(vars('SID','timepoint','sex','hand','med','group','scanner','b2800'),as.factor) %>% mutate_at(vars('etiv.long'),as.numeric)
conn= readxl::read_xlsx('CONNERS.xlsx')%>% mutate_at(vars('SID','timepoint'),as.factor)
neuro = left_join(neuro,conn)
write.csv(neuro,'fba_total_final_conners.csv')

# how many timepoints did each subject complete?
neuro_waves_per_SID = reshape2::dcast(neuro, SID ~ timepoint, value.var = c('timepoint'))
# Convert to wide format
neuro_wide = dcast(setDT(neuro), SID ~ timepoint, value.var=c('sex','hand','med','age','IQ','SES','group','scanner','b2800',
                                                                'etiv.long','meanFWD','GoRT','mu.true','mu.false','sigma.true','sigma.false','tau.true','tau.false','SSRT', 'muS', 'sigmaS', 'tauS','prob_TF','prob_GF',
                                                                "LOGFC_LH_IFG_PRESMA", "LOGFC_LH_IFG_STN", "LOGFC_LH_PRESMA_STN",
                                                                "LOGFC_RH_IFG_PRESMA", "LOGFC_RH_IFG_STN", "LOGFC_RH_PRESMA_STN",'CPADHDI'))
neuro_merge = left_join(neuro_wide,neuro_waves_per_SID)
write.csv(neuro_merge,'fba_total_wide.csv')
# Summary stats by Wave x group: TOTAL DATASET
neuro_desc <- neuro %>% dplyr::group_by(timepoint) %>% dplyr::group_by(group, .add = T) %>% skimr::skim_without_charts()
write.csv(neuro_desc,'fba_total_desc.csv')

# Group comparisons
names(neuro_wide)
vars1 = names(neuro_wide)[c(11,17,29,32,92)]
vars2 = names(neuro_wide)[c(12,18,30,33,93)]
vars3 = names(neuro_wide)[c(13,19,31,34,94)]


ttests <- lapply(vars1, function(x) wilcox.test(reformulate("group_1", x), data = neuro_wide))
ttests2 <- lapply(vars2, function(x) wilcox.test(reformulate("group_2", x), data = neuro_wide))
ttests3 <- lapply(vars3, function(x) wilcox.test(reformulate("group_3", x), data = neuro_wide))
names(ttests) <- c(vars1)
names(ttests2) <- c(vars2)
names(ttests3) <- c(vars3)

save.image('Study3_Imaging_Desc.RData')

