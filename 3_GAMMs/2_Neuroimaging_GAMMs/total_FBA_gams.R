
# GAMS analysis for FBA sub-sample


#############################################################

# ENVIRONMENT SET-UP

#############################################################


setwd("~/Desktop/final_GAMMS")
parent_dir = setwd("~/Desktop/final_GAMMS")

# Load packages
packages <- c("readxl","ggpubr","Hmisc","tidyverse","outliers","ggplot2","parallel","data.table","nlme","mgcv","plyr","dplyr","broom","SemiPar","itsadug","skimr", "fitdistrplus", "mice")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

# set number of cores
# number of cores selelcted will be equal to the total number of cores on your machine
numcores <- detectCores()

# set select function from dplyr
select <- dplyr::select

# set directory paths for the input and output data
dir.create("./FBA_GAMMS")
dir.create("./FBA_GAMMS/Controls_only")
dir.create("./FBA_GAMMS/Total_sample")
setwd("./FBA_GAMMS")


#############################################################

# DATA SET-UP

#############################################################

# Identify subset for neuroimaging GAMS analysis

# load exg3 behav dataframe
dat <-read_csv("../EXG3_GAMMS/Total_sample/exg3_total_longform.csv")
# reformat factors
dat <- mutate_at(dat, vars('SID', 'timepoint', 'sex', 'hand', 'med', 'group'), as.factor)
# load dMRI dataframe
dwi <- read_csv("../data_setup/outfiles/nicap_dmri_long_v2.csv")
# reformat factors
dwi <- mutate_at(dwi, vars('SID', 'timepoint', 'b2800', 'group'), as.factor)
# merge with cleaned behav dataframe,remove dwi NAs & re calculate age_c
total <- dat %>% left_join(., dwi) %>% drop_na() %>% mutate(age_c = age - mean(age))
# Save in Total sample folder
write.csv(total, "../FBA_GAMMS/Total_sample/FBAsample_total.csv")


#############################################################

# FILTER FBA SUBSAMPLE BASED ON QC 

#############################################################

# Load in list of subjects that have failed QC
qc = readxl::read_xlsx("../data_setup/dwiQC/qc_fails.xlsx")
# reformat factors
qc <- mutate_at(qc, vars('SID','timepoint'), as.factor)
# remove all fails from total FBA subsample with anti_join  & re calculate age_c
total_clean <- anti_join(total,qc, by = c("SID", "timepoint")) %>% mutate(age_c = age - mean(age))
# save cleaned total fba subsample
write.csv(total_clean, "../FBA_GAMMS/Total_sample/FBAsample_total_QCed.csv")
write.table(total_clean[c(1,2)], file = "../FBA_GAMMS/Total_sample/subject.txt", row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE)

# SIDs that were removed
fails = anti_join(total,total_clean, by = c("SID", "timepoint"))
# save in dwQC folder
write.csv(fails, "../data_setup/dwiQC/dwi_fails.csv")

# Save 
save.image("FBA_subsamples.RData")



#############################################################

# FBA GAMMS

#############################################################


# DATA PREP


#load covariate data
covs = read_xlsx("./covs/FBAsample_total_covars.xlsx")

# reformat factors
covs <- mutate_at(covs, vars('SID','timepoint','scanner'), as.factor)
# merge
total_clean = total_clean %>% left_join(., covs)

# set NAs
total_clean <- total_clean %>% mutate_all(na_if,"NA")

# Check for missingness
sapply(total_clean, function(x) sum(is.na(x)))

# Create a subset of Data removing all obs with missing etiv
total_sub <- subset(total_clean, (!is.na(total_clean[["etiv.long"]])))
sapply(total_sub, function(x) sum(is.na(x)))

# Load FBA metrics for total sample
fba = read_xlsx("./fba_metrics/total_fba_measures.xlsx")
fba <- mutate_at(fba, vars('SID','timepoint'), as.factor)

# merge
total_sub = total_sub %>% left_join(., fba)



# STATS

# Summary stats by Wave x group: TOTAL DATA
total_summ <- total_sub %>% dplyr::group_by(timepoint) %>% dplyr::group_by(group, .add = T) %>% skimr::skim_without_charts()
# how many unique IDs completed each wave?
adhd_ids <- total_sub %>% filter(group=="ADHD") %>% group_by(SID) %>% dplyr::summarise(N = n()) %>% mutate(N = as.factor(N))
skim(adhd_ids)
write.csv(total_sub, "../FBA_GAMMS/Total_sample/FBAsample_total_final.csv")
write.csv(total_summ, "../FBA_GAMMS/Total_sample/TotalDescriptives.csv")

# remove uneeded vars
total_sub2 = total_sub[c(1:3,5:7,9:11,32:40,44:49)]


# mean centre age, SES, eTIV and meanFWD
total_sub2 <- total_sub2 %>% 
  mutate(age_c = age - mean(age)) %>% 
  relocate(age_c, .after = age) %>%
  mutate(SES_c = SES - mean(SES)) %>% 
  relocate(SES_c, .after = SES) %>%
  mutate_at(vars('etiv.long'),as.numeric) %>%
  mutate(etiv.long_c = etiv.long - mean(etiv.long)) %>% 
  relocate(etiv.long_c , .after = etiv.long) %>%
  mutate(meanFWD_c = meanFWD - mean(meanFWD)) %>% 
  relocate(meanFWD_c , .after = meanFWD)


## Set up for looping

# Create list of vars for lapply to loop different GAM models through
dvstotal = colnames(total_sub2[c(15:26)])


## Transform data to extra long format for lapply to loop over
total_sub2_xlong <- total_sub2 %>% gather(dvstotal, value, -SID, -timepoint, -sex, -med, -age, -age_c, -SES, -SES_c, -group, -meanFWD, -meanFWD_c, -scanner, -etiv.long, -etiv.long_c)

# write to csv
write.csv(total_sub2_xlong, "./Total_sample/total_final_xlong.csv")



#############################################################

# GAMS MODELLING

#############################################################


# # Run the GAMS modelling code for all DVs via sourcing the script
setwd(parent_dir)

# TOTAL
source("./scripts/total/FBA_GAMMS_total.R")
setwd(parent_dir)

# SAVE WORKSPACE
save.image("FBA_GAMMS.RData")
