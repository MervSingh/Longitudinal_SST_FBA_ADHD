
#############################################################

# ENVIRONMENT SET-UP

#############################################################

setwd("~/Desktop/final_GAMMS")
parent_dir = setwd("~/Desktop/final_GAMMS")

# Load packages
packages <- c("tidyverse","outliers","ggplot2","parallel","data.table","nlme","mgcv","plyr","dplyr","broom","SemiPar","itsadug","skimr", "fitdistrplus")
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
dir.create("/GAMMS_COMBINED")
dir.create("./GAMMS_COMBINED/Total_sample")
setwd("./GAMMS_COMBINED")


#############################################################

# DATA SET-UP

#############################################################

# Load FBA CONTROL subsample
total_fba <- read.csv("../FBA_GAMMS/Total_sample/FBAsample_total_final.csv", header = T)

# Format data - ensure all categorical vars (inc "SID) are factors; all continuous vars are numeric
total_fba <- mutate_at(total_fba, vars('SID', 'timepoint', 'sex', 'hand', 'med', 'group','scanner','b2800'), as.factor)


# mean centre covariates
total_fba <- total_fba %>%
  mutate(age_c = age - mean(age)) %>% 
  relocate(age_c, .after = age) %>%
  mutate(SES_c = SES - mean(SES)) %>% 
  relocate(SES_c, .after = SES) %>%
  mutate_at(vars('etiv.long'),as.numeric) %>%
  mutate(etiv.long_c = etiv.long - mean(etiv.long)) %>% 
  relocate(etiv.long_c , .after = etiv.long) %>%
  mutate_at(vars('meanFWD'),as.numeric) %>%
  mutate(meanFWD_c = meanFWD - mean(meanFWD)) %>% 
  relocate(meanFWD_c , .after = meanFWD)



# ordered factoring of scanner type
total_fba$OFscanner <- as.factor(total_fba$scanner)
# change factor to ordered factor:
total_fba$OFscanner <- as.ordered(total_fba$OFscanner)
# change contrast to treatment coding (difference curves)
contrasts(total_fba$OFscanner) <- 'contr.treatment'
# Inspect contrasts:
contrasts(total_fba$OFscanner)


# ordered factoring of sex
total_fba$OFsex <- as.factor(total_fba$sex)
# change factor to ordered factor:
total_fba$OFsex <- as.ordered(total_fba$OFsex)
# change contrast to treatment coding (difference curves)
contrasts(total_fba$OFsex) <- 'contr.treatment'
# Inspect contrasts:
contrasts(total_fba$OFsex)


# ordered factoring of med
total_fba$OFmed <- as.factor(total_fba$med)
# change factor to ordered factor:
total_fba$OFmed <- as.ordered(total_fba$OFmed)
# change contrast to treatment coding (difference curves)
contrasts(total_fba$OFmed) <- 'contr.treatment'
# Inspect contrasts:
contrasts(total_fba$OFmed)


# ordered factoring of group
total_fba$OFgroup <- as.factor(total_fba$group)
# change factor to ordered factor:
total_fba$OFgroup <- as.ordered(total_fba$OFgroup)
# change contrast to treatment coding (difference curves)
contrasts(total_fba$OFgroup) <- 'contr.treatment'
# Inspect contrasts:
contrasts(total_fba$OFgroup)


# set clean dataframe and remove uneeded vars
demo = total_fba[c(2:4,6:8,10:13,15,20:22,24,26,30:31,34,37,41,43,50:52,56:59)]

# model change in conners (with lme)
# extract slopes for FBA metrics - these will be the predictors in our models (adjusting for age, meanFWD, etiv.long)

ctrl <- lmeControl(opt='optim',maxIter=100,msMaxIter = 100)


#1
slopemod1 = lme(LOGFC_LH_IFG_PRESMA~age_c+meanFWD_c+etiv.long_c,control=ctrl,random=~1+age_c|SID,data=demo,method="ML")
#extract and save random intercepts and slopes (estimate change based on predicted scores)
randeffs <- as.data.frame(ranef(slopemod1)) %>% rownames_to_column() %>% dplyr::rename(SID = rowname, LOGFC_LH_IFG_PRESMA_intercept = `(Intercept)`, LOGFC_LH_IFG_PRESMA_slope = age_c)
demo <- demo %>% left_join(., randeffs) %>% mutate(LOGFC_LH_IFG_PRESMA_slope_bin = as.factor(ifelse(LOGFC_LH_IFG_PRESMA_slope < 0,0,1)))

#2
slopemod2 = lme(LOGFC_LH_PRESMA_STN~age_c+meanFWD_c+etiv.long_c,random=~1+age_c|SID,control=ctrl,data=demo,method="ML")
#extract and save random intercepts and slopes (estimate change based on predicted scores)
randeffs2 <- as.data.frame(ranef(slopemod2)) %>% rownames_to_column() %>% dplyr::rename(SID = rowname, LOGFC_LH_PRESMA_STN_intercept = `(Intercept)`, LOGFC_LH_PRESMA_STN_slope = age_c)
demo <- demo %>% left_join(., randeffs2) %>% mutate(LOGFC_LH_PRESMA_STN_slope_bin = as.factor(ifelse(LOGFC_LH_PRESMA_STN_slope < 0,0,1)))

#3
slopemod3 = lme(LOGFC_RH_PRESMA_STN~age_c+meanFWD_c+etiv.long_c,random=~1+age_c|SID,control=ctrl,data=demo,method="ML")
#extract and save random intercepts and slopes (estimate change based on predicted scores)
randeffs3 <- as.data.frame(ranef(slopemod3)) %>% rownames_to_column() %>% dplyr::rename(SID = rowname, LOGFC_RH_PRESMA_STN_intercept = `(Intercept)`, LOGFC_RH_PRESMA_STN_slope = age_c)
demo <- demo %>% left_join(., randeffs3) %>% mutate(LOGFC_RH_PRESMA_STN_slope_bin = as.factor(ifelse(LOGFC_RH_PRESMA_STN_slope < 0,0,1)))

#4
slopemod4 = lme(LOGFC_RH_IFG_PRESMA~age_c+meanFWD_c+etiv.long_c,random=~1+age_c|SID,control=ctrl,data=demo,method="ML")
#extract and save random intercepts and slopes (estimate change based on predicted scores)
randeffs4 <- as.data.frame(ranef(slopemod4)) %>% rownames_to_column() %>% dplyr::rename(SID = rowname, LOGFC_RH_IFG_PRESMA_intercept = `(Intercept)`, LOGFC_RH_IFG_PRESMA_slope = age_c)
demo <- demo %>% left_join(., randeffs4) %>% mutate(LOGFC_RH_IFG_PRESMA_slope_bin = as.factor(ifelse(LOGFC_RH_IFG_PRESMA_slope < 0,0,1)))

#5
slopemod5 = lme(LOGFC_RH_IFG_STN~age_c+meanFWD_c+etiv.long_c,random=~1+age_c|SID,control=ctrl,data=demo,method="ML")
#extract and save random intercepts and slopes (estimate change based on predicted scores)
randeffs5 <- as.data.frame(ranef(slopemod5)) %>% rownames_to_column() %>% dplyr::rename(SID = rowname, LOGFC_RH_IFG_STN_intercept = `(Intercept)`, LOGFC_RH_IFG_STN_slope = age_c)
demo <- demo %>% left_join(., randeffs5) %>% mutate(LOGFC_RH_IFG_STN_slope_bin = as.factor(ifelse(LOGFC_RH_IFG_STN_slope < 0,0,1)))


# Prep data for looping

demo2 = demo[-c(21:25,30,32:33,35:36,38:39,41:42,44)]

dvs = colnames(demo2[c(10:15,16:18)])
dvsgo = colnames(demo2[c(10,16)])
dvsother = colnames(demo2[c(11:15,17:18)])

demo2_xlong <- demo2 %>% gather(dvs,value,-SID,-timepoint,-sex,-med,-age,-age_c,-SES,-SES_c,-group,-etiv.long_c,-meanFWD_c,-OFsex,
                                -OFmed,-OFscanner,-OFgroup,-LOGFC_LH_IFG_PRESMA_slope,-LOGFC_LH_PRESMA_STN_slope,-LOGFC_RH_IFG_PRESMA_slope,
                                -LOGFC_RH_PRESMA_STN_slope,-LOGFC_RH_IFG_STN_slope) %>% mutate_at(vars('SID','dvs'),as.factor)


# filter only GoRT & mu.true vars
target <- c("GoRT", "mu.true")
filterdat = demo2_xlong %>%
  filter(demo2_xlong$dvs %in% target)

setwd("~/Desktop/final_GAMMS/GAMMS_COMBINED/Total_sample")

# LEFT HEMISPHERE

# LH LOGFC IFG-PRESMA
dir.create("./LOGFC_LH_IFG_PRESMA")


# GAM models looking at change in whether change in FBA metrics predict change in SST performance (GoRT & mu.true)

#loop through dvs

modelsA<-lapply(X=as.character(as.list(dvsgo)),
                df=filterdat,
                FUN=function(dv_name, df) {
                  print(dv_name)
                  
                  # now create a new dataframe within the loop that is filtered to dv_name
                  adf<-df %>% filter(dvs==dv_name) %>%
                    mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                           age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                           OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                           OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                           meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                           LOGFC_LH_IFG_PRESMA_slope = as.numeric(LOGFC_LH_IFG_PRESMA_slope),
                           value = as.numeric(value))
                  
                  # ordered factoring of group
                  adf$OFgroup <- as.factor(adf$OFgroup)
                  # change factor to ordered factor:
                  adf$OFgroup <- as.ordered(adf$OFgroup)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFgroup) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFgroup)
                  
                  
                  # ordered factoring of sex
                  adf$OFsex <- as.factor(adf$OFsex)
                  # change factor to ordered factor:
                  adf$OFsex <- as.ordered(adf$OFsex)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFsex) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFsex)
                  
                  
                  # ordered factoring of med
                  adf$OFmed <- as.factor(adf$OFmed)
                  # change factor to ordered factor:
                  adf$OFmed <- as.ordered(adf$OFmed)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFmed) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFmed)
                  
                  # ordered factoring of scanner
                  adf$OFscanner <- as.factor(adf$OFscanner)
                  # change factor to ordered factor:
                  adf$OFscanner <- as.ordered(adf$OFscanner)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFscanner) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFscanner)
                  
                  # Run the following gam models for each DV
                  
                  # Initial best-fitting model
                  agemod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  # Main effect of slope model
                  mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_IFG_PRESMA_slope + SES_c + OFgroup + OFmed + OFsex +
                                     OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  #Model with intraction of slope and age
                  interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_IFG_PRESMA_slope + ti(age_c, LOGFC_LH_IFG_PRESMA_slope, bs="cr", k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  save(agemod,mainslope,interslope, file = paste0("./LOGFC_LH_IFG_PRESMA/", dv_name,"_gams_models.Rdata"))
                  
                  # Model comparisons
                  
                  # 1 vs 2
                  # age model vs. main model
                  comp1 <- compareML(agemod,mainslope)
                  table1 <- comp1$table
                  pvalue1 <- as.numeric(comp1$table[2,6])
                  table1$AIC <- comp1$AIC
                  table1$advice <- comp1$advice
                  
                  # 2 vs 3
                  # main model vs. interaction model
                  comp2 <- compareML(mainslope,interslope)
                  table2 <- comp2$table
                  pvalue2 <- as.numeric(comp2$table[2,6])
                  table2$AIC <- comp2$AIC
                  table2$advice <- comp2$advice
                  
                  #Saving output
                  
                  # save age model
                  ptable <- as.data.frame(summary(agemod)$p.table) %>% rownames_to_column()
                  stable <- as.data.frame(summary(agemod)$s.table) %>% rownames_to_column()
                  
                  # save main model
                  ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                  stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                  
                  # save main interaction model
                  ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                  stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                  
                  
                  #combine all tables
                  table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                  table$dv <- dv_name
                  table
                  
                })

models_LOGFC_LH_IFG_PRESMA_GoRTmu.true <- rbindlist(modelsA,fill=TRUE)
View(models_LOGFC_LH_IFG_PRESMA_GoRTmu.true)
write.csv(models_LOGFC_LH_IFG_PRESMA_GoRTmu.true,file= "./LOGFC_LH_IFG_PRESMA/models_LOGFC_LH_IFG_PRESMA_GoRTmu.true.csv",row.names=F)


# LH LOGFC IFG-PRESMA

# GAM models looking at change in whether change in FBA metrics predict change in SST performance (all other vars)

#loop through covariates

modelsA1<-lapply(X=as.character(as.list(dvsother)),
                 df=demo2_xlong,
                 FUN=function(dv_name, df) {
                   print(dv_name)
                   
                   # now create a new dataframe within the loop that is filtered to dv_name
                   adf<-df %>% filter(dvs==dv_name) %>%
                     mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                            age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                            OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                            OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                            meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                            LOGFC_LH_IFG_PRESMA_slope = as.numeric(LOGFC_LH_IFG_PRESMA_slope),
                            value = as.numeric(value))
                   
                   # ordered factoring of group
                   adf$OFgroup <- as.factor(adf$OFgroup)
                   # change factor to ordered factor:
                   adf$OFgroup <- as.ordered(adf$OFgroup)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFgroup) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFgroup)
                   
                   
                   # ordered factoring of sex
                   adf$OFsex <- as.factor(adf$OFsex)
                   # change factor to ordered factor:
                   adf$OFsex <- as.ordered(adf$OFsex)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFsex) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFsex)
                   
                   
                   # ordered factoring of med
                   adf$OFmed <- as.factor(adf$OFmed)
                   # change factor to ordered factor:
                   adf$OFmed <- as.ordered(adf$OFmed)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFmed) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFmed)
                   
                   
                   # ordered factoring of scanner
                   adf$OFscanner <- as.factor(adf$OFscanner)
                   # change factor to ordered factor:
                   adf$OFscanner <- as.ordered(adf$OFscanner)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFscanner) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFscanner)
                   
                   # Run the following gam models for each DV
                   
                   # Initial best-fitting model
                   groupmod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                   
                   # Main effect of slope model
                   mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_IFG_PRESMA_slope + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                    data=adf, method = "ML")
                   
                   interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_IFG_PRESMA_slope + ti(age_c, LOGFC_LH_IFG_PRESMA_slope, bs="cr", k=4) +
                                       SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                     data=adf, method = "ML")
                   
                   save(groupmod,mainslope,interslope, file = paste0("./LOGFC_LH_IFG_PRESMA/", dv_name,"_gams_models.Rdata"))
                   
                   # Model comparisons
                   
                   # 1 vs 2
                   # age model vs. main model
                   comp1 <- compareML(groupmod,mainslope)
                   table1 <- comp1$table
                   pvalue1 <- as.numeric(comp1$table[2,6])
                   table1$AIC <- comp1$AIC
                   table1$advice <- comp1$advice
                   
                   # 2 vs 3
                   # main model vs. interaction model
                   comp2 <- compareML(mainslope,interslope)
                   table2 <- comp2$table
                   pvalue2 <- as.numeric(comp2$table[2,6])
                   table2$AIC <- comp2$AIC
                   table2$advice <- comp2$advice
                   
                   #Saving output
                   
                   # save age model
                   ptable <- as.data.frame(summary(groupmod)$p.table) %>% rownames_to_column()
                   stable <- as.data.frame(summary(groupmod)$s.table) %>% rownames_to_column()
                   
                   # save main model
                   ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                   stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                   
                   # save main interaction model
                   ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                   stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                   
                   
                   #combine all tables
                   table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                   table$dv <- dv_name
                   table
                   
                 })

models_LOGFC_LH_IFG_PRESMA_allothers <- rbindlist(modelsA1,fill=TRUE)
View(models_LOGFC_LH_IFG_PRESMA_allothers)
write.csv(models_LOGFC_LH_IFG_PRESMA_allothers,file= "./LOGFC_LH_IFG_PRESMA/models_LOGFC_LH_IFG_PRESMA_allothers.csv",row.names=F)




#LH LOGFC PRESMA-STN

dir.create("./LOGFC_LH_PRESMA_STN")


# GAM models looking at change in whether change in FBA metrics predict change in SST performance (GoRT & mu.true)

#loop through covariates

modelsB<-lapply(X=as.character(as.list(dvsgo)),
                df=filterdat,
                FUN=function(dv_name, df) {
                  print(dv_name)
                  
                  # now create a new dataframe within the loop that is filtered to dv_name
                  adf<-df %>% filter(dvs==dv_name) %>%
                    mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                           age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                           OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                           OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                           meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                           LOGFC_LH_PRESMA_STN_slope = as.numeric(LOGFC_LH_PRESMA_STN_slope),
                           value = as.numeric(value))
                  
                  # ordered factoring of group
                  adf$OFgroup <- as.factor(adf$OFgroup)
                  # change factor to ordered factor:
                  adf$OFgroup <- as.ordered(adf$OFgroup)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFgroup) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFgroup)
                  
                  
                  # ordered factoring of sex
                  adf$OFsex <- as.factor(adf$OFsex)
                  # change factor to ordered factor:
                  adf$OFsex <- as.ordered(adf$OFsex)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFsex) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFsex)
                  
                  
                  # ordered factoring of med
                  adf$OFmed <- as.factor(adf$OFmed)
                  # change factor to ordered factor:
                  adf$OFmed <- as.ordered(adf$OFmed)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFmed) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFmed)
                  
                  # ordered factoring of scanner
                  adf$OFscanner <- as.factor(adf$OFscanner)
                  # change factor to ordered factor:
                  adf$OFscanner <- as.ordered(adf$OFscanner)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFscanner) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFscanner)
                  
                  # Run the following gam models for each DV
                  
                  # Initial best-fitting model
                  agemod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  # Main effect of slope model
                  mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_PRESMA_STN_slope + SES_c + OFgroup + OFmed + OFsex +
                                     OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  #Model with intraction of slope and age
                  interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_PRESMA_STN_slope + ti(age_c, LOGFC_LH_PRESMA_STN_slope, bs="cr", k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  save(agemod,mainslope,interslope, file = paste0("./LOGFC_LH_PRESMA_STN/", dv_name,"_gams_models.Rdata"))
                  
                  # Model comparisons
                  
                  # 1 vs 2
                  # age model vs. main model
                  comp1 <- compareML(agemod,mainslope)
                  table1 <- comp1$table
                  pvalue1 <- as.numeric(comp1$table[2,6])
                  table1$AIC <- comp1$AIC
                  table1$advice <- comp1$advice
                  
                  # 2 vs 3
                  # main model vs. interaction model
                  comp2 <- compareML(mainslope,interslope)
                  table2 <- comp2$table
                  pvalue2 <- as.numeric(comp2$table[2,6])
                  table2$AIC <- comp2$AIC
                  table2$advice <- comp2$advice
                  
                  #Saving output
                  
                  # save age model
                  ptable <- as.data.frame(summary(agemod)$p.table) %>% rownames_to_column()
                  stable <- as.data.frame(summary(agemod)$s.table) %>% rownames_to_column()
                  
                  # save main model
                  ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                  stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                  
                  # save main interaction model
                  ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                  stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                  
                  
                  #combine all tables
                  table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                  table$dv <- dv_name
                  table
                  
                })

models_LOGFC_LH_PRESMA_STN_GoRTmu.true <- rbindlist(modelsB,fill=TRUE)
View(models_LOGFC_LH_PRESMA_STN_GoRTmu.true)
write.csv(models_LOGFC_LH_PRESMA_STN_GoRTmu.true,file= "./LOGFC_LH_PRESMA_STN/models_LOGFC_LH_PRESMA_STN_GoRTmu.true.csv",row.names=F)


# LH LOGFC PRESMA-STN

# GAM models looking at change in whether change in FBA metrics predict change in SST performance (all other vars)

#loop through covariates

modelsB1<-lapply(X=as.character(as.list(dvsother)),
                 df=demo2_xlong,
                 FUN=function(dv_name, df) {
                   print(dv_name)
                   
                   # now create a new dataframe within the loop that is filtered to dv_name
                   adf<-df %>% filter(dvs==dv_name) %>%
                     mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                            age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                            OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                            OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                            meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                            LOGFC_LH_PRESMA_STN_slope = as.numeric(LOGFC_LH_PRESMA_STN_slope),
                            value = as.numeric(value))
                   
                   # ordered factoring of group
                   adf$OFgroup <- as.factor(adf$OFgroup)
                   # change factor to ordered factor:
                   adf$OFgroup <- as.ordered(adf$OFgroup)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFgroup) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFgroup)
                   
                   
                   # ordered factoring of sex
                   adf$OFsex <- as.factor(adf$OFsex)
                   # change factor to ordered factor:
                   adf$OFsex <- as.ordered(adf$OFsex)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFsex) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFsex)
                   
                   
                   # ordered factoring of med
                   adf$OFmed <- as.factor(adf$OFmed)
                   # change factor to ordered factor:
                   adf$OFmed <- as.ordered(adf$OFmed)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFmed) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFmed)
                   
                   
                   # ordered factoring of scanner
                   adf$OFscanner <- as.factor(adf$OFscanner)
                   # change factor to ordered factor:
                   adf$OFscanner <- as.ordered(adf$OFscanner)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFscanner) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFscanner)
                   
                   # Run the following gam models for each DV
                   
                   # Initial best-fitting model
                   groupmod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                   
                   # Main effect of slope model
                   mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_PRESMA_STN_slope + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                    data=adf, method = "ML")
                   
                   interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_LH_PRESMA_STN_slope + ti(age_c, LOGFC_LH_PRESMA_STN_slope, bs="cr", k=4) +
                                       SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                     data=adf, method = "ML")
                   
                   save(groupmod,mainslope,interslope, file = paste0("./LOGFC_LH_PRESMA_STN/", dv_name,"_gams_models.Rdata"))
                   
                   # Model comparisons
                   
                   # 1 vs 2
                   # age model vs. main model
                   comp1 <- compareML(groupmod,mainslope)
                   table1 <- comp1$table
                   pvalue1 <- as.numeric(comp1$table[2,6])
                   table1$AIC <- comp1$AIC
                   table1$advice <- comp1$advice
                   
                   # 2 vs 3
                   # main model vs. interaction model
                   comp2 <- compareML(mainslope,interslope)
                   table2 <- comp2$table
                   pvalue2 <- as.numeric(comp2$table[2,6])
                   table2$AIC <- comp2$AIC
                   table2$advice <- comp2$advice
                   
                   #Saving output
                   
                   # save age model
                   ptable <- as.data.frame(summary(groupmod)$p.table) %>% rownames_to_column()
                   stable <- as.data.frame(summary(groupmod)$s.table) %>% rownames_to_column()
                   
                   # save main model
                   ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                   stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                   
                   # save main interaction model
                   ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                   stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                   
                   
                   #combine all tables
                   table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                   table$dv <- dv_name
                   table
                   
                 })

models_LOGFC_LH_PRESMA_STN_allothers <- rbindlist(modelsB1,fill=TRUE)
View(models_LOGFC_LH_PRESMA_STN_allothers)
write.csv(models_LOGFC_LH_PRESMA_STN_allothers,file= "./LOGFC_LH_PRESMA_STN/models_LOGFC_LH_PRESMA_STN_allothers.csv",row.names=F)




# RIGHT HEMISPHERE

# RH LOGFC IFG-PRESMA

dir.create("./LOGFC_RH_IFG_PRESMA")


# GAM models looking at change in whether change in FBA metrics predict change in SST performance (GoRT & mu.true)

#loop through covariates

modelsC<-lapply(X=as.character(as.list(dvsgo)),
                df=filterdat,
                FUN=function(dv_name, df) {
                  print(dv_name)
                  
                  # now create a new dataframe within the loop that is filtered to dv_name
                  adf<-df %>% filter(dvs==dv_name) %>%
                    mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                           age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                           OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                           OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                           meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                           LOGFC_RH_IFG_PRESMA_slope = as.numeric(LOGFC_RH_IFG_PRESMA_slope),
                           value = as.numeric(value))
                  
                  # ordered factoring of group
                  adf$OFgroup <- as.factor(adf$OFgroup)
                  # change factor to ordered factor:
                  adf$OFgroup <- as.ordered(adf$OFgroup)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFgroup) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFgroup)
                  
                  
                  # ordered factoring of sex
                  adf$OFsex <- as.factor(adf$OFsex)
                  # change factor to ordered factor:
                  adf$OFsex <- as.ordered(adf$OFsex)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFsex) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFsex)
                  
                  
                  # ordered factoring of med
                  adf$OFmed <- as.factor(adf$OFmed)
                  # change factor to ordered factor:
                  adf$OFmed <- as.ordered(adf$OFmed)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFmed) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFmed)
                  
                  # ordered factoring of scanner
                  adf$OFscanner <- as.factor(adf$OFscanner)
                  # change factor to ordered factor:
                  adf$OFscanner <- as.ordered(adf$OFscanner)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFscanner) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFscanner)
                  
                  # Run the following gam models for each DV
                  
                  # Initial best-fitting model
                  agemod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  # Main effect of slope model
                  mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_PRESMA_slope + SES_c + OFgroup + OFmed + OFsex +
                                     OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  #Model with intraction of slope and age
                  interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_PRESMA_slope + ti(age_c, LOGFC_RH_IFG_PRESMA_slope, bs="cr", k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  save(agemod,mainslope,interslope, file = paste0("./LOGFC_RH_IFG_PRESMA/", dv_name,"_gams_models.Rdata"))
                  
                  # Model comparisons
                  
                  # 1 vs 2
                  # age model vs. main model
                  comp1 <- compareML(agemod,mainslope)
                  table1 <- comp1$table
                  pvalue1 <- as.numeric(comp1$table[2,6])
                  table1$AIC <- comp1$AIC
                  table1$advice <- comp1$advice
                  
                  # 2 vs 3
                  # main model vs. interaction model
                  comp2 <- compareML(mainslope,interslope)
                  table2 <- comp2$table
                  pvalue2 <- as.numeric(comp2$table[2,6])
                  table2$AIC <- comp2$AIC
                  table2$advice <- comp2$advice
                  
                  #Saving output
                  
                  # save age model
                  ptable <- as.data.frame(summary(agemod)$p.table) %>% rownames_to_column()
                  stable <- as.data.frame(summary(agemod)$s.table) %>% rownames_to_column()
                  
                  # save main model
                  ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                  stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                  
                  # save main interaction model
                  ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                  stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                  
                  
                  #combine all tables
                  table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                  table$dv <- dv_name
                  table
                  
                })

models_LOGFC_RH_IFG_PRESMA_GoRTmu.true <- rbindlist(modelsC,fill=TRUE)
View(models_LOGFC_RH_IFG_PRESMA_GoRTmu.true)
write.csv(models_LOGFC_RH_IFG_PRESMA_GoRTmu.true,file= "./LOGFC_RH_IFG_PRESMA/models_LOGFC_RH_IFG_PRESMA_GoRTmu.true.csv",row.names=F)


# RH LOGFC IFG-PRESMA

# GAM models looking at change in whether change in FBA metrics predict change in SST performance (all other vars)

#loop through covariates

modelsC1<-lapply(X=as.character(as.list(dvsother)),
                 df=demo2_xlong,
                 FUN=function(dv_name, df) {
                   print(dv_name)
                   
                   # now create a new dataframe within the loop that is filtered to dv_name
                   adf<-df %>% filter(dvs==dv_name) %>%
                     mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                            age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                            OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                            OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                            meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                            LOGFC_RH_IFG_PRESMA_slope = as.numeric(LOGFC_RH_IFG_PRESMA_slope),
                            value = as.numeric(value))
                   
                   # ordered factoring of group
                   adf$OFgroup <- as.factor(adf$OFgroup)
                   # change factor to ordered factor:
                   adf$OFgroup <- as.ordered(adf$OFgroup)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFgroup) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFgroup)
                   
                   
                   # ordered factoring of sex
                   adf$OFsex <- as.factor(adf$OFsex)
                   # change factor to ordered factor:
                   adf$OFsex <- as.ordered(adf$OFsex)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFsex) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFsex)
                   
                   
                   # ordered factoring of med
                   adf$OFmed <- as.factor(adf$OFmed)
                   # change factor to ordered factor:
                   adf$OFmed <- as.ordered(adf$OFmed)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFmed) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFmed)
                   
                   
                   # ordered factoring of scanner
                   adf$OFscanner <- as.factor(adf$OFscanner)
                   # change factor to ordered factor:
                   adf$OFscanner <- as.ordered(adf$OFscanner)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFscanner) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFscanner)
                   
                   # Run the following gam models for each DV
                   
                   # Initial best-fitting model
                   groupmod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                   
                   # Main effect of slope model
                   mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_PRESMA_slope + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                    data=adf, method = "ML")
                   
                   interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_PRESMA_slope + ti(age_c, LOGFC_RH_IFG_PRESMA_slope, bs="cr", k=4) +
                                       SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                     data=adf, method = "ML")
                   
                   save(groupmod,mainslope,interslope, file = paste0("./LOGFC_RH_IFG_PRESMA/", dv_name,"_gams_models.Rdata"))
                   
                   # Model comparisons
                   
                   # 1 vs 2
                   # age model vs. main model
                   comp1 <- compareML(groupmod,mainslope)
                   table1 <- comp1$table
                   pvalue1 <- as.numeric(comp1$table[2,6])
                   table1$AIC <- comp1$AIC
                   table1$advice <- comp1$advice
                   
                   # 2 vs 3
                   # main model vs. interaction model
                   comp2 <- compareML(mainslope,interslope)
                   table2 <- comp2$table
                   pvalue2 <- as.numeric(comp2$table[2,6])
                   table2$AIC <- comp2$AIC
                   table2$advice <- comp2$advice
                   
                   #Saving output
                   
                   # save age model
                   ptable <- as.data.frame(summary(groupmod)$p.table) %>% rownames_to_column()
                   stable <- as.data.frame(summary(groupmod)$s.table) %>% rownames_to_column()
                   
                   # save main model
                   ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                   stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                   
                   # save main interaction model
                   ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                   stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                   
                   
                   #combine all tables
                   table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                   table$dv <- dv_name
                   table
                   
                 })

models_LOGFC_RH_IFG_PRESMA_allothers <- rbindlist(modelsC1,fill=TRUE)
View(models_LOGFC_RH_IFG_PRESMA_allothers)
write.csv(models_LOGFC_RH_IFG_PRESMA_allothers,file= "./LOGFC_RH_IFG_PRESMA/models_LOGFC_RH_IFG_PRESMA_allothers.csv",row.names=F)


#RH LOGFC PRESMA-STN

dir.create("./LOGFC_RH_PRESMA_STN")


# GAM models looking at change in whether change in FBA metrics predict change in SST performance (GoRT & mu.true)

#loop through covariates

modelsD<-lapply(X=as.character(as.list(dvsgo)),
                df=filterdat,
                FUN=function(dv_name, df) {
                  print(dv_name)
                  
                  # now create a new dataframe within the loop that is filtered to dv_name
                  adf<-df %>% filter(dvs==dv_name) %>%
                    mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                           age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                           OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                           OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                           meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                           LOGFC_RH_PRESMA_STN_slope = as.numeric(LOGFC_RH_PRESMA_STN_slope),
                           value = as.numeric(value))
                  
                  # ordered factoring of group
                  adf$OFgroup <- as.factor(adf$OFgroup)
                  # change factor to ordered factor:
                  adf$OFgroup <- as.ordered(adf$OFgroup)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFgroup) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFgroup)
                  
                  
                  # ordered factoring of sex
                  adf$OFsex <- as.factor(adf$OFsex)
                  # change factor to ordered factor:
                  adf$OFsex <- as.ordered(adf$OFsex)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFsex) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFsex)
                  
                  
                  # ordered factoring of med
                  adf$OFmed <- as.factor(adf$OFmed)
                  # change factor to ordered factor:
                  adf$OFmed <- as.ordered(adf$OFmed)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFmed) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFmed)
                  
                  # ordered factoring of scanner
                  adf$OFscanner <- as.factor(adf$OFscanner)
                  # change factor to ordered factor:
                  adf$OFscanner <- as.ordered(adf$OFscanner)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFscanner) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFscanner)
                  
                  # Run the following gam models for each DV
                  
                  # Initial best-fitting model
                  agemod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  # Main effect of slope model
                  mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_PRESMA_STN_slope + SES_c + OFgroup + OFmed + OFsex +
                                     OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  #Model with intraction of slope and age
                  interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_PRESMA_STN_slope + ti(age_c, LOGFC_RH_PRESMA_STN_slope, bs="cr", k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  save(agemod,mainslope,interslope, file = paste0("./LOGFC_RH_PRESMA_STN/", dv_name,"_gams_models.Rdata"))
                  
                  # Model comparisons
                  
                  # 1 vs 2
                  # age model vs. main model
                  comp1 <- compareML(agemod,mainslope)
                  table1 <- comp1$table
                  pvalue1 <- as.numeric(comp1$table[2,6])
                  table1$AIC <- comp1$AIC
                  table1$advice <- comp1$advice
                  
                  # 2 vs 3
                  # main model vs. interaction model
                  comp2 <- compareML(mainslope,interslope)
                  table2 <- comp2$table
                  pvalue2 <- as.numeric(comp2$table[2,6])
                  table2$AIC <- comp2$AIC
                  table2$advice <- comp2$advice
                  
                  #Saving output
                  
                  # save age model
                  ptable <- as.data.frame(summary(agemod)$p.table) %>% rownames_to_column()
                  stable <- as.data.frame(summary(agemod)$s.table) %>% rownames_to_column()
                  
                  # save main model
                  ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                  stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                  
                  # save main interaction model
                  ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                  stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                  
                  
                  #combine all tables
                  table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                  table$dv <- dv_name
                  table
                  
                })

models_LOGFC_RH_PRESMA_STN_GoRTmu.true <- rbindlist(modelsD,fill=TRUE)
View(models_LOGFC_RH_PRESMA_STN_GoRTmu.true)
write.csv(models_LOGFC_RH_PRESMA_STN_GoRTmu.true,file= "./LOGFC_RH_PRESMA_STN/models_LOGFC_RH_PRESMA_STN_GoRTmu.true.csv",row.names=F)


# RH LOGFC PRESMA-STN

# GAM models looking at change in whether change in FBA metrics predict change in SST performance (all other vars)

#loop through covariates

modelsD1<-lapply(X=as.character(as.list(dvsother)),
                 df=demo2_xlong,
                 FUN=function(dv_name, df) {
                   print(dv_name)
                   
                   # now create a new dataframe within the loop that is filtered to dv_name
                   adf<-df %>% filter(dvs==dv_name) %>%
                     mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                            age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                            OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                            OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                            meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                            LOGFC_RH_PRESMA_STN_slope = as.numeric(LOGFC_RH_PRESMA_STN_slope),
                            value = as.numeric(value))
                   
                   # ordered factoring of group
                   adf$OFgroup <- as.factor(adf$OFgroup)
                   # change factor to ordered factor:
                   adf$OFgroup <- as.ordered(adf$OFgroup)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFgroup) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFgroup)
                   
                   
                   # ordered factoring of sex
                   adf$OFsex <- as.factor(adf$OFsex)
                   # change factor to ordered factor:
                   adf$OFsex <- as.ordered(adf$OFsex)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFsex) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFsex)
                   
                   
                   # ordered factoring of med
                   adf$OFmed <- as.factor(adf$OFmed)
                   # change factor to ordered factor:
                   adf$OFmed <- as.ordered(adf$OFmed)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFmed) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFmed)
                   
                   
                   # ordered factoring of scanner
                   adf$OFscanner <- as.factor(adf$OFscanner)
                   # change factor to ordered factor:
                   adf$OFscanner <- as.ordered(adf$OFscanner)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFscanner) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFscanner)
                   
                   # Run the following gam models for each DV
                   
                   # Initial best-fitting model
                   groupmod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                   
                   # Main effect of slope model
                   mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_PRESMA_STN_slope + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                    data=adf, method = "ML")
                   
                   interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_PRESMA_STN_slope + ti(age_c, LOGFC_RH_PRESMA_STN_slope, bs="cr", k=4) +
                                       SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                     data=adf, method = "ML")
                   
                   save(groupmod,mainslope,interslope, file = paste0("./LOGFC_RH_PRESMA_STN/", dv_name,"_gams_models.Rdata"))
                   
                   # Model comparisons
                   
                   # 1 vs 2
                   # age model vs. main model
                   comp1 <- compareML(groupmod,mainslope)
                   table1 <- comp1$table
                   pvalue1 <- as.numeric(comp1$table[2,6])
                   table1$AIC <- comp1$AIC
                   table1$advice <- comp1$advice
                   
                   # 2 vs 3
                   # main model vs. interaction model
                   comp2 <- compareML(mainslope,interslope)
                   table2 <- comp2$table
                   pvalue2 <- as.numeric(comp2$table[2,6])
                   table2$AIC <- comp2$AIC
                   table2$advice <- comp2$advice
                   
                   #Saving output
                   
                   # save age model
                   ptable <- as.data.frame(summary(groupmod)$p.table) %>% rownames_to_column()
                   stable <- as.data.frame(summary(groupmod)$s.table) %>% rownames_to_column()
                   
                   # save main model
                   ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                   stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                   
                   # save main interaction model
                   ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                   stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                   
                   
                   #combine all tables
                   table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                   table$dv <- dv_name
                   table
                   
                 })

models_LOGFC_RH_PRESMA_STN_allothers <- rbindlist(modelsD1,fill=TRUE)
View(models_LOGFC_RH_PRESMA_STN_allothers)
write.csv(models_LOGFC_RH_PRESMA_STN_allothers,file= "./LOGFC_RH_PRESMA_STN/models_LOGFC_RH_PRESMA_STN_allothers.csv",row.names=F)


#RH LOGFC IFG-STN

dir.create("./LOGFC_RH_IFG_STN")


# GAM models looking at change in whether change in FBA metrics predict change in SST performance (GoRT & mu.true)

#loop through covariates

modelsE<-lapply(X=as.character(as.list(dvsgo)),
                df=filterdat,
                FUN=function(dv_name, df) {
                  print(dv_name)
                  
                  # now create a new dataframe within the loop that is filtered to dv_name
                  adf<-df %>% filter(dvs==dv_name) %>%
                    mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                           age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                           OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                           OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                           meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                           LOGFC_RH_IFG_STN_slope = as.numeric(LOGFC_RH_IFG_STN_slope),
                           value = as.numeric(value))
                  
                  # ordered factoring of group
                  adf$OFgroup <- as.factor(adf$OFgroup)
                  # change factor to ordered factor:
                  adf$OFgroup <- as.ordered(adf$OFgroup)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFgroup) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFgroup)
                  
                  
                  # ordered factoring of sex
                  adf$OFsex <- as.factor(adf$OFsex)
                  # change factor to ordered factor:
                  adf$OFsex <- as.ordered(adf$OFsex)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFsex) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFsex)
                  
                  
                  # ordered factoring of med
                  adf$OFmed <- as.factor(adf$OFmed)
                  # change factor to ordered factor:
                  adf$OFmed <- as.ordered(adf$OFmed)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFmed) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFmed)
                  
                  # ordered factoring of scanner
                  adf$OFscanner <- as.factor(adf$OFscanner)
                  # change factor to ordered factor:
                  adf$OFscanner <- as.ordered(adf$OFscanner)
                  # change contrast to treatment coding (difference curves)
                  contrasts(adf$OFscanner) <- 'contr.treatment'
                  # Inspect contrasts:
                  contrasts(adf$OFscanner)
                  
                  # Run the following gam models for each DV
                  
                  # Initial best-fitting model
                  agemod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  # Main effect of slope model
                  mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_STN_slope + SES_c + OFgroup + OFmed + OFsex +
                                     OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  #Model with intraction of slope and age
                  interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_STN_slope + ti(age_c, LOGFC_RH_IFG_STN_slope, bs="cr", k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                  
                  save(agemod,mainslope,interslope, file = paste0("./LOGFC_RH_IFG_STN/", dv_name,"_gams_models.Rdata"))
                  
                  # Model comparisons
                  
                  # 1 vs 2
                  # age model vs. main model
                  comp1 <- compareML(agemod,mainslope)
                  table1 <- comp1$table
                  pvalue1 <- as.numeric(comp1$table[2,6])
                  table1$AIC <- comp1$AIC
                  table1$advice <- comp1$advice
                  
                  # 2 vs 3
                  # main model vs. interaction model
                  comp2 <- compareML(mainslope,interslope)
                  table2 <- comp2$table
                  pvalue2 <- as.numeric(comp2$table[2,6])
                  table2$AIC <- comp2$AIC
                  table2$advice <- comp2$advice
                  
                  #Saving output
                  
                  # save age model
                  ptable <- as.data.frame(summary(agemod)$p.table) %>% rownames_to_column()
                  stable <- as.data.frame(summary(agemod)$s.table) %>% rownames_to_column()
                  
                  # save main model
                  ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                  stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                  
                  # save main interaction model
                  ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                  stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                  
                  
                  #combine all tables
                  table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                  table$dv <- dv_name
                  table
                  
                })

models_LOGFC_RH_IFG_STN_GoRTmu.true <- rbindlist(modelsE,fill=TRUE)
View(models_LOGFC_RH_IFG_STN_GoRTmu.true)
write.csv(models_LOGFC_RH_IFG_STN_GoRTmu.true,file= "./LOGFC_RH_IFG_STN/models_LOGFC_RH_IFG_STN_GoRTmu.true.csv",row.names=F)


# RH LOGFC IFG-STN

# GAM models looking at change in whether change in FBA metrics predict change in SST performance (all other vars)

#loop through covariates

modelsE1<-lapply(X=as.character(as.list(dvsother)),
                 df=demo2_xlong,
                 FUN=function(dv_name, df) {
                   print(dv_name)
                   
                   # now create a new dataframe within the loop that is filtered to dv_name
                   adf<-df %>% filter(dvs==dv_name) %>%
                     mutate(SID = as.factor(SID), timepoint = as.factor(timepoint),
                            age_c = as.numeric(age_c), OFsex = as.factor(OFsex),
                            OFmed = as.factor(OFmed), OFscanner = as.factor(OFscanner),
                            OFgroup = as.factor(OFgroup), SES_c = as.numeric(SES_c),
                            meanFWD_c = as.numeric(meanFWD_c), etiv.long_c = as.numeric(etiv.long_c),
                            LOGFC_RH_IFG_STN_slope = as.numeric(LOGFC_RH_IFG_STN_slope),
                            value = as.numeric(value))
                   
                   # ordered factoring of group
                   adf$OFgroup <- as.factor(adf$OFgroup)
                   # change factor to ordered factor:
                   adf$OFgroup <- as.ordered(adf$OFgroup)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFgroup) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFgroup)
                   
                   
                   # ordered factoring of sex
                   adf$OFsex <- as.factor(adf$OFsex)
                   # change factor to ordered factor:
                   adf$OFsex <- as.ordered(adf$OFsex)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFsex) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFsex)
                   
                   
                   # ordered factoring of med
                   adf$OFmed <- as.factor(adf$OFmed)
                   # change factor to ordered factor:
                   adf$OFmed <- as.ordered(adf$OFmed)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFmed) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFmed)
                   
                   
                   # ordered factoring of scanner
                   adf$OFscanner <- as.factor(adf$OFscanner)
                   # change factor to ordered factor:
                   adf$OFscanner <- as.ordered(adf$OFscanner)
                   # change contrast to treatment coding (difference curves)
                   contrasts(adf$OFscanner) <- 'contr.treatment'
                   # Inspect contrasts:
                   contrasts(adf$OFscanner)
                   
                   # Run the following gam models for each DV
                   
                   # Initial best-fitting model
                   groupmod <- gam(value  ~ s(age_c,bs="cr",k=4) + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'), data=adf, method = "ML")
                   
                   # Main effect of slope model
                   mainslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_STN_slope + SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                    data=adf, method = "ML")
                   
                   interslope <- gam(value  ~ s(age_c,bs="cr",k=4) + LOGFC_RH_IFG_STN_slope + ti(age_c, LOGFC_RH_IFG_STN_slope, bs="cr", k=4) +
                                       SES_c + OFgroup + OFmed + OFsex + OFscanner + s(SID,bs='re'),
                                     data=adf, method = "ML")
                   
                   save(groupmod,mainslope,interslope, file = paste0("./LOGFC_RH_IFG_STN/", dv_name,"_gams_models.Rdata"))
                   
                   # Model comparisons
                   
                   # 1 vs 2
                   # age model vs. main model
                   comp1 <- compareML(groupmod,mainslope)
                   table1 <- comp1$table
                   pvalue1 <- as.numeric(comp1$table[2,6])
                   table1$AIC <- comp1$AIC
                   table1$advice <- comp1$advice
                   
                   # 2 vs 3
                   # main model vs. interaction model
                   comp2 <- compareML(mainslope,interslope)
                   table2 <- comp2$table
                   pvalue2 <- as.numeric(comp2$table[2,6])
                   table2$AIC <- comp2$AIC
                   table2$advice <- comp2$advice
                   
                   #Saving output
                   
                   # save age model
                   ptable <- as.data.frame(summary(groupmod)$p.table) %>% rownames_to_column()
                   stable <- as.data.frame(summary(groupmod)$s.table) %>% rownames_to_column()
                   
                   # save main model
                   ptable2 <- as.data.frame(summary(mainslope)$p.table) %>% rownames_to_column()
                   stable2 <- as.data.frame(summary(mainslope)$s.table) %>% rownames_to_column()
                   
                   # save main interaction model
                   ptable3 <- as.data.frame(summary(interslope)$p.table) %>% rownames_to_column()
                   stable3 <- as.data.frame(summary(interslope)$s.table) %>% rownames_to_column()
                   
                   
                   #combine all tables
                   table <- plyr::rbind.fill(table1,table2,ptable,stable,ptable2,stable2,ptable3,stable3)
                   table$dv <- dv_name
                   table
                   
                 })

models_LOGFC_RH_IFG_STN_allothers <- rbindlist(modelsE1,fill=TRUE)
View(models_LOGFC_RH_IFG_STN_allothers)
write.csv(models_LOGFC_RH_IFG_STN_allothers,file= "./LOGFC_RH_IFG_STN/models_LOGFC_RH_IFG_STN_allothers.csv",row.names=F)

setwd(parent_dir)
save.image("COMBINED_GAMMS_total.RData")
