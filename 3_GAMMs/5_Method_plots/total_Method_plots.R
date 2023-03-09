# Figures for STUDY 3 Manuscript


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
dir.create("./METHOD_PLOTS")
dir.create("./METHOD_PLOTS/Total_sample")
setwd("./METHOD_PLOTS")


################## TOTAL DATASET FIGURES #######################################

# Read behavioural and FBA subsampe data into R TOTAL SAMPLE
behavdat  <- read_csv("../EXG3_GAMMS/Total_sample/exg3_total_longform.csv")
neurodat <- read_csv("../FBA_GAMMS/Total_sample/FBAsample_total_final.csv")



# set NAs
behavdat <- behavdat %>% mutate_all(na_if,"NA")
neurodat <- neurodat %>% mutate_all(na_if,"NA")


# Check for missingness
sapply(behavdat, function(x) sum(is.na(x)))
sapply(neurodat, function(x) sum(is.na(x)))


# Format data - ensure all categorical vars (inc "SID) are factors; all continuous vars are numeric
behavdat <- mutate_at(behavdat, c(1:5,11), as.factor)
neurodat <- mutate_at(neurodat, c(2:6,12,32,34), as.factor)


#################################################################################

# SAMPLE SPREAD OF SCORES

################################################################################

# order age variable 

# behav data

bplot <- behavdat %>% mutate(age = round(as.numeric(age),2)) %>% arrange(age) %>%
  mutate(SID = factor(SID, unique(SID)))

# neuro data

nplot <- neurodat %>% mutate(age = round(as.numeric(age),2)) %>% arrange(age) %>%
  mutate(SID = factor(SID, unique(SID)))


# plot spread of data

# behav data

plot1 <- ggplot(bplot, aes(y=SID, x=age, group=SID, colour=sex))+
geom_line(size=.6,alpha=0.2) +
ylab("Participants") +  #Specify titles for y-axis...
xlab("Age") +           #x-axis...
geom_point(size=2, aes(shape=timepoint,alpha=0.2)) +
theme_bw() +
theme(axis.line = element_line(colour = "black"),
axis.text.y = element_blank(),
axis.ticks.y = element_blank(),
legend.position="none",
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank()) + theme(legend.position="right") + facet_wrap(~group) + ggtitle('Behavioural sample')  +
  theme(plot.title = element_text(hjust = 0.5))  


# neuro data

plot2 <- ggplot(nplot, aes(y=SID, x=age, group=SID, colour=sex))+
  geom_line(size=.6,alpha=0.2) +
  ylab("Participants") +  #Specify titles for y-axis...
  xlab("Age") +           #x-axis...
  geom_point(size=2, aes(shape=timepoint,alpha=0.2)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + theme(legend.position="right") + facet_wrap(~group) + ggtitle('Neuroimaging sample')  +
  theme(plot.title = element_text(hjust = 0.5))


# combine plots to one figure 
fig1 = ggarrange(plot1,plot2, labels = c("A", "B"),common.legend = TRUE, legend = "bottom")
fig1

# save
ggsave("./Total_sample/Study3_spread.png", fig1, width = 8,height=6)
dev.off()
