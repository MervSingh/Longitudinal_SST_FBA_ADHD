rm(list=ls())
library(tidyverse)
library(ggpubr)
library(effectsize)
wd = "~/Desktop/Merv/Study3-Desc/ATTR_S3/ATTR_ADHD/SST_ADHD"
setwd(wd)
data <- read.csv("~/Desktop/Merv/Study3-Desc/ATTR_S3/fba_total_wide.csv")
master = data


# ATTRITION ANALYSIS

# CREATE NEW DX VARIABLE

data = data %>%
  mutate(DX = case_when(group_1 == 'ADHD' | group_2 == 'ADHD' | group_3 == 'ADHD' ~ 'ADHD',
                        group_1 == 'CONTROL' | group_2 == 'CONTROL' | group_3 == 'CONTROL' ~ 'CONTROL'))

# SST ATTRITION ANALYSIS - BEHAV SAMPLE

# filter to only ADHD sample

data = data %>% filter(DX == 'ADHD')


# people with wave 1 ONLY vs people with wave 1-2 ONLY/wave1-3 ONLY vs people with all 3 waves

# GoRT
print('GoRT_1')
data$GoRT_1Y_2Y_3Y <- !is.na(data$GoRT_1) & !is.na(data$GoRT_2)  & !is.na(data$GoRT_3) #people who have all 3 waves
NROW(which((data$GoRT_1Y_2Y==TRUE)))
data$GoRT_1Y_2Y_3N <- !is.na(data$GoRT_1) & !is.na(data$GoRT_2) & is.na(data$GoRT_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$GoRT_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$GoRT_1Y_2N_3Y <- !is.na(data$GoRT_1) & is.na(data$GoRT_2) & !is.na(data$GoRT_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$GoRT_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$GoRT_1Y_2N_3N <- !is.na(data$GoRT_1) &  is.na(data$GoRT_2) & is.na(data$GoRT_3) #people who have WAVE 1 ONLY
NROW(which((data$GoRT_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$GoRT_1Y_2Y_3Y==TRUE | data$GoRT_1Y_2Y_3N==TRUE | data$GoRT_1Y_2N_3N==TRUE | data$GoRT_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(GoRT_1Y_2Y_3Y==TRUE~"three",GoRT_1Y_2Y_3N==TRUE~"two",GoRT_1Y_2N_3Y==TRUE~"two",GoRT_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$GoRT_1 ~ df$obs, df)
summary(aov(df$GoRT_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "GoRT_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "GoRT", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('GoRT_fba.png',p1)
save(p1,file='GoRT_fba.RData')
eff=effectsize(m,partial=T)
eff



# mu.true
print('mu.true_1')
data$mu.true_1Y_2Y_3Y <- !is.na(data$mu.true_1) & !is.na(data$mu.true_2)  & !is.na(data$mu.true_3) #people who have all 3 waves
NROW(which((data$mu.true_1Y_2Y==TRUE)))
data$mu.true_1Y_2Y_3N <- !is.na(data$mu.true_1) & !is.na(data$mu.true_2) & is.na(data$mu.true_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$mu.true_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$mu.true_1Y_2N_3Y <- !is.na(data$mu.true_1) & is.na(data$mu.true_2) & !is.na(data$mu.true_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$mu.true_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$mu.true_1Y_2N_3N <- !is.na(data$mu.true_1) &  is.na(data$mu.true_2) & is.na(data$mu.true_3) #people who have WAVE 1 ONLY
NROW(which((data$mu.true_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$mu.true_1Y_2Y_3Y==TRUE | data$mu.true_1Y_2Y_3N==TRUE | data$mu.true_1Y_2N_3N==TRUE | data$mu.true_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(mu.true_1Y_2Y_3Y==TRUE~"three",mu.true_1Y_2Y_3N==TRUE~"two",mu.true_1Y_2N_3Y==TRUE~"two",mu.true_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$mu.true_1 ~ df$obs, df)
summary(aov(df$mu.true_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "mu.true_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "mu.true", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('mu.true_fba.png',p1)
save(p1,file='mu.true_fba.RData')
eff=effectsize(m,partial=T)
eff



# sigma.true
print('sigma.true_1')
data$sigma.true_1Y_2Y_3Y <- !is.na(data$sigma.true_1) & !is.na(data$sigma.true_2)  & !is.na(data$sigma.true_3) #people who have all 3 waves
NROW(which((data$sigma.true_1Y_2Y==TRUE)))
data$sigma.true_1Y_2Y_3N <- !is.na(data$sigma.true_1) & !is.na(data$sigma.true_2) & is.na(data$sigma.true_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$sigma.true_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$sigma.true_1Y_2N_3Y <- !is.na(data$sigma.true_1) & is.na(data$sigma.true_2) & !is.na(data$sigma.true_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$sigma.true_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$sigma.true_1Y_2N_3N <- !is.na(data$sigma.true_1) &  is.na(data$sigma.true_2) & is.na(data$sigma.true_3) #people who have WAVE 1 ONLY
NROW(which((data$sigma.true_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$sigma.true_1Y_2Y_3Y==TRUE | data$sigma.true_1Y_2Y_3N==TRUE | data$sigma.true_1Y_2N_3N==TRUE | data$sigma.true_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(sigma.true_1Y_2Y_3Y==TRUE~"three",sigma.true_1Y_2Y_3N==TRUE~"two",sigma.true_1Y_2N_3Y==TRUE~"two",sigma.true_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$sigma.true_1 ~ df$obs, df)
summary(aov(df$sigma.true_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "sigma.true_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "sigma.true", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('sigma.true_fba.png',p1)
save(p1,file='sigma.true_fba.RData')
eff=effectsize(m,partial=T)
eff



# tau.true
print('tau.true_1')
data$tau.true_1Y_2Y_3Y <- !is.na(data$tau.true_1) & !is.na(data$tau.true_2)  & !is.na(data$tau.true_3) #people who have all 3 waves
NROW(which((data$tau.true_1Y_2Y==TRUE)))
data$tau.true_1Y_2Y_3N <- !is.na(data$tau.true_1) & !is.na(data$tau.true_2) & is.na(data$tau.true_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$tau.true_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$tau.true_1Y_2N_3Y <- !is.na(data$tau.true_1) & is.na(data$tau.true_2) & !is.na(data$tau.true_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$tau.true_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$tau.true_1Y_2N_3N <- !is.na(data$tau.true_1) &  is.na(data$tau.true_2) & is.na(data$tau.true_3) #people who have WAVE 1 ONLY
NROW(which((data$tau.true_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$tau.true_1Y_2Y_3Y==TRUE | data$tau.true_1Y_2Y_3N==TRUE | data$tau.true_1Y_2N_3N==TRUE | data$tau.true_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(tau.true_1Y_2Y_3Y==TRUE~"three",tau.true_1Y_2Y_3N==TRUE~"two",tau.true_1Y_2N_3Y==TRUE~"two",tau.true_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$tau.true_1 ~ df$obs, df)
summary(aov(df$tau.true_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "tau.true_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "tau.true", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('tau.true_fba.png',p1)
save(p1,file='tau.true_fba.RData')
eff=effectsize(m,partial=T)
eff
TukeyHSD(m)
plot(TukeyHSD(m))

# mu.false
print('mu.false_1')
data$mu.false_1Y_2Y_3Y <- !is.na(data$mu.false_1) & !is.na(data$mu.false_2)  & !is.na(data$mu.false_3) #people who have all 3 waves
NROW(which((data$mu.false_1Y_2Y==TRUE)))
data$mu.false_1Y_2Y_3N <- !is.na(data$mu.false_1) & !is.na(data$mu.false_2) & is.na(data$mu.false_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$mu.false_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$mu.false_1Y_2N_3Y <- !is.na(data$mu.false_1) & is.na(data$mu.false_2) & !is.na(data$mu.false_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$mu.false_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$mu.false_1Y_2N_3N <- !is.na(data$mu.false_1) &  is.na(data$mu.false_2) & is.na(data$mu.false_3) #people who have WAVE 1 ONLY
NROW(which((data$mu.false_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$mu.false_1Y_2Y_3Y==TRUE | data$mu.false_1Y_2Y_3N==TRUE | data$mu.false_1Y_2N_3N==TRUE | data$mu.false_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(mu.false_1Y_2Y_3Y==TRUE~"three",mu.false_1Y_2Y_3N==TRUE~"two",mu.false_1Y_2N_3Y==TRUE~"two",mu.false_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$mu.false_1 ~ df$obs, df)
summary(aov(df$mu.false_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "mu.false_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "mu.false", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('mu.false_fba.png',p1)
save(p1,file='mu.false_fba.RData')
eff=effectsize(m,partial=T)
eff



# sigma.false
print('sigma.false_1')
data$sigma.false_1Y_2Y_3Y <- !is.na(data$sigma.false_1) & !is.na(data$sigma.false_2)  & !is.na(data$sigma.false_3) #people who have all 3 waves
NROW(which((data$sigma.false_1Y_2Y==TRUE)))
data$sigma.false_1Y_2Y_3N <- !is.na(data$sigma.false_1) & !is.na(data$sigma.false_2) & is.na(data$sigma.false_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$sigma.false_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$sigma.false_1Y_2N_3Y <- !is.na(data$sigma.false_1) & is.na(data$sigma.false_2) & !is.na(data$sigma.false_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$sigma.false_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$sigma.false_1Y_2N_3N <- !is.na(data$sigma.false_1) &  is.na(data$sigma.false_2) & is.na(data$sigma.false_3) #people who have WAVE 1 ONLY
NROW(which((data$sigma.false_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$sigma.false_1Y_2Y_3Y==TRUE | data$sigma.false_1Y_2Y_3N==TRUE | data$sigma.false_1Y_2N_3N==TRUE | data$sigma.false_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(sigma.false_1Y_2Y_3Y==TRUE~"three",sigma.false_1Y_2Y_3N==TRUE~"two",sigma.false_1Y_2N_3Y==TRUE~"two",sigma.false_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$sigma.false_1 ~ df$obs, df)
summary(aov(df$sigma.false_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "sigma.false_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "sigma.false", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('sigma.false_fba.png',p1)
save(p1,file='sigma.false_fba.RData')
eff=effectsize(m,partial=T)
eff



# tau.false
print('tau.false_1')
data$tau.false_1Y_2Y_3Y <- !is.na(data$tau.false_1) & !is.na(data$tau.false_2)  & !is.na(data$tau.false_3) #people who have all 3 waves
NROW(which((data$tau.false_1Y_2Y==TRUE)))
data$tau.false_1Y_2Y_3N <- !is.na(data$tau.false_1) & !is.na(data$tau.false_2) & is.na(data$tau.false_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$tau.false_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$tau.false_1Y_2N_3Y <- !is.na(data$tau.false_1) & is.na(data$tau.false_2) & !is.na(data$tau.false_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$tau.false_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$tau.false_1Y_2N_3N <- !is.na(data$tau.false_1) &  is.na(data$tau.false_2) & is.na(data$tau.false_3) #people who have WAVE 1 ONLY
NROW(which((data$tau.false_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$tau.false_1Y_2Y_3Y==TRUE | data$tau.false_1Y_2Y_3N==TRUE | data$tau.false_1Y_2N_3N==TRUE | data$tau.false_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(tau.false_1Y_2Y_3Y==TRUE~"three",tau.false_1Y_2Y_3N==TRUE~"two",tau.false_1Y_2N_3Y==TRUE~"two",tau.false_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$tau.false_1 ~ df$obs, df)
summary(aov(df$tau.false_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "tau.false_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "tau.false", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('tau.false_fba.png',p1)
save(p1,file='tau.false_fba.RData')
eff=effectsize(m,partial=T)
eff



# SSRT
print('SSRT_1')
data$SSRT_1Y_2Y_3Y <- !is.na(data$SSRT_1) & !is.na(data$SSRT_2)  & !is.na(data$SSRT_3) #people who have all 3 waves
NROW(which((data$SSRT_1Y_2Y==TRUE)))
data$SSRT_1Y_2Y_3N <- !is.na(data$SSRT_1) & !is.na(data$SSRT_2) & is.na(data$SSRT_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$SSRT_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$SSRT_1Y_2N_3Y <- !is.na(data$SSRT_1) & is.na(data$SSRT_2) & !is.na(data$SSRT_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$SSRT_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$SSRT_1Y_2N_3N <- !is.na(data$SSRT_1) &  is.na(data$SSRT_2) & is.na(data$SSRT_3) #people who have WAVE 1 ONLY
NROW(which((data$SSRT_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$SSRT_1Y_2Y_3Y==TRUE | data$SSRT_1Y_2Y_3N==TRUE | data$SSRT_1Y_2N_3N==TRUE | data$SSRT_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(SSRT_1Y_2Y_3Y==TRUE~"three",SSRT_1Y_2Y_3N==TRUE~"two",SSRT_1Y_2N_3Y==TRUE~"two",SSRT_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$SSRT_1 ~ df$obs, df)
summary(aov(df$SSRT_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "SSRT_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "SSRT", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('SSRT_fba.png',p1)
save(p1,file='SSRT_fba.RData')
eff=effectsize(m,partial=T)
eff




# muS
print('muS_1')
data$muS_1Y_2Y_3Y <- !is.na(data$muS_1) & !is.na(data$muS_2)  & !is.na(data$muS_3) #people who have all 3 waves
NROW(which((data$muS_1Y_2Y==TRUE)))
data$muS_1Y_2Y_3N <- !is.na(data$muS_1) & !is.na(data$muS_2) & is.na(data$muS_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$muS_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$muS_1Y_2N_3Y <- !is.na(data$muS_1) & is.na(data$muS_2) & !is.na(data$muS_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$muS_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$muS_1Y_2N_3N <- !is.na(data$muS_1) &  is.na(data$muS_2) & is.na(data$muS_3) #people who have WAVE 1 ONLY
NROW(which((data$muS_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$muS_1Y_2Y_3Y==TRUE | data$muS_1Y_2Y_3N==TRUE | data$muS_1Y_2N_3N==TRUE | data$muS_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(muS_1Y_2Y_3Y==TRUE~"three",muS_1Y_2Y_3N==TRUE~"two",muS_1Y_2N_3Y==TRUE~"two",muS_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$muS_1 ~ df$obs, df)
summary(aov(df$muS_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "muS_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "muS", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('muS_fba.png',p1)
save(p1,file='muS_fba.RData')
eff=effectsize(m,partial=T)
eff



# sigmaS
print('sigmaS_1')
data$sigmaS_1Y_2Y_3Y <- !is.na(data$sigmaS_1) & !is.na(data$sigmaS_2)  & !is.na(data$sigmaS_3) #people who have all 3 waves
NROW(which((data$sigmaS_1Y_2Y==TRUE)))
data$sigmaS_1Y_2Y_3N <- !is.na(data$sigmaS_1) & !is.na(data$sigmaS_2) & is.na(data$sigmaS_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$sigmaS_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$sigmaS_1Y_2N_3Y <- !is.na(data$sigmaS_1) & is.na(data$sigmaS_2) & !is.na(data$sigmaS_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$sigmaS_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$sigmaS_1Y_2N_3N <- !is.na(data$sigmaS_1) &  is.na(data$sigmaS_2) & is.na(data$sigmaS_3) #people who have WAVE 1 ONLY
NROW(which((data$sigmaS_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$sigmaS_1Y_2Y_3Y==TRUE | data$sigmaS_1Y_2Y_3N==TRUE | data$sigmaS_1Y_2N_3N==TRUE | data$sigmaS_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(sigmaS_1Y_2Y_3Y==TRUE~"three",sigmaS_1Y_2Y_3N==TRUE~"two",sigmaS_1Y_2N_3Y==TRUE~"two",sigmaS_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$sigmaS_1 ~ df$obs, df)
summary(aov(df$sigmaS_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "sigmaS_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "sigmaS", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('sigmaS_fba.png',p1)
save(p1,file='sigmaS_fba.RData')
eff=effectsize(m,partial=T)
eff



# tauS
print('tauS_1')
data$tauS_1Y_2Y_3Y <- !is.na(data$tauS_1) & !is.na(data$tauS_2)  & !is.na(data$tauS_3) #people who have all 3 waves
NROW(which((data$tauS_1Y_2Y==TRUE)))
data$tauS_1Y_2Y_3N <- !is.na(data$tauS_1) & !is.na(data$tauS_2) & is.na(data$tauS_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$tauS_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$tauS_1Y_2N_3Y <- !is.na(data$tauS_1) & is.na(data$tauS_2) & !is.na(data$tauS_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$tauS_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$tauS_1Y_2N_3N <- !is.na(data$tauS_1) &  is.na(data$tauS_2) & is.na(data$tauS_3) #people who have WAVE 1 ONLY
NROW(which((data$tauS_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$tauS_1Y_2Y_3Y==TRUE | data$tauS_1Y_2Y_3N==TRUE | data$tauS_1Y_2N_3N==TRUE | data$tauS_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(tauS_1Y_2Y_3Y==TRUE~"three",tauS_1Y_2Y_3N==TRUE~"two",tauS_1Y_2N_3Y==TRUE~"two",tauS_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$tauS_1 ~ df$obs, df)
summary(aov(df$tauS_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "tauS_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "tauS", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('tauS_fba.png',p1)
save(p1,file='tauS_fba.RData')
eff=effectsize(m,partial=T)
eff




# prob_GF
print('prob_GF_1')
data$prob_GF_1Y_2Y_3Y <- !is.na(data$prob_GF_1) & !is.na(data$prob_GF_2)  & !is.na(data$prob_GF_3) #people who have all 3 waves
NROW(which((data$prob_GF_1Y_2Y==TRUE)))
data$prob_GF_1Y_2Y_3N <- !is.na(data$prob_GF_1) & !is.na(data$prob_GF_2) & is.na(data$prob_GF_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$prob_GF_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$prob_GF_1Y_2N_3Y <- !is.na(data$prob_GF_1) & is.na(data$prob_GF_2) & !is.na(data$prob_GF_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$prob_GF_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$prob_GF_1Y_2N_3N <- !is.na(data$prob_GF_1) &  is.na(data$prob_GF_2) & is.na(data$prob_GF_3) #people who have WAVE 1 ONLY
NROW(which((data$prob_GF_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$prob_GF_1Y_2Y_3Y==TRUE | data$prob_GF_1Y_2Y_3N==TRUE | data$prob_GF_1Y_2N_3N==TRUE | data$prob_GF_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(prob_GF_1Y_2Y_3Y==TRUE~"three",prob_GF_1Y_2Y_3N==TRUE~"two",prob_GF_1Y_2N_3Y==TRUE~"two",prob_GF_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$prob_GF_1 ~ df$obs, df)
summary(aov(df$prob_GF_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "prob_GF_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "prob_GF", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('prob_GF_fba.png',p1)
save(p1,file='prob_GF_fba.RData')
eff=effectsize(m,partial=T)
eff



# prob_TF
print('prob_TF_1')
data$prob_TF_1Y_2Y_3Y <- !is.na(data$prob_TF_1) & !is.na(data$prob_TF_2)  & !is.na(data$prob_TF_3) #people who have all 3 waves
NROW(which((data$prob_TF_1Y_2Y==TRUE)))
data$prob_TF_1Y_2Y_3N <- !is.na(data$prob_TF_1) & !is.na(data$prob_TF_2) & is.na(data$prob_TF_3) #people who have both  WAVE 1 and 2 ONLY
NROW(which((data$prob_TF_1Y_2Y_3N==TRUE))) # print out number of people who have both 1 and 2
data$prob_TF_1Y_2N_3Y <- !is.na(data$prob_TF_1) & is.na(data$prob_TF_2) & !is.na(data$prob_TF_3) #people who have both  WAVE 1 and 3 ONLY
NROW(which((data$prob_TF_1Y_2N_3Y==TRUE))) # print out number of people who have both 1 and 3
data$prob_TF_1Y_2N_3N <- !is.na(data$prob_TF_1) &  is.na(data$prob_TF_2) & is.na(data$prob_TF_3) #people who have WAVE 1 ONLY
NROW(which((data$prob_TF_1Y_2N_3N==TRUE))) # print out number of people who have wave 1

df <- data[data$prob_TF_1Y_2Y_3Y==TRUE | data$prob_TF_1Y_2Y_3N==TRUE | data$prob_TF_1Y_2N_3N==TRUE | data$prob_TF_1Y_2N_3Y==TRUE,]
df = df %>% mutate(obs = case_when(prob_TF_1Y_2Y_3Y==TRUE~"three",prob_TF_1Y_2Y_3N==TRUE~"two",prob_TF_1Y_2N_3Y==TRUE~"two",prob_TF_1Y_2N_3N==TRUE~"one"))
df$obs = as.factor(df$obs)
levels(df$obs)
df$obs <- factor(df$obs, levels = c("one", "two", "three"))
# remaining subjects with only wave 2, wave 3, or wave 2-3 data
tmp = anti_join(data,df,by='SID')

#compare using  aov
m=aov(df$prob_TF_1 ~ df$obs, df)
summary(aov(df$prob_TF_1 ~ df$obs, df))
p1 = ggline(df, x = "obs", y = "prob_TF_1",
            add = c("mean_se", "jitter"),
            order = c("one", "two", "three"),
            ylab = "prob_TF", xlab = "no. of timepoints completed") + stat_compare_means(method = "anova")+ stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")
p1
ggsave('prob_TF_fba.png',p1)
save(p1,file='prob_TF_fba.RData')
eff=effectsize(m,partial=T)
eff

