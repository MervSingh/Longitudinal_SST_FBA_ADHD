library(tidyverse)
library(readxl)


t1 = read_xlsx('T1.xlsx')
t2 = read_xlsx('T2.xlsx')
t3 = read_xlsx('T3.xlsx')

# SST FAILS
tmp = t1 %>% filter(t1_Lowtrials==1 | t1_NoError==1 | t1_HighStopRR==1 | t1_Slow==1 | t1_IfBad==1)
tmp = tmp %>% mutate_at(c(4:8),as.numeric)
tmp$t1_HowBad <- rowSums(tmp[,4:8])
write_csv(tmp, "T1_SSTfails.csv")

tmp2 = t2 %>% filter(t2_Lowtrials==1 | t2_NoError==1 | t2_HighStopRR==1 | t2_Slow==1 | t2_IfBad==1)
tmp2 = tmp2 %>% mutate_at(c(4:8),as.numeric)
tmp2$t2_HowBad <- rowSums(tmp2[,4:8])
write_csv(tmp2, "T2_SSTfails.csv")

tmp3 = t3 %>% filter(t3_Lowtrials==1 | t3_NoError==1 | t3_HighStopRR==1 | t3_Slow==1 | t3_IfBad==1)
tmp3 = tmp3 %>% mutate_at(c(4:8),as.numeric)
tmp3$t3_HowBad <- rowSums(tmp3[,4:8])
write_csv(tmp3, "T3_SSTfails.csv")



# SST PASSES
t1 = read_xlsx('T1.xlsx')
tmp_pass = t1 %>% filter(t1_Lowtrials==0 & t1_NoError==0 & t1_HighStopRR==0 & t1_Slow==0 & t1_IfBad==0)
write_csv(tmp_pass, "T1_SSTpasses.csv")

t2 = read_xlsx('T2.xlsx')
tmp_pass2 = t2 %>% filter(t2_Lowtrials==0 & t2_NoError==0 & t2_HighStopRR==0 & t2_Slow==0 & t2_IfBad==0)
write_csv(tmp_pass2, "T2_SSTpasses.csv")

t3 = read_xlsx('T3.xlsx')
tmp_pass3 = t3 %>% filter(t3_Lowtrials==0 & t3_NoError==0 & t3_HighStopRR==0 & t3_Slow==0 & t3_IfBad==0)
write_csv(tmp_pass3, "T3_SSTpasses.csv")

save.image('EXGQC_pass_fails_totalsample.RData')
