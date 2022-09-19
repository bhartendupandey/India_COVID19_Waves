rm(list=ls())
library(ggplot2)
library(sf)
library(corrplot)
setwd("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/Intermediate/AllRDS")

source("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Script_for_paper_v2/GetAllData.R")

regdat = getalldata()

covidnum = readRDS("COVID_Num.rds")
covidnum = subset(as.data.frame(covidnum),select=-c(geom))

data = merge(regdat,covidnum,by=c("STATE_UT","NAME"),all.x=T)

plot(log(data$n_crisis_1w),log(data$CumSum_1))
plot(log(data$n_crisis_2w),log(data$CumSum_2))

plot(log(data$n_crisis_1w),log(data$CumProp_1))
plot(log(data$n_crisis_2w),log(data$CumProp_2))

plot(log(data$n_crisis_1w),log(data$CumRate_1))
plot(log(data$n_crisis_2w),log(data$CumRate_2))


outdf = data.frame()

v1 = "Cumulative Incidence(1st Wave)"
r1 = round(cor.test(data$n_crisis_1w,data$CumSum_1,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #Sig at 0.01 level

v1 = "Cumulative Incidence(2nd Wave)"
r1 = round(cor.test(data$n_crisis_2w,data$CumSum_2,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1))  #Sig at 0.01 level

v1 = "Cumulative Proportion(1st Wave)"
r1 = round(cor.test(data$n_crisis_1w,data$CumProp_1,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1))  #Sig at 0.01 level

v1 = "Cumulative Proportion(2nd Wave)"
r1 = round(cor.test(data$n_crisis_2w,data$CumProp_2,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #Sig at 0.01 level

v1 = "Cumulative Rate(1st Wave)"
r1 = round(cor.test(data$n_crisis_1w,data$CumRate_1,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #InSig at 0.01 level

v1 = "Cumulative Rate(2nd Wave)"
r1 = round(cor.test(data$n_crisis_2w,data$CumRate_2,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #InSig at 0.01 level

outdf1 = data.frame()
v1 = "Difference in Cumulative Incidence(2nd Wave - 1st Wave)"
r1 = round(cor.test(data$n_crisis_2w - data$n_crisis_1w,data$CumSum_2 - data$CumSum_1,use="complete.obs",method="spearman")$estimate,2)
outdf1 = rbind(outdf1,cbind(v1,r1)) # Sig at 0.01 level

v1 = "Difference in Cumulative Proportion(2nd Wave - 1st Wave)"
r1 = round(cor.test(data$n_crisis_2w - data$n_crisis_1w,data$CumProp_2 - data$CumProp_1,use="complete.obs",method="spearman")$estimate,2)
outdf1 = rbind(outdf1,cbind(v1,r1)) # InSig at 0.01 level

v1 = "Difference in Cumulative Rate(2nd Wave - 1st Wave)"
r1 = round(cor.test(data$n_crisis_2w - data$n_crisis_1w,data$CumRate_2 - data$CumRate_1,use="complete.obs",method="spearman")$estimate,2)
outdf1 = rbind(outdf1,cbind(v1,r1)) # InSig at 0.01 level


write.csv(outdf,"D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/SocialMediaCheck_1.csv")
write.csv(outdf1,"D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/SocialMediaCheck_2.csv")

v1 = "Cumulative Proportion(1st Wave)"
r1 = round(cor.test(data$n_crisis_1w/data$Length_1,data$CumProp_1,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1))  #Sig at 0.01 level

v1 = "Cumulative Proportion(2nd Wave)"
r1 = round(cor.test(data$n_crisis_2w/data$Length_2,data$CumProp_2,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #Sig at 0.01 level

v1 = "Cumulative Rate(1st Wave)"
r1 = round(cor.test(data$n_crisis_1w,data$CumRate_1,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #InSig at 0.01 level

v1 = "Cumulative Rate(2nd Wave)"
r1 = round(cor.test(data$n_crisis_2w,data$CumRate_2,use="complete.obs",method="spearman")$estimate,2)
outdf = rbind(outdf,cbind(v1,r1)) #InSig at 0.01 level



