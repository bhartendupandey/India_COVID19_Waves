# This Script Calculates Moran's I for the Incidence Rate Based Severity Ratio
# This Script Outputs Severity Ratio Map at the District Level
# This Script Outputs Severity Ration Boxplots by States


rm(list=ls())
library(sf)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(boot)
library(spdep)
library(spatialreg)
library(dplyr)
set.seed(1234)
setwd("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/Intermediate/AllRDS")
files=list.files(pattern="*.rds")

covidnum = readRDS("COVID_Num.rds")
cdemog = as.data.frame(readRDS("GHSL_Threshold10_WorldPop.rds"))
#cdemog = subset(cdemog,select=-c(geom))

pltdat = merge(covidnum,cdemog,by=c("STATE_UT","NAME"))
pltdat$CumProp_1 = pltdat$CumSum_1 * 1000000 / pltdat$EstTotPopulation_WorldPop
pltdat$CumProp_2 = pltdat$CumSum_2 * 1000000 / pltdat$EstTotPopulation_WorldPop
pltdat$CumRate_1 = pltdat$CumProp_1 /pltdat$Length_1
pltdat$CumRate_2 = pltdat$CumProp_2 / pltdat$Length_2
pltdat$SR_CumRate = pltdat$CumRate_2/pltdat$CumRate_1
#pltdat$SR_CumProp = pltdat$CumProp_2/pltdat$CumProp_1

nb = poly2nb(pltdat)
lw = nb2listw(nb,zero.policy=T)
moran.test(pltdat$SR_CumRate,lw,zero.policy=T,na.action=na.omit)

severityR = ggplot() + geom_sf(data= covidnum,color="gray") + geom_sf(data = pltdat, aes(fill = log(SR_CumRate)),color=NA) + 
scale_fill_viridis_c(name=expression(log *" "* Severity[ratio]),option="inferno") + 
xlab("Longitude") + 
ylab("Latitude") +
theme_bw()

ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS3_SeverityRatiomap.pdf",severityR)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS3_SeverityRatiomap.jpg",severityR)


boxplotSR = pltdat %>%
subset(STATE_UT != "POK") %>%
subset(STATE_UT != "Lakshadweep") %>%
  ggplot(aes(x=reorder(STATE_UT,SR_CumRate,na.rm = TRUE), y=SR_CumRate)) +
  geom_boxplot() +
  labs(y=expression(Severity[ratio]), x="State", 
       subtitle=expression(Severity[ratio] * " by State")) + theme_bw() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS4_SeverityRatio_State.pdf",boxplotSR)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS4_SeverityRatio_State.jpg",boxplotSR)


listsev = as.data.frame(pltdat[,c("NAME","STATE_UT","SR_CumRate")])
listsev = subset(listsev,listsev$SR_CumRate> mean(listsev$SR_CumRate,na.rm=T))