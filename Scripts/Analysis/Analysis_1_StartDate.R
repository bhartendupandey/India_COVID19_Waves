rm(list=ls())
library(sf)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(boot)
set.seed(1234)
setwd("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/Intermediate/AllRDS")
files=list.files(pattern="*.rds")
shp1 = readRDS("COVID_wmet.rds")

s1 = do.call(rbind.data.frame,strsplit(as.character(shp1$Confirmed_StartDate_1),"-"))
colnames(s1) = c("Year","Month","Day")
shp1$S1 = month.abb[as.numeric(as.character(s1$Month))]
shp1$S1 = factor(shp1$S1,levels=c("Apr","May","Jun","Jul","Aug","No Data"))
shp1$S1[is.na(shp1$S1)] = "No Data"
summary(shp1$S1)
(243/636)

# The starting date for the first wave dominated in April (243) and May (343), with June in 47 districts. August(2) and Jul (4)
# We do not have metrics data for 2 districts

s1plot = ggplot() + geom_sf(data = shp1, aes(fill = S1),color=NA) + 
scale_fill_manual(values = c(viridis::viridis(5)[1:5],"gray"),name="First-wave (Starting Month)") + 
xlab("Longitude") + 
ylab("Latitude") +
theme_bw() + labs(title="Year: 2020")+ 
theme(plot.title = element_text(size=20))


#StartDate #2
# Select Timing for districts where number of peaks are 2
shp1.1 = as.data.frame(shp1)
shp1.1 = subset(shp1.1,shp1.1$Confirmed_NPeaks==2)
s2.1 = do.call(rbind.data.frame,strsplit(as.character(shp1.1$Confirmed_StartDate_2),"-"))
colnames(s2.1) = c("Year","Month","Day")
s2 = cbind(shp1.1[,c("STATE_UT","NAME")],s2.1)

#Select Timing for districts where number of peaks are 3
shp1.2 = as.data.frame(shp1)
shp1.2 = subset(shp1.2,shp1.2$Confirmed_NPeaks==3)
s2.2 = do.call(rbind.data.frame,strsplit(as.character(shp1.2$Confirmed_StartDate_3),"-"))
colnames(s2.2) = c("Year","Month","Day")
s2_ = cbind(shp1.2[,c("STATE_UT","NAME")],s2.2)

s2 = rbind(s2,s2_)
s2$S2 = month.abb[as.numeric(as.character(s2$Month))]
s2$S2 = factor(s2$S2,levels=c("Jan","Feb","Mar","Apr","May","No Data"))
s2$S2[is.na(s2$S2)] = "No Data"
summary(s2$S2)

s2 = s2[,c("STATE_UT","NAME","S2")]

shp1 = merge(shp1,s2,by=c("STATE_UT","NAME"),all.x=T)
shp1$S2[is.na(shp1$S2)] = "No Data"
# The starting date for the second wave dominated in Mar (241) and Apr (355), with Feb in 27 districts. May(8) and Jan (2)
# We do not have metrics data for 3 districts

s2plot = ggplot() + geom_sf(data = shp1, aes(fill = S2),color=NA) + 
scale_fill_manual(values = c(viridis::viridis(5)[1:5],"gray"),name="Second-wave (Starting Month)") + 
xlab("Longitude") + 
ylab("Latitude") +
theme_bw()  +labs(title="Year: 2021")+ 
theme(plot.title = element_text(size=20))

outplt = grid.arrange(s1plot,s2plot,ncol=2)

ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Fig2_wave_start.pdf",outplt,width=12.70,height=6.98 )

