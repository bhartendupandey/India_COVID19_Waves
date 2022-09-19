rm(list=ls())
library(ggplot2)
library(sf)
library(corrplot)
library(Hmisc)
setwd("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/Intermediate/AllRDS")

WorldPop=TRUE

covidnum = readRDS("COVID_Num.rds")
cdemog = readRDS("Census_Demog.rds")
wdemog = readRDS("GHSL_Threshold10_WorldPop.rds")


	covidnum = readRDS("COVID_Num.rds")
	cdemog = as.data.frame(readRDS("GHSL_Threshold10_WorldPop.rds"))
	combdat = merge(covidnum,cdemog,by=c("STATE_UT","NAME","C_CODE11"),all.x=T)
	covidnum = merge(covidnum,cdemog,by=c("STATE_UT","NAME"),all.x=T)
	combdat$CumProp_1 = combdat$CumSum_1 * 1000000 / combdat$EstTotPopulation_WorldPop
	combdat$CumProp_2 = combdat$CumSum_2 * 1000000 / combdat$EstTotPopulation_WorldPop
	combdat$CumRate_1 = combdat$CumProp_1 /combdat$Length_1
	combdat$CumRate_2 = combdat$CumProp_2 / combdat$Length_2
	combdat$SR_CumRate = combdat$CumRate_2/combdat$CumRate_1
	R0 = readRDS("R0dist.rds")
	colnames(R0)[3:4] = c("R0_1","R0_2")
	combdat = merge(combdat,R0,by=c("STATE_UT","NAME"),all.x=T)
	Rt = readRDS("Rt_dist.rds")
	colnames(Rt)[3:4] = c("Rt_1","Rt_2")
	combdat = merge(combdat,Rt,by=c("STATE_UT","NAME"),all.x=T)
	combdat$Urbanization = combdat$EstUrbanization_WorldPop *100 / combdat$EstTotPopulation_WorldPop
	avgwealth = as.data.frame(readRDS("DHSAvgWealth.rds"))
	avgwealth = subset(avgwealth,select=-c(geometry))
	combdat = merge(combdat,avgwealth ,by=c("STATE_UT","NAME"),all.x=T)
	combdat$PopDen = combdat$EstTotPopulation_WorldPop/(as.numeric(st_area(combdat))/1000000)
	avg_age = as.data.frame(readRDS("District_Avg_Age.rds"))
	avg_age= subset(avg_age,select=-c(geometry))
	combdat = merge(combdat,avg_age,by=c("STATE_UT","NAME","C_CODE11"),all.x=T)
	tt2work = readRDS("TT2Work.rds")
	tt2workr = data.frame()
	for(i in 1:dim(tt2work)[1]){
		dat = tt2work[i,]
		notravel = dat[,"D_NT"]*100/dat[,"TotPop"]
		disttravel = sum((0.5 * dat[,"D_0_1"]/dat[,"TotPop"]),
				 (3.5 * dat[,"D_2_5"]/dat[,"TotPop"]),
				 (8 * dat[,"D_6_10"]/dat[,"TotPop"]),
				 (15.5 * dat[,"D_11_20"]/dat[,"TotPop"]),
				 (25.5 * dat[,"D_21_30"]/dat[,"TotPop"]),
				 (40.5 * dat[,"D_31_50"]/dat[,"TotPop"]),
				 (60 * dat[,"D_gt50"]/dat[,"TotPop"]))
		OnFoot = dat[,"Onfoot__D__Persons"]*100/dat[,"TotPop"]
		Bicycle = dat[,"Bicycle__D__Persons"]*100/dat[,"TotPop"]
		Motorcycle = dat[,"Moped/Scooter/MotorCycle__D__Persons"]*100/dat[,"TotPop"]
		Car = dat[,"Car/Jeep/Van__D__Persons"]*100/dat[,"TotPop"]
		Taxi = dat[,"Tempo/Autorickshaw/Taxi__D__Persons"]*100/dat[,"TotPop"]
		Bus = dat[,"Bus__D__Persons"]*100/dat[,"TotPop"]
		Train = dat[,"Train__D__Persons"]*100/dat[,"TotPop"]
		Water = dat[,"Watertransport__D__Persons"]*100/dat[,"TotPop"]
		Other = dat[,"Anyother__D__Persons"]*100/dat[,"TotPop"]
		outdf = data.frame(STATE_UT = dat$STATE_UT,NAME = dat$NAME,notravel,disttravel,OnFoot,Bicycle,Motorcycle,
			  Car,Taxi,Bus,Train,Water,Other)
		tt2workr = rbind(tt2workr,outdf)
	}
	combdat = merge(combdat,tt2workr,by=c("STATE_UT","NAME"),all.x=T)
	fb = readRDS("FB_March_2020_July_2021_indegree.rds")
	combdat = merge(combdat,fb,by=c("STATE_UT","NAME"),all.x=T)

### Correlation Matrix For WorldPop Dataset.

vars = c("STATE_UT","NAME","CumProp_1","CumProp_2","CumRate_1","CumRate_2","SR_CumRate",
#	   "R0_1","R0_2","Rt_1","Rt_2",
	   "Urbanization","AvgWealth","PopDen","Age",
	   "notravel","disttravel","OnFoot","Bicycle","Motorcycle","Car","Taxi","Bus","Train","Water",
	   "Other","n_crisis_1w","n_crisis_2w")

cordat1 = as.data.frame(combdat[,vars])
cordat1 = subset(cordat1,select=-c(geometry))

cordat1 = cordat1[complete.cases(cordat1),]

cordat2 = cordat1[,-c(1:2)]


cordat2.1 = as.matrix(cordat2)
colnames(cordat2.1) = c("1st Wave (Prop.)","2nd Wave (Prop.)","1st Wave (Rate)","2nd Wave (Rate)","Severity Ratio",
	#			"1st Wave (R0)","2nd Wave (R0)","1st Wave (Rt)","2nd Wave (Rt)",
				"Urbanization","Wealth", "Population Dens.","Average Age",
				"No Travel","Average Distance",
                        "On foot","Bicycle","Motorcycle","Car","Taxi","Bus","Train","Water Transport","Other",
				"1st Wave (In Degree)","2nd Wave (In Degree)")
cormat = rcorr(cordat2.1,type="spearman")
pdf("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Fig3_correlationmatrix_withoutR0_Rt.pdf")
corrplot(cormat$r,p.mat=cormat$P, method="color",outline=T,insig="pch",sig.level=0.01,pch.col="grey")
dev.off()









