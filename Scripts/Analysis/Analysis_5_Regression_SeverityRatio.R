rm(list=ls())
library(ggplot2)
library(sf)
library(corrplot)
library(viridis)
library(tmap)
library(stargazer)
library(spdep)
library(spatialreg)
library(MASS)
library(bsreg)
library(factoextra)
library(FactoMineR)
library(lmtest)
library(sandwich)
set.seed(1234)

source("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Script_for_paper_v2/GetAllData.R")

regdat = getalldata()
regdat = regdat[,c(1:7,12:29)]

vars = c("STATE_UT","NAME","1st Wave (Prop.)","2nd Wave (Prop.)","1st Wave (Rate)","2nd Wave (Rate)","Severity Ratio",
				"1st Wave (R0)","2nd Wave (R0)","1st Wave (Rt)","2nd Wave (Rt)",
				"Urbanization","Wealth", "Population Dens.","Average Age",
				"No Travel","Average Distance",
                        "On foot","Bicycle","Motorcycle","Car","Taxi","Bus","Train","Water Transport","Other",
				"1st Wave (In Degree)","2nd Wave (In Degree)")

nb = poly2nb(regdat)
lw = nb2listw(nb,zero.policy=T)

## Regression with Proportion
regdat$indeg = (regdat$n_crisis_2w/regdat$n_crisis_1w)

mod1 = lm(log(SR_CumRate)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(indeg) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_lag1 = lagsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error1 = errorsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)

SRmod_lag1$r.squared = summary(SRmod_lag1, Nagelkerke = TRUE)$NK
SRmod_error1$r.squared = summary(SRmod_error1, Nagelkerke = TRUE)$NK

## Heteroscedasticity-corrected SE

# First Wave
mod1_hcse =   coeftest(mod1, vcov=vcovHC(mod1, type="HC1"))
mm = lm(SRmod_lag1$tary ~ SRmod_lag1$tarX - 1)
SRmod_lag1_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))
mm = lm(SRmod_error1$tary ~ SRmod_error1$tarX - 1)
SRmod_error1_hcse =  coeftest(mm, vcov=vcovHC(mm, type="HC1"))

mod1rho = paste((as.character(as.numeric(round(SRmod_lag1$rho,2)))),"***",sep="")
mod1lambda = paste((as.character(as.numeric(round(SRmod_error1$lambda,2)))),"***",sep="")

stargazer(mod1,SRmod_lag1,SRmod_error1,type="html",omit="STATE_UT",
p=list(as.numeric(mod1_hcse[,4]),as.numeric(SRmod_lag1_hcse[,4]),as.numeric(SRmod_error1_hcse[,4])),
se = list(as.numeric(mod1_hcse[,2]),as.numeric(SRmod_lag1_hcse[,2]),as.numeric(SRmod_error1_hcse[,2])),
add.lines = list(c("State Fixed effects?", "Yes", "Yes","Yes"),
                 c("Rho", "-", mod1rho,"-"),
		     c("Lambda","-", "-",mod1lambda)),
covariate.labels = c("Urbanization","Wealth","Log(Population Density)","Population Age",
			   "Log(No Travel)","Log(Distance)","Log(In-Degree(1st Wave/2nd Wave))"),
dep.var.caption = c("log (Severity Ratio)"),
column.labels = c("OLS","Spatial Lag","Spatial Error"),
out = "D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Regression Table/SeverityRatio_revised.doc",no.space=T,single.row=T
)


mod1 = lm(log(SR_CumRate)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(indeg) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_lag1 = lagsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error1 = errorsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)



### Moran's I
#colnames(regdat)[1:28] = vars

nb = poly2nb(regdat)
lw = nb2listw(nb,zero.policy=T)

SeverityRatio_moran = moran.mc(regdat$CumRate_2/regdat$CumRate_1,lw,nsim=1000,zero.policy=T,na.action=na.omit)
print(SeverityRatio_moran)

