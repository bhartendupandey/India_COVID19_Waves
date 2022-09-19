rm(list=ls())
library(ggplot2)
library(sf)
library(corrplot)
library(viridis)
library(tmap)
library(stargazer)
library(spdep)
library(MASS)
library(bsreg)
library(factoextra)
library(FactoMineR)
library(lmtest)
library(sandwich)
set.seed(1234)

source("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Script_for_paper_v2/GetAllData.R")

regdat = getalldata()
regdat = regdat[,c(1:6,12:29)]

vars = c("STATE_UT","NAME","1st Wave (Prop.)","2nd Wave (Prop.)","1st Wave (Rate)","2nd Wave (Rate)","Severity Ratio",
				"1st Wave (R0)","2nd Wave (R0)","1st Wave (Rt)","2nd Wave (Rt)",
				"Urbanization","Wealth", "Population Dens.","Average Age",
				"No Travel","Average Distance",
                        "On foot","Bicycle","Motorcycle","Car","Taxi","Bus","Train","Water Transport","Other",
				"1st Wave (In Degree)","2nd Wave (In Degree)")


nb = poly2nb(regdat)
lw = nb2listw(nb,zero.policy=T)

## Regression with Proportion
#First Wave: mod1,SRmod_lag1,SRmod_error1
#Second Wave: mod2, SRmod_lag2,SRmod_error2

mod1 = lm(log(CumProp_1)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_1w) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_lag1 = lagsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error1 = errorsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)

mod2 = lm(log(CumProp_2)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_2w) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_lag2 = lagsarlm(mod2$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error2 = errorsarlm(mod2$call$formula,data=regdat,lw,zero.policy=T)

SRmod_lag1$r.squared = summary(SRmod_lag1, Nagelkerke = TRUE)$NK
SRmod_error1$r.squared = summary(SRmod_error1, Nagelkerke = TRUE)$NK
SRmod_lag2$r.squared = summary(SRmod_lag2, Nagelkerke = TRUE)$NK
SRmod_error2$r.squared = summary(SRmod_error2, Nagelkerke = TRUE)$NK

## Heteroscedasticity-corrected SE

# First Wave
mod1_hcse =   coeftest(mod1, vcov=vcovHC(mod1, type="HC1"))
mm = lm(SRmod_lag1$tary ~ SRmod_lag1$tarX - 1)
SRmod_lag1_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))
mm = lm(SRmod_error1$tary ~ SRmod_error1$tarX - 1)
SRmod_error1_hcse =  coeftest(mm, vcov=vcovHC(mm, type="HC1"))

mod2_hcse =   coeftest(mod2, vcov=vcovHC(mod2, type="HC1"))
mm = lm(SRmod_lag2$tary ~ SRmod_lag2$tarX - 1)
SRmod_lag2_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))
mm = lm(SRmod_error2$tary ~ SRmod_error2$tarX - 1)
SRmod_error2_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))

mod1rho = paste((as.character(as.numeric(round(SRmod_lag1$rho,2)))),"*",sep="")
mod2rho = paste((as.character(as.numeric(round(SRmod_lag2$rho,2)))),"",sep="")

mod1lambda = paste((as.character(as.numeric(round(SRmod_error1$lambda,2)))),"***",sep="")
mod2lambda = paste((as.character(as.numeric(round(SRmod_error2$lambda,2)))),"***",sep="")

stargazer(mod1,SRmod_lag1,SRmod_error1,mod2,SRmod_lag2,SRmod_error2,type="html",omit="STATE_UT",
p=list(as.numeric(mod1_hcse[,4]),as.numeric(SRmod_lag1_hcse[,4]),as.numeric(SRmod_error1_hcse[,4]),
       as.numeric(mod2_hcse[,4]),as.numeric(SRmod_lag2_hcse[,4]),as.numeric(SRmod_error2_hcse[,4])),
se = list(as.numeric(mod1_hcse[,2]),as.numeric(SRmod_lag1_hcse[,2]),as.numeric(SRmod_error1_hcse[,2]),
          as.numeric(mod2_hcse[,2]),as.numeric(SRmod_lag2_hcse[,2]),as.numeric(SRmod_error2_hcse[,2])),
add.lines = list(c("State Fixed effects?", "Yes", "Yes","Yes","Yes","Yes","Yes"),
                 c("Rho", "-", mod1rho,"-","-",mod2rho,"-"),
		     c("Lambda","-", "-",mod1lambda,"-","-",mod2lambda)),
covariate.labels = c("Urbanization","Wealth","Log(Population Density)","Population Age",
			   "Log(No Travel)","Log(Distance)","Log(1+In-Degree(1st Wave))",
			   "Log(1+In-Degree(2nd Wave))"),
dep.var.caption = c("log (Cumulative Incidence Proportion)"),
#column.labels   = c("First Wave", "Second Wave"),
#column.separate = c(3, 3),
#dep.var.labels =c("","First Wave","","","Second Wave",""),
column.labels = c("OLS","Spatial Lag","Spatial Error","OLS","Spatial Lag","Spatial Error"),
out = "D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Regression Table/CumProp.doc",no.space=T,single.row=T
)

## Regression with Rate

#First Wave: mod1,SRmod_lag1,SRmod_error1
#Second Wave: mod2, SRmod_lag2,SRmod_error2

mod1 = lm(log(CumRate_1)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_1w) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_lag1 = lagsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error1 = errorsarlm(mod1$call$formula,data=regdat,lw,zero.policy=T)

mod2 = lm(log(CumRate_2)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_2w) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_lag2 = lagsarlm(mod2$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error2 = errorsarlm(mod2$call$formula,data=regdat,lw,zero.policy=T)

SRmod_lag1$r.squared = summary(SRmod_lag1, Nagelkerke = TRUE)$NK
SRmod_error1$r.squared = summary(SRmod_error1, Nagelkerke = TRUE)$NK
SRmod_lag2$r.squared = summary(SRmod_lag2, Nagelkerke = TRUE)$NK
SRmod_error2$r.squared = summary(SRmod_error2, Nagelkerke = TRUE)$NK

## Heteroscedasticity-corrected SE

# First Wave
mod1_hcse =   coeftest(mod1, vcov=vcovHC(mod1, type="HC1"))
mm = lm(SRmod_lag1$tary ~ SRmod_lag1$tarX - 1)
SRmod_lag1_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))
mm = lm(SRmod_error1$tary ~ SRmod_error1$tarX - 1)
SRmod_error1_hcse =  coeftest(mm, vcov=vcovHC(mm, type="HC1"))

mod2_hcse =   coeftest(mod2, vcov=vcovHC(mod2, type="HC1"))
mm = lm(SRmod_lag2$tary ~ SRmod_lag2$tarX - 1)
SRmod_lag2_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))
mm = lm(SRmod_error2$tary ~ SRmod_error2$tarX - 1)
SRmod_error2_hcse =   coeftest(mm, vcov=vcovHC(mm, type="HC1"))

mod1rho = paste((as.character(as.numeric(round(SRmod_lag1$rho,2)))),"***",sep="")
mod2rho = paste((as.character(as.numeric(round(SRmod_lag2$rho,2)))),"",sep="")

mod1lambda = paste((as.character(as.numeric(round(SRmod_error1$lambda,2)))),"***",sep="")
mod2lambda = paste((as.character(as.numeric(round(SRmod_error2$lambda,2)))),"***",sep="")

stargazer(mod1,SRmod_lag1,SRmod_error1,mod2,SRmod_lag2,SRmod_error2,type="html",omit="STATE_UT",
p=list(as.numeric(mod1_hcse[,4]),as.numeric(SRmod_lag1_hcse[,4]),as.numeric(SRmod_error1_hcse[,4]),
       as.numeric(mod2_hcse[,4]),as.numeric(SRmod_lag2_hcse[,4]),as.numeric(SRmod_error2_hcse[,4])),
se = list(as.numeric(mod1_hcse[,2]),as.numeric(SRmod_lag1_hcse[,2]),as.numeric(SRmod_error1_hcse[,2]),
          as.numeric(mod2_hcse[,2]),as.numeric(SRmod_lag2_hcse[,2]),as.numeric(SRmod_error2_hcse[,2])),
add.lines = list(c("State Fixed effects?", "Yes", "Yes","Yes","Yes","Yes","Yes"),
                 c("Rho", "-", mod1rho,"-","-",mod2rho,"-"),
		     c("Lambda","-", "-",mod1lambda,"-","-",mod2lambda)),
covariate.labels = c("Urbanization","Wealth","Log(Population Density)","Population Age",
			   "Log(No Travel)","Log(Distance)","Log(1+In-Degree(1st Wave))",
			   "Log(1+In-Degree(2nd Wave))"),
dep.var.caption = c("log (Cumulative Incidence Rate)"),
column.labels = c("OLS","Spatial Lag","Spatial Error","OLS","Spatial Lag","Spatial Error"),
out = "D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Regression Table/CumRate.doc",no.space=T,single.row=T
)


### Moran's I
#colnames(regdat)[1:28] = vars
regdat
summary(regdat)

nb = poly2nb(regdat)
lw = nb2listw(nb,zero.policy=T)

CumProp_1_moran = moran.mc(regdat$CumProp_1,lw,nsim=1000,zero.policy=T,na.action=na.omit)
print(CumProp_1_moran)

CumProp_2_moran = moran.mc(regdat$CumProp_2,lw,nsim=1000,zero.policy=T,na.action=na.omit)
print(CumProp_2_moran)

CumRate_1_moran = moran.mc(regdat$CumRate_1,lw,nsim=1000,zero.policy=T,na.action=na.omit)
print(CumRate_1_moran)

CumRate_2_moran = moran.mc(regdat$CumRate_2,lw,nsim=1000,zero.policy=T,na.action=na.omit)
print(CumRate_2_moran)
