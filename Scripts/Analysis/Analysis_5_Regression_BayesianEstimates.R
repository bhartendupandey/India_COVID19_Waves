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
regdat = regdat[,c(1:6,12:29)]

nb = poly2nb(regdat)
lw = nb2listw(nb,zero.policy=T)

## Regression with Proportion
#First Wave: SRmod_error1
#Second Wave: SRmod_error2
mod1 = lm(log(CumProp_1)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_1w) + as.factor(STATE_UT),data=as.data.frame(regdat))
mod2 = lm(log(CumProp_2)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_2w) + as.factor(STATE_UT),data=as.data.frame(regdat))

SRmod_error1 = spBreg_err(mod1$call$formula,data=regdat,lw,zero.policy=T)
SRmod_error2 = spBreg_err(mod2$call$formula,data=regdat,lw,zero.policy=T)

summary(SRmod_error1)
summary(SRmod_error2)

## Regression with Rate

#First Wave: mod1,SRmod_lag1,SRmod_error1
#Second Wave: mod2, SRmod_lag2,SRmod_error2

mod1 = lm(log(CumRate_1)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_1w) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_error1 = spBreg_err(mod1$call$formula,data=regdat,lw,zero.policy=T)

mod2 = lm(log(CumRate_2)~Urbanization + AvgWealth + log(PopDen) + Age + log(notravel) + log(disttravel) + log1p(n_crisis_2w) + as.factor(STATE_UT),data=as.data.frame(regdat))
SRmod_error2 = spBreg_err(mod2$call$formula,data=regdat,lw,zero.policy=T)

summary(SRmod_error1)
summary(SRmod_error2)

pltdat = read.csv("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Regression Table/BayesianEstimates.csv")
pltdat$Variable = factor(pltdat$Variable,levels=pltdat$Variable[1:7])
pltdat = subset(pltdat,Model=="CIP")

p1 = ggplot(pltdat, aes(x=Variable, y=Estimate)) + geom_hline(yintercept=0,size=0.75,linetype="dashed")+
geom_pointrange(aes(ymin=Lower.Bound, ymax=Upper.Bound)) + facet_wrap(~Model+Wave) +
 xlab("Covariate") + ylab("Regression Estimate")+ theme_bw() +
theme(axis.title=element_text(size=16),axis.text=element_text(size=16),
axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
strip.text.x = element_text(size = 20))

p2 = ggplot(pltdat, aes(x=Variable, y=Estimate)) + geom_hline(yintercept=0,size=0.75,linetype="dashed")+
geom_pointrange(aes(ymin=Lower.Bound, ymax=Upper.Bound)) + facet_wrap(~Model+Wave) +
 xlab("Covariate") + ylab("Regression Estimate")+ theme_bw() +
theme(axis.title=element_text(size=16),axis.text=element_text(size=16),
axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
strip.text.x = element_text(size = 20))


