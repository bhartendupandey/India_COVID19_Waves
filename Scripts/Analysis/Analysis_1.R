rm(list=ls())
library(sf)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(boot)
set.seed(1234)
setwd("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Data/Intermediate/AllRDS")
files=list.files(pattern="*.rds")

covidnum = readRDS("COVID_Num.rds")
cdemog = as.data.frame(readRDS("GHSL_Threshold10_WorldPop.rds"))
cdemog = subset(cdemog,select=-c(geom))

pltdat = merge(covidnum,cdemog,by=c("STATE_UT","NAME"))
pltdat$CumProp_1 = pltdat$CumSum_1 * 1000000 / pltdat$EstTotPopulation_WorldPop
pltdat$CumProp_2 = pltdat$CumSum_2 * 1000000 / pltdat$EstTotPopulation_WorldPop

cor.test(pltdat$CumProp_1,pltdat$CumProp_2,method="spearman",use="complete.obs")

data = data.frame(xs = pltdat$CumProp_2/pltdat$CumProp_1)
data$ID = 1
data = data[complete.cases(data),]

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
bo <- boot(data[, "xs", drop = FALSE], statistic=meanfun, R=100000)
severityinten = round(mean(bo$t),2)
severityinten_ci = boot.ci(bo, conf=0.95, type="bca")
severityinten_ci_l = round(severityinten_ci$bca[4],2)
severityinten_ci_h = round(severityinten_ci$bca[5],2)
paste("Severity Ratio:",severityinten,sep=" ")
paste("Severity Ratio (95% CI): ",severityinten_ci_l," - ",severityinten_ci_h,sep="")

p1 = ggplot(pltdat,aes(x=CumProp_1,y=CumProp_2)) + geom_point(alpha=0.25,size=2) + theme_bw() +
xlab(expression(Cases * " " * (Million^-1) * "" *": " *First * " "* Wave)) + 
ylab(expression(Cases * " " * (Million^-1) * "" *": " *Second * " "* Wave)) + 
theme(axis.title=element_text(size=15),axis.text=element_text(size=13)) +
labs(caption=paste("n = 636")) + 
geom_smooth(method="lm") + 
stat_regline_equation(label.y = 105000,size=6) + 
stat_regline_equation(label.y =115000, aes(label = ..rr.label..),size=6) +
geom_abline(intercept = 0,slope=1,linetype=2) + 
annotate(geom="text", x=75000, y=10000, label=paste("Severity Ratio:",severityinten,sep=" "),
              size=6) + 
annotate(geom="text", x=75000, y=10, label=paste("95% CI: ",severityinten_ci_l," - ",severityinten_ci_h,sep=""),
              size=6)  + labs(title="a. Incidence Proportion") +
theme(plot.title = element_text(size=20)) +
annotate(geom="text",x=100000,y=90000,label="Line of Equality",size=4) + xlim(0,120000) + ylim(0,120000)

pltdat$CumRate_1 = pltdat$CumProp_1 /pltdat$Length_1
pltdat$CumRate_2 = pltdat$CumProp_2 / pltdat$Length_2

cor.test(pltdat$CumRate_1,pltdat$CumRate_2,method="spearman",use="complete.obs")

data = data.frame(xs = pltdat$CumRate_2/pltdat$CumRate_1)
data$ID = 1
data = data[complete.cases(data),]

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
bo <- boot(data[, "xs", drop = FALSE], statistic=meanfun, R=100000)
severityinten = round(mean(bo$t),2)
severityinten_ci = boot.ci(bo, conf=0.95, type="bca")
severityinten_ci_l = round(severityinten_ci$bca[4],2)
severityinten_ci_h = round(severityinten_ci$bca[5],2)
paste("Severity Ratio:",severityinten,sep=" ")
paste("Severity Ratio (95% CI): ",severityinten_ci_l," - ",severityinten_ci_h,sep="")

p2 = ggplot(pltdat,aes(x=CumRate_1,y=CumRate_2)) + geom_point(alpha=0.25,size=2) + theme_bw() +
xlab(expression(Cases*  " " *  " " * (Million-Day)^-1 * "" *": " *First * " "* Wave)) + 
ylab(expression(Cases*  " " *  " " * (Million-Day)^-1 * "" *": " *Second * " "* Wave)) +
theme(axis.title=element_text(size=15),axis.text=element_text(size=13)) +
labs(caption=paste("n = 636")) + 
geom_smooth(method="lm") + 
stat_regline_equation(label.y = 800,size=6) + 
 stat_regline_equation(label.y =900, aes(label = ..rr.label..),size=6)+
geom_abline(intercept = 0,linetype=2) + 
annotate(geom="text", x=185, y=75, label=paste("Severity Ratio:",severityinten,sep=" "),
              size=6) + 
annotate(geom="text", x=182, y=10, label=paste("95% CI: ",severityinten_ci_l," - ",severityinten_ci_h,sep=""),
              size=6) + labs(title="b. Incidence Rate") +
theme(plot.title = element_text(size=20)) +
annotate(geom="text",x=220,y=250,label="Line of Equality",size=4) +  xlim(0,1000) + ylim(0,1000)

r0 = readRDS("R0dist.rds")
cdemog = readRDS("Census_Demog.rds")
r0m = merge(cdemog,r0,by=c("STATE_UT","NAME"),all.x=T)
pltdat = r0m

data = data.frame(xs = pltdat$w2/pltdat$w1)
data$ID = 1
data = data[complete.cases(data),]

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
bo <- boot(data[, "xs", drop = FALSE], statistic=meanfun, R=100000)
severityinten = round(mean(bo$t),2)
severityinten_ci = boot.ci(bo, conf=0.95, type="bca")
severityinten_ci_l = round(severityinten_ci$bca[4],2)
severityinten_ci_h = round(severityinten_ci$bca[5],2)
paste("Severity Ratio:",severityinten,sep=" ")
paste("Severity Ratio (95% CI): ",severityinten_ci_l," - ",severityinten_ci_h,sep="")

p3 = ggplot(pltdat,aes(x=w1,y=w2)) + geom_point(alpha=0.25,size=2) + theme_bw() +
xlab(expression(R[0] * "" *": " *First * " "* Wave)) + 
ylab(expression(R[0] * "" *": " *Second * " "* Wave)) +
theme(axis.title=element_text(size=15),axis.text=element_text(size=13)) +
labs(caption=paste("n = 636")) + 
geom_abline(intercept = 0,linetype=2) + 
annotate(geom="text", x=2, y=0.90, label=paste("Severity Ratio:",severityinten,sep=" "),
              size=6) + 
annotate(geom="text", x=2.01, y=0.75, label=paste("95% CI: ",severityinten_ci_l," - ",severityinten_ci_h,sep=""),
              size=6) + labs(title="a. Basic Reproduction Number") +
theme(plot.title = element_text(size=20)) +
annotate(geom="text",x=2.25,y=2.25,label="Line of Equality",size=4)



## RE

r3 = readRDS("Rt_dist.rds")
cdemog = readRDS("Census_Demog.rds")
r3m = merge(cdemog,r3,by=c("STATE_UT","NAME"),all.x=T)
pltdat = r3m

data = data.frame(xs = pltdat$w2/pltdat$w1)
data$ID = 1
data = data[complete.cases(data),]

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
bo <- boot(data[, "xs", drop = FALSE], statistic=meanfun, R=100000)
severityinten = round(mean(bo$t),2)
severityinten_ci = boot.ci(bo, conf=0.95, type="bca")
severityinten_ci_l = round(severityinten_ci$bca[4],2)
severityinten_ci_h = round(severityinten_ci$bca[5],2)
paste("Severity Ratio:",severityinten,sep=" ")
paste("Severity Ratio (95% CI): ",severityinten_ci_l," - ",severityinten_ci_h,sep="")

p4 = ggplot(pltdat,aes(x=w1,y=w2)) + geom_point(alpha=0.25,size=2) + theme_bw() +
xlab(expression(R[t]^mu * "" *": " *First * " "* Wave)) + 
ylab(expression(R[t]^mu * "" *": " *Second * " "* Wave)) +
theme(axis.title=element_text(size=15),axis.text=element_text(size=13)) +
labs(caption=paste("n = 617")) + 
geom_abline(intercept = 0,linetype=2) + 
annotate(geom="text", x=1.4, y=0.90, label=paste("Severity Ratio:",severityinten,sep=" "),
              size=6) + 
annotate(geom="text", x=1.41, y=0.75, label=paste("95% CI: ",severityinten_ci_l," - ",severityinten_ci_h,sep=""),
              size=6) + labs(title="b. Effective Reproduction Number") +
theme(plot.title = element_text(size=20)) +
annotate(geom="text",x=1.5,y=1.8,label="Line of Equality",size=4)

dev.new(width=11.708,height=5)
outplt = grid.arrange(p1,p2,ncol=2,nrow=1)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Fig1.pdf",outplt,width=11.708,height=5)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Fig1.jpg",outplt,width=11.708,height=5)

dev.new(width=11.708,height=5)
outplt = grid.arrange(p3,p4,ncol=2,nrow=1)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Fig1_ro_rt.pdf",outplt,width=11.708,height=5)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/Fig1_ro_rt.jpg",outplt,width=11.708,height=5)

numpeaks = readRDS("COVID_wmet.rds")
dev.new()
npeak = ggplot() + geom_sf(data = numpeaks, aes(fill = Confirmed_NPeaks),color=NA) + 
scale_fill_viridis_c(name="Peaks") + 
xlab("Longitude") + 
ylab("Latitude") +
theme_bw()
npeak

ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS1_npeak.pdf",npeak)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS1_npeak.jpg",npeak)






