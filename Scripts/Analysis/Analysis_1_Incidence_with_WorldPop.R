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
cdemog = as.data.frame(readRDS("Census_Demog.rds"))
cdemog = subset(cdemog,select=-c(geom))


pltdat = merge(covidnum,cdemog,by=c("STATE_UT","NAME"))
pltdat$CumProp_1 = pltdat$CumSum_1 * 1000000 / pltdat$TOT_POP
pltdat$CumProp_2 = pltdat$CumSum_2 * 1000000 / pltdat$TOT_POP

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
geom_abline(intercept = 0,linetype=2) + 
annotate(geom="text", x=75000, y=10000, label=paste("Severity Ratio:",severityinten,sep=" "),
              size=6) + 
annotate(geom="text", x=75000, y=10, label=paste("95% CI: ",severityinten_ci_l," - ",severityinten_ci_h,sep=""),
              size=6)  + labs(title="a. Incidence Proportion") +
theme(plot.title = element_text(size=20)) +
annotate(geom="text",x=100000,y=90000,label="Line of Equality",size=4)

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
annotate(geom="text", x=250, y=75, label=paste("Severity Ratio:",severityinten,sep=" "),
              size=6) + 
annotate(geom="text", x=248, y=10, label=paste("95% CI: ",severityinten_ci_l," - ",severityinten_ci_h,sep=""),
              size=6) + labs(title="b. Incidence Rate") +
theme(plot.title = element_text(size=20)) +
annotate(geom="text",x=220,y=250,label="Line of Equality",size=4)

outplt=grid.arrange(p1,p2,ncol=2)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS2_Incidence_Census.pdf",outplt,width=13,height=7)
ggsave("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigS2_Incidence_Census.jpg",outplt,width=13,height=7)
