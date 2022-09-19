rm(list=ls())
library(ggplot2)
library(sf)
library(corrplot)
library(viridis)
library(tmap)
library(stargazer)

source("D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Script_for_paper_v2/GetAllData.R")

data = getalldata()

pltdat = subset(data,select=-c(STATE_UT,NAME))
pltdat$n_crisis_1w = pltdat$n_crisis_1w + 1
pltdat$n_crisis_2w = pltdat$n_crisis_2w + 1


vars = c("1st Wave (Prop.)","2nd Wave (Prop.)","1st Wave (Rate)","2nd Wave (Rate)","Severity Ratio",
				"1st Wave (R0)","2nd Wave (R0)","1st Wave (Rt)","2nd Wave (Rt)",
				"Urbanization","Wealth", "Population Dens.","Average Age",
				"No Travel","Average Distance",
                        "On foot","Bicycle","Motorcycle","Car","Taxi","Bus","Train","Water Transport","Other",
				"1st Wave (In Degree)","2nd Wave (In Degree)")
p1 = tm_shape(pltdat) +
    tm_polygons("CumProp_1", title=vars[1],style="log10",border.alpha=0,lwd=0,palette = "viridis",legend.reverse=T) +
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p2 = tm_shape(pltdat) +
    tm_polygons("CumProp_2", title=vars[2],style="log10",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p3 = tm_shape(pltdat) +
    tm_polygons("CumRate_1", title=vars[3],style="log10",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p4 = tm_shape(pltdat) +
    tm_polygons("CumRate_2", title=vars[4],style="log10",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p5 = tm_shape(pltdat) +
    tm_polygons("SR_CumRate", title=vars[5],style="log10",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p6 = tm_shape(pltdat) +
    tm_polygons("R0_1", title=vars[6],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p7 = tm_shape(pltdat) +
    tm_polygons("R0_2", title=vars[7],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p8 = tm_shape(pltdat) +
    tm_polygons("Rt_1", title=vars[8],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p9 = tm_shape(pltdat) +
    tm_polygons("Rt_2", title=vars[9],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)

dev.new(height=10,width=10)
outplt = tmap_arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3)
tmap_save(outplt,"D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigSX_COVIDMETRICS.jpg",width=10,height=10)

p10 = tm_shape(pltdat) +
    tm_polygons("Urbanization", title=vars[10],style="cont",border.alpha=0,lwd=0,palette = "viridis",legend.reverse=T) +
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p11 = tm_shape(pltdat) +
    tm_polygons("AvgWealth", title=vars[11],style="cont",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p12 = tm_shape(pltdat) +
    tm_polygons("PopDen", title=vars[12],style="log10",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p13 = tm_shape(pltdat) +
    tm_polygons("Age", title=vars[13],style="cont",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p14 = tm_shape(pltdat) +
    tm_polygons("notravel", title=vars[14],style="cont",border.alpha=0,palette = "viridis",legend.reverse=T)+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p15 = tm_shape(pltdat) +
    tm_polygons("disttravel", title=vars[15],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p16 = tm_shape(pltdat) +
    tm_polygons("OnFoot", title=vars[16],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p17 = tm_shape(pltdat) +
    tm_polygons("Bicycle", title=vars[17],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p18 = tm_shape(pltdat) +
    tm_polygons("Motorcycle", title=vars[18],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p19 = tm_shape(pltdat) +
    tm_polygons("Car", title=vars[19],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p20 = tm_shape(pltdat) +
    tm_polygons("Taxi", title=vars[20],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p21 = tm_shape(pltdat) +
    tm_polygons("Bus", title=vars[21],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p22 = tm_shape(pltdat) +
    tm_polygons("Train", title=vars[22],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p23 = tm_shape(pltdat) +
    tm_polygons("Water", title=vars[23],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p24 = tm_shape(pltdat) +
    tm_polygons("Other", title=vars[24],border.alpha=0,palette = "viridis",legend.reverse=T,style="cont")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p25 = tm_shape(pltdat) +
    tm_polygons("n_crisis_1w", title=vars[25],border.alpha=0,palette = "viridis",legend.reverse=T,style="log10")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)
p26 = tm_shape(pltdat) +
    tm_polygons("n_crisis_2w", title=vars[26],border.alpha=0,palette = "viridis",legend.reverse=T,style="log10")+
	tm_layout(legend.position=c('right', 'bottom'), bg.color = NA)

dev.new(height=10,width=10)
outplt = tmap_arrange(p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26, ncol = 4)
tmap_save(outplt,"D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/FigSX_COVIDCoVars.jpg",width=10,height=10)

stargazer(subset(as.data.frame(pltdat),select=-c(geometry)),type="html",iqr = FALSE,out=
"D:/GoogleDrive/Workstation/Projects/Health/COVID-19-F/COVID-19/Figures/Script_for_paper_v2/SummaryRegTable.doc")




