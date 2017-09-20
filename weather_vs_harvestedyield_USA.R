#comparing wether data vs harvested yield
rm(list=ls(all=TRUE))

x<-read.csv("summary_Colorado_wheat.csv",header=TRUE)
#x<-read.csv("D:/2017/CGRA data/from_cheryl/Ames/summary_Soybean/Summary_Soybean_CM2.csv",header=TRUE)

dir<-"Colorado_wheat_yield_vs_weather.jpeg"
jpeg(dir,res=300, width=4, height=10, unit="in")

par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(0.5,5,1,1),mgp = c(0,0.2, 0))

x1<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),substring(x$HDAT,1,4),x$HWAM,x$TMAXA,x$TMINA,x$PRCP)
head(x1)
x2<-setNames(x1, c("TNAM","WSTA","ID_CLIM","year","HWAM","TMAXA","TMINA","PRCP"))
head(x2)
x2

a<-unique(x2$ID_CLIM)
a

b<-unique(x2$TNAM)
b

ID_CLI="0XXX"
tnam=b[1]

cex_axis=1.2
cex_nam=1.4
crop_legend="Maize"
ylim_n=5
ylim_x=32

year_crop1<-as.numeric(x2$year[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI ])
year_crop1
hwam_crop1<-as.numeric(x2$HWAM[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
hwam_crop1
tmax<-as.numeric(x2$TMAX[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
tmax
tmin<-as.numeric(x2$TMIN[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
tmin
prec<-as.numeric(x2$PRCP[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
prec

#year_crop2<-as.numeric(y2$year[y2$TNAM==137 & y2$ID_CLIM==ID_CLI ])
#hwam_crop2<-as.numeric(y2$HWAM[y2$TNAM==137 & y2$ID_CLIM==ID_CLI])
#plot(year_crop1, hwam_crop1)
plot(tmax,hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)


mtext("Maximum temperature (°C)", side = 1, line = 1.75, outer = FALSE, at = NA,
       adj = NA, padj = NA, cex =1, col = NA, font = NA)
mtext("Harvested yield (kg/ha)", side = 2, line = 3.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

plot(tmin,hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Minimum temperature (°C)", side = 1, line = 1.75, outer = FALSE, at = NA,
       adj = NA, padj = NA, cex =1, col = NA, font = NA)
mtext("Harvested yield (kg/ha)", side = 2, line = 3.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

plot(prec,hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Precipitation (mm)", side = 1, line = 1.75, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

mtext("Harvested yield (kg/ha)", side = 2, line = 3.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

##### 
dev.off()
#####

