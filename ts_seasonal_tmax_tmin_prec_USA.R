##time series of seasonal tmin, tmax and prec over 30 years (United states)
#graph observed vs simulate

rm(list=ls(all=TRUE))

#x<-read.csv("D:/2017/CGRA data/from_cheryl/Colorado/summary_Wheat/Summary_Wheat_CM2.csv",header=TRUE)
x<-read.csv("summary_Camilla_peanut.csv",header=TRUE)

dir<-"Camilla_peanut_weather_ts.jpeg"
jpeg(dir,res=300, width=4.5, height=8, unit="in")
crop_legend="peanut"

par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(1,4,1,1),mgp = c(0,0.2, 0))

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

#### For Camilla, Georgia, use treatment 6
tnam <- as.character(b[6])
####

cex_axis=1.2
cex_nam=1.4
ylim_n=5
ylim_x=32

year_crop1<-as.numeric(x2$year[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI ])
year_crop1
tmax<-as.numeric(x2$TMAX[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
tmax
tmin<-as.numeric(x2$TMIN[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
tmin
prec<-as.numeric(x2$PRCP[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
prec

#### Maximum temperature plot ####
#plot(year_crop1, hwam_crop1)
plot((c(1980:2009)), tmax , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)
abline(lm(tmax ~ c(1980:2009)), lty=3,lwd=0.75)
##legend("topleft",inset = 0.01,xpd=TRUE,
#legend("topright",inset = 0.0,xpd=TRUE,
#              c("Maximum temperature"),
##       pch=c(19),
##       lty=c(5),
##      lwd=c(0.75),
#       col=c("black"),
#       horiz=TRUE,
#       cex=cex_nam,
#       bty="n")

mtext("Max Temperature (°C)", side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

#### Minimum temperature plot ####
plot((c(1980:2009)), tmin , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)
abline(lm(tmin ~ c(1980:2009)), lty=3,lwd=0.75)
##legend("topleft",inset = 0.01,xpd=TRUE,
#legend("topright",inset = 0.0,xpd=TRUE,
#       c("Minimum temperature"),
#       # pch=c(19),
#       # lty=c(5),
#       # lwd=c(0.75),
#       col=c("black"),
#       horiz=TRUE,
#       cex=cex_nam,
#       bty="n")

mtext("Min Temperature (°C)", side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

#### Rainfall plot ####
plot((c(1980:2009)), prec , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)
abline(lm(prec ~ c(1980:2009)), lty=3,lwd=0.75)
##legend("topleft",inset = 0.01,xpd=TRUE,
#legend("topright",inset = 0.00,xpd=TRUE,
#       c("Precipitation"),
#       # pch=c(19),
#       # lty=c(5),
#       # lwd=c(0.75),
#       col=c("black"),
#       horiz=TRUE,
#       cex=cex_nam,
#       bty="n")

mtext("Rain (mm)", side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)


##### 
dev.off()
#####

