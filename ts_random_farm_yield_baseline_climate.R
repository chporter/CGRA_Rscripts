##time series of harvested yield over 30 years (United states)
#graph observed vs simulated
rm(list=ls(all=TRUE))

#x<-read.csv("D:/2017/CGRA data/from_cheryl/COlorado/summary_Maize/Summary_Maize_CM2.csv",header=TRUE)
x<-read.csv("summary_Camilla_peanut.csv",header=TRUE)

##export to file
dir<-"Camilla_peanut_yield_ts.jpeg"
jpeg(dir,res=300, width=6, height=4, unit="in")
crop_legend="peanut"
ID_CLI="0XXX"

par (mfrow=c(1,1),mar=c(0,0,0,0),oma=c(3,4,1,1),mgp = c(0,0.2, 0))

x1<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),substring(x$HDAT,1,4), x$HWAM)
head(x1)
x2<-setNames(x1, c("TNAM","WSTA","ID_CLIM","year","HWAM"))
head(x2)

##to identify the mean of every ID_CLIM
#boxplot(x2$HWAM~x2$ID_CLIM)
#bp<-boxplot(z2$HWAM~z2$ID_CLIM)
##to identify the summary for each ID_CLIM
#bp$stats 

a<-unique(x2$ID_CLIM)
a

b<-unique(x2$TNAM)
b

#tnam="PT normal residue" #for Colorado
#tnam="Iowa maize - soil 1" #for Ames - Maize

##### for Camilla, Georgia, use treatment 6 (least stressed)
tnam <- as.character(b[6])
####

cex_axis=1

#####
##CM1
#####
year_crop1<-as.numeric(x2$year[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI ])
year_crop1
hwam_crop1<-as.numeric(x2$HWAM[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
hwam_crop1
#year_crop2<-as.numeric(y2$year[y2$TNAM==137 & y2$ID_CLIM==ID_CLI ])
#hwam_crop2<-as.numeric(y2$HWAM[y2$TNAM==137 & y2$ID_CLIM==ID_CLI])

#plot(year_crop1, hwam_crop1)
#plot(year_crop1, hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)
plot((c(1980:2009)), hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)

#points(year_millet, hwam_millet , pch= 0)#, ylim=c(200, 800))
#points(year_peanut, hwam_peanut , pch= 5)#, ylim=c(200, 800))
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

abline(lm(hwam_crop1 ~ (c(1980:2009))), lty=3,lwd=0.75)

summary(lm(hwam_crop1 ~ (c(1980:2009))))

#legend("top",inset = 0.01,xpd=TRUE,
#       #c("Maize","Millet","Peanut"),
#       #c("Maize"),
#       c(crop_legend),
#       pch=c(19),
#       lty=c(5),
#       lwd=c(0.75),
#       col=c("black"),
#       horiz=TRUE,
#       cex=1,
#       bty="n")

mtext("Harvested yield (kg)", side = 2, line = 3, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

##### 
dev.off()
#####