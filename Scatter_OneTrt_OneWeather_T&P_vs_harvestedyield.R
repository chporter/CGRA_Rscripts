#comparing weather data vs harvested yield
rm(list=ls(all=TRUE))

setwd("C:/CGRA_data/US_data/Colorado/Wheat/Generic_FtCollins_wheat")
Location <- "Colorado"
Crop <- "wheat"
#trtno <- 6
ID_CLI="0XXX"

#Names of files are constructed from location, crop, climate, and treatment number
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")
OutputFile1 <- paste("Sctr_",Location,"_",Crop,"_",ID_CLI,"_yield_vs_T&P.jpg", sep="")

x<-read.csv(csv_input_file,header=TRUE)

x2<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),substring(x$HDAT,1,4),x$HWAM,x$TMAXA,x$TMINA,x$PRCP)
x2<-setNames(x2, c("TNAM","WSTA","ID_CLIM","year","HWAM","TMAXA","TMINA","PRCP"))

a<-unique(x2$ID_CLIM)
b<-unique(x2$TNAM)

year_crop1<-as.numeric(x2$year[x2$ID_CLIM==ID_CLI])
hwam_crop1<-as.numeric(x2$HWAM[x2$ID_CLIM==ID_CLI])
tmax<-as.numeric(x2$TMAX[x2$ID_CLIM==ID_CLI])
tmin<-as.numeric(x2$TMIN[x2$ID_CLIM==ID_CLI])
prec<-as.numeric(x2$PRCP[x2$ID_CLIM==ID_CLI])

#############################################################################################
#############################################################################################
##export to file
dir <- OutputFile1
jpeg(dir,res=300, width=4, height=8, unit="in")
#par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(0.5,5,1,1),mgp = c(0,0.2, 0))
par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(1,4,1,1),mgp = c(0.2,0.2, 0))
cex_axis=1.0
cex_label=0.9
#cex_axis=1.2
#cex_nam=1.4
#ylim_n=5
#ylim_x=32

#############################################################################################
# plot hwam vs tmax 

plot(tmax,hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)

# trendline
abline(lm(hwam_crop1 ~ tmax), lty=3, col="black")

box()

axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Maximum temperature (°C)", side = 1, line = 1.5, outer = FALSE, at = NA,
       adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)
mtext("Harvested yield (kg/ha)", side = 2, line = 2.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)

#############################################################################################
# plot hwam vs tmin
plot(tmin,hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)

# trendline
abline(lm(hwam_crop1 ~ tmin), lty=3, col="black")

box()

axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Minimum temperature (°C)", side = 1, line = 1.5, outer = FALSE, at = NA,
       adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)
mtext("Harvested yield (kg/ha)", side = 2, line = 2.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)

#############################################################################################
# plot hwam vs precip
plot(prec,hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)

# trendline
abline(lm(hwam_crop1 ~ prec), lty=3, col="black")

box()

axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Precipitation (mm)", side = 1, line = 1.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)
      
mtext("Harvested yield (kg/ha)", side = 2, line = 2.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)

##### 
dev.off()
#####

