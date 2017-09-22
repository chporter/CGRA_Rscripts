#comparing weather data vs harvested yield
rm(list=ls(all=TRUE))

Location <- "Camilla"
Crop <- "peanut"
trtno1 <- 5
trtno2 <- 6
ID_CLI="0XXX"

#Names of files are constructed from location, crop, climate, and treatment number
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")
OutputFile1 <- paste("Sctr_",Location,"_",Crop,"_",ID_CLI,"_yield_vs_T&P.jpg", sep="")

x<-read.csv(csv_input_file,header=TRUE)

x2<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),substring(x$HDAT,1,4),x$HWAM,x$TMAXA,x$TMINA,x$PRCP)
x2<-setNames(x2, c("TNAM","WSTA","ID_CLIM","year","HWAM","TMAXA","TMINA","PRCP"))

a<-unique(x2$ID_CLIM)
b<-unique(x2$TNAM)

tnam1 <- as.character(b[trtno1])
tnam2 <- as.character(b[trtno2])

hwam_crop1<-as.numeric(x2$HWAM[x2$TNAM==tnam1 & x2$ID_CLIM==ID_CLI])
tmax1<-as.numeric(x2$TMAX[x2$TNAM==tnam1 & x2$ID_CLIM==ID_CLI])
tmin1<-as.numeric(x2$TMIN[x2$TNAM==tnam1 & x2$ID_CLIM==ID_CLI])
prec1<-as.numeric(x2$PRCP[x2$TNAM==tnam1 & x2$ID_CLIM==ID_CLI])

hwam_crop2<-as.numeric(x2$HWAM[x2$TNAM==tnam2 & x2$ID_CLIM==ID_CLI])
tmax2<-as.numeric(x2$TMAX[x2$TNAM==tnam2 & x2$ID_CLIM==ID_CLI])
tmin2<-as.numeric(x2$TMIN[x2$TNAM==tnam2 & x2$ID_CLIM==ID_CLI])
prec2<-as.numeric(x2$PRCP[x2$TNAM==tnam2 & x2$ID_CLIM==ID_CLI])

#############################################################################################
#############################################################################################
##export to file
dir <- OutputFile1
jpeg(dir,res=300, width=4, height=8, unit="in")
###
# plot info
#par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(6,5,1,1),mgp = c(0,0.2, 0), xpd=TRUE)
par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(1,4,1,1),mgp = c(0.2,0.2, 0))
cex_axis=1.0
cex_label=0.9

#############################################################################################
# plot hwam vs tmax 
plot(tmax1,hwam_crop1 , 
     pch= 19, col="blue", las=1, ylab="",xlab="",axes=F, 
     ylim = c(min(hwam_crop1-250,hwam_crop2-250),max(hwam_crop1+250,hwam_crop2+250)),
     xlim = c(min(tmax1,tmax2),max(tmax1,tmax2)))

# add points for treatment 2
points(tmax2,hwam_crop2, pch=19, col="red")

# add trendlines
abline(lm(hwam_crop1 ~ tmax1), col="blue")
abline(lm(hwam_crop2 ~ tmax2), col="red")

#legend("bottomright", inset=c(0,-0.60), legend=c("irrigated","rainfed"), pch=c(19,19), col = c("red","blue"), horiz=TRUE)
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
plot(tmin1,hwam_crop1 , 
     pch= 19, col="blue", las=1, ylab="",xlab="",axes=F,
     ylim = c(min(hwam_crop1-250,hwam_crop2-250),max(hwam_crop1+250,hwam_crop2+250)),
     xlim = c(min(tmin1,tmin2),max(tmin1,tmin2)))
points(tmin2,hwam_crop2, pch=19, col="red")

# trendlines
abline(lm(hwam_crop1 ~ tmin1), col="blue")
abline(lm(hwam_crop2 ~ tmin2), col="red")

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
plot(prec1, hwam_crop1 , 
  pch= 19, col="blue", las=1, ylab="",xlab="",axes=F, 
  ylim = c(min(hwam_crop1-250,hwam_crop2-500),max(hwam_crop1+250,hwam_crop2+250)),
  xlim = c(min(prec1,prec2),max(prec1,prec2)))
points(prec2,hwam_crop2, pch=19, col="red")

# trendlines
abline(lm(hwam_crop1 ~ prec1), col="blue")
abline(lm(hwam_crop2 ~ prec2), col="red")

#legend("left", inset=c(0,0), legend=c("irrigated","rainfed"), pch=c(19,19), col = c("red","blue"))
box()

axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Precipitation (mm)", side = 1, line = 1.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)
mtext("Harvested yield (kg/ha)", side = 2, line = 2.5, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_label, col = NA, font = NA)


#mtext("Harvested yield (kg/ha)", side = 2, line = 3.5, outer = TRUE, at = 0.55, col="black",
#      adj = NA, padj = NA, cex =cex_label, font = NA)

legend("bottomright", legend=c("irrigated","rainfed"), pch=c(19,19), col = c("red","blue"), horiz=TRUE)
#legend("bottomright", inset=c(0.0,-0.4), legend=c("irrigated","rainfed"), pch=c(19,19), col = c("red","blue"), horiz=TRUE)
#legend("left", inset=c(0.0,-0.4), legend=c("irrigated","rainfed"), pch=c(19,19), col = c("red","blue") )#, horiz=TRUE)



##### 
dev.off()
#####

