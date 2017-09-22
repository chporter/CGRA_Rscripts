#Script to display whiskert plots for one farm 
#variables:
# - Harvested yield (kg/ha)
# - Total biomass (kg/ha)
# - Seasonal Tmax (°C)
# - Seasonal Tmin (°C)
# - Seasonal prec (°C)
#graph observed vs simulated
rm(list=ls(all=TRUE))

Location <- "Camilla"
Crop <- "soybean"
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")
trtno <- 6

OutputFile1 <- paste("B&W_",Location,"_",Crop,"_Trt",trtno,"_growth.jpg", sep="")
OutputFile2 <- paste("B&W_",Location,"_",Crop,"_Trt",trtno,"_weather.jpg", sep="")

#reading table
x<-read.csv(csv_input_file,header=TRUE)

##weather station code
b<-unique(x$WSTA)

##farm code
c<-unique(x$TNAM)

#### treatment number
tnam <- as.character(c[trtno])

x2 <- subset(x, TNAM == tnam, select=c(TNAM, WSTA, PDAT, ADAT, MDAT, HWAM, CWAM, TMAXA, TMINA, PRCP))

pyear <- floor(x2$PDAT / 1000)  # truncate
pdoy <- x2$PDAT %% 1000         # modulo function
pdat <- ISOdate(pyear,12,31) + pdoy * 24 * 3600

ayear <- floor(x2$ADAT / 1000)
adoy <- x2$ADAT %% 1000
adat <- ISOdate(ayear,12,31) + adoy * 24 * 3600

myear <- floor(x2$MDAT / 1000)
mdoy <- x2$MDAT %% 1000
mdat <- ISOdate(myear,12,31) + mdoy * 24 * 3600

x2$nday_flo <- as.numeric(adat - pdat)
x2$nday_mat <- as.numeric(mdat - pdat)

###################################

####size of x-axis and y-axis labels
cex=1.2
cex_lab=0.95
###################################

#####to export the figure as a file
dir<-OutputFile1
jpeg(dir,res=300, width=7, height=11, unit="in")
#####

#number of panels in a figure
par (mfrow=c(4,1),mar=c(0,4,0,1),oma=c(6,2,2,1),mgp = c(0,0.2, 0))
#number of panels in a figure

#a=x$HWAM[x$TNAM==tnam & x$WSTA=="SNNRV4XF"]
#a

#####original without edition
#boxplot(biomass~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(2, tck=0.01,las=1,cex.axis=1)
#############################

boxplot(
  x2$HWAM~x2$WSTA, 
  xaxt="n",
  yaxt="n",
  las=1,
  at=c(1,2,3,4,5,6, 8,9,10,11,12), 
  col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, 
#  outline=F
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)#AN00021
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Harv. yield (kg/ha)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

boxplot(x2$CWAM~x2$WSTA, xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Biomass (kg/ha)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

boxplot(x2$nday_flo~x2$WSTA, xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Days to flowering", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

boxplot(x2$nday_mat~x2$WSTA, xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Days to maturity", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

##to export the figure
dev.off()
##to export the figure

#####to export the figure as a file
dir<-OutputFile2
jpeg(dir,res=300, width=7, height=7, unit="in")
#####

#number of panels in a figure
par (mfrow=c(3,1),mar=c(0,4,0,1),oma=c(6,2,2,1),mgp = c(0,0.2, 0))
#number of panels in a figure

boxplot(x2$TMAXA~x2$WSTA, xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Seas. Tmax (°C)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

boxplot(x2$TMINA~x2$WSTA, xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Seas. Tmin (°C)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

boxplot(x2$PRCP~x2$WSTA, xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:12,labels=c("Base","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1"), tck=0.01,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Seas. prec (mm)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

##to export the figure
dev.off()
##to export the figure

