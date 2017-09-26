#Script to display whiskert plots for a random farm 
#variables:
# - Harvested yield (kg/ha)
# - Total biomass (kg/ha)
# - Seasonal Tmax (°C)
# - Seasonal Tmin (°C)
# - Seasonal prec (°C)
#graph observed vs simulated
rm(list=ls(all=TRUE))

#reading table
#x<-read.csv("D:/2017/CGRA data/from_cheryl/Colorado/summary_Maize/Summary_Maize_CM2.csv",header=TRUE)
#x<-read.csv("D:/2017/CGRA data/from_cheryl/Colorado/summary_Wheat/Summary_Wheat_CM2.csv",header=TRUE)
x<-read.csv("D:/2017/CGRA data/from_cheryl/Ames/summary_Soybean/Summary_Soybean_CM2.csv",header=TRUE)


##weather station code
b<-unique(x$WSTA)
b

##farm code
c<-unique(x$TNAM)
c

#######select select a random farm 
#### random farm for maize
#tnam<-"Iowa maize - soil 1"###for Ames maize
tnam<-"Clarion Loam"###for Ames
#tnam<-"Generic"         ###for Colorado
#tnam<-"PT normal residue"         ###for Colorado###highly important

tnam
#### random farm for soybean
#tnam<-"UABN0001"
###################################

####size of x-axis and y-axis labels
cex=1.3
###################################


biomass=x$CWAM[x$TNAM==tnam]+x$HWAM[x$TNAM==tnam]

#####to export the figure as a file
dir<-"D:/2017/CGRA data/mine/graph/USA/random_farm_ames_soybean_CM2.jpeg"
jpeg(dir,res=300, width=7, height=11, unit="in")
#####


#number of panels in a figure
par (mfrow=c(5,1),mar=c(0,4,0,1),oma=c(6,2,2,1),mgp = c(0,0.2, 0))
#number of panels in a figure

#a=x$HWAM[x$TNAM==tnam & x$WSTA=="SNNRV4XF"]
#a

#####original without edition
#boxplot(biomass~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(2, tck=0.01,las=1,cex.axis=1)
#############################

boxplot(x$HWAM[x$TNAM==tnam]~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12), col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)#AN00021
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Harvested yield (kg/ha)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

boxplot(x$CWAM~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Total biomass (kg/ha)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

boxplot(x$TMAXA[x$TNAM==tnam]~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Seasonal Tmax (°C)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

boxplot(x$TMINA[x$TNAM==tnam]~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Seasonal Tmin (°C)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

boxplot(x$PRCP[x$TNAM==tnam]~x$WSTA[x$TNAM==tnam], xaxt="n",yaxt="n",las=1,las=1,at=c(1,2,3,4,5,6, 8,9,10,11,12),col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))#, outline=F,las=1,xaxt="n",yaxt="n")
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)
axis(1, at=1:12,labels=c("Base","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=cex)
abline(v=c(7),lty=3, col="black")
mtext("Seasonal prec (mm)", side = 2, line = 4, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

##to export the figure
dev.off()
##to export the figure
