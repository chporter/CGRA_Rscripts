#Script to display whiskert plots for one farm 
#variables:
# - Harvested yield (kg/ha)
# - Total biomass (kg/ha)
# - Seasonal Tmax (°C)
# - Seasonal Tmin (°C)
# - Seasonal prec (°C)
#graph observed vs simulated
rm(list=ls(all=TRUE))

setwd("C:/CGRA_data/US_data/Georgia/GenericCotton")
Location <- "Camilla"
Crop <- "cotton"
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")

OutputFile <- paste("B&W_",Location,"_",Crop,"_All_Trt_yield.jpg", sep="")
DataOut <- paste("Quartiles_",Location,"_",Crop,".csv", sep="")

#reading table
x<-read.csv(csv_input_file,header=TRUE)

###################################

####size of x-axis and y-axis labels
cex=1.2
cex_lab=1.2
###################################

#####to export the figure as a file
dir<-OutputFile
jpeg(dir,res=300, width=8, height=11, unit="in")
#####

#number of panels in a figure
par (mfrow=c(3,2),mar=c(0,0,0,0),oma=c(6,6,4,1),mgp = c(0,0.2, 0))
#number of panels in a figure

#############################
# weather station codes
uWsta <- unique(x$WSTA)

# trim white space from TNAM
x$TNAM <- trimws(x$TNAM)

# treatment / farm codes
uTnam <- unique(x$TNAM)

tnam <- "All"
ClimID <- "All"
# data frame to hold quartiles to output
Qs <- quantile(x$HWAM)
Qdf <- data.frame(Crop, tnam, ClimID, as.numeric(Qs[1]), as.numeric(Qs[2]), as.numeric(Qs[3]), as.numeric(Qs[4]), as.numeric(Qs[5]))
Qdf <- setNames(Qdf, c("Crop","Treatment","ClimID","Min","Q1","Median","Q3","Max"))

ylim1 <- as.numeric(min(x$HWAM))
ylim2 <- as.numeric(max(x$HWAM))

###############################################################################################
# loop thru treatments
for (n in (1:length(uTnam))){
  
  tnam <- as.character(uTnam[n])
  
  x2 <- subset(x, TNAM == tnam, select=c(TNAM, WSTA, PDAT, ADAT, MDAT, HWAM))
  
  Qs <- quantile(x2$HWAM)
  ClimID <- "All"
  Qdf1 <- data.frame(Crop, tnam, ClimID, as.numeric(Qs[1]), as.numeric(Qs[2]), as.numeric(Qs[3]), as.numeric(Qs[4]), as.numeric(Qs[5]))
  Qdf1 <- setNames(Qdf1, c("Crop","Treatment","ClimID","Min","Q1","Median","Q3","Max"))
  Qdf <- rbind(Qdf, Qdf1)
  
  ###############################################################################################
  # loop thru climate IDs
  for (i in (1:length(uWsta))){
    ClimID <- as.character(uWsta[i])
    Qs <- quantile(x2$HWAM[x2$WSTA==ClimID])
    Qdf1 <- data.frame(Crop, tnam, ClimID, as.numeric(Qs[1]), as.numeric(Qs[2]), as.numeric(Qs[3]), as.numeric(Qs[4]), as.numeric(Qs[5]))
    Qdf1 <- setNames(Qdf1, c("Crop","Treatment","ClimID","Min","Q1","Median","Q3","Max"))
    Qdf <- rbind(Qdf, Qdf1)
    
    if (i ==1){
      median_0XXX <- Qs[3]
    }
  }  
  
  boxplot(
    x2$HWAM~x2$WSTA, 
    xaxt="n",
    yaxt="n",
    ylim=c(0,ylim2+500),
    las=1,
    at=c(1, 3,4,5,6,7,  9,10,11,12,13), 
    col=c("gray", "green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink")) 
  
  axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
  axis(2, tck=0.01, labels=FALSE, las=1, cex.axis=cex)
  
  #  abline(v=c(2),lty=3, col="black")
  #  abline(v=c(8),lty=3, col="black")
  abline(h=c(median_0XXX),lty=3, col="black")
  
  
  #mtext(tnam, side = 3, line = -1, outer = FALSE, at = c(0,6),
  mtext(tnam, side = 3, line=-1.5, outer = FALSE, at = 7,
        adj = NA, padj = NA, cex =1, col = NA, font = NA)
  
  
  if (n == 1 | n ==3 | n == 5){
    axis(side=2, cex.axis=cex)
  }
  
  if (n == 5 | n ==6){
    axis(1, at=1:13,
         labels=c("Baseline","", "CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
    mtext("+1.5", side = 1, line=-1, outer = FALSE, at = 5,
          adj = NA, padj = NA, cex =1, col = NA, font = NA)
    mtext("+2.0", side = 1, line=-1, outer = FALSE, at = 11,
          adj = NA, padj = NA, cex =1, col = NA, font = NA)
  }
  
}
# end loops
##############################################################################

mtext("Harvested yield (kg/ha)", side = 2, line = 3, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

mtext(Crop, side = 3, line = 1, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

#mtext("+1.5", side = 3, line=1, outer = TRUE, at = 0.15,
#        adj = NA, padj = NA, cex =1, col = NA, font = 2)
#mtext("+2.0", side = 3, line=1, outer = TRUE, at = 0.37,
#        adj = NA, padj = NA, cex =1, col = NA, font = 2)
#
#mtext("+1.5", side = 3, line=1, outer = TRUE, at = 0.65,
#      adj = NA, padj = NA, cex =1, col = NA, font = 2)
#mtext("+2.0", side = 3, line=1, outer = TRUE, at = 0.85,
#      adj = NA, padj = NA, cex =1, col = NA, font = 2)

##to export the figure
dev.off()
##to export the figure

write.csv(Qdf, file=DataOut)

