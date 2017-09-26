###############################################################################################
#Cheryl Porter 9/25/2017
#script to compute the relative change in harvested yield for one location, multiple treatments in USA
# Multiple treatments, one diagram for each.
# One crop 
###############################################################################################
#Script to display whiskert plots for one farm 
rm(list=ls(all=TRUE))

setwd("C:/CGRA_data/US_data/Georgia/GenericMaize")
Location <- "Camilla"
Crop <- "maize"
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")

OutputFile <- paste("B&W_",      Location,"_",Crop,"_Relyield.jpg", sep="")
OutputFile2<- paste("MedianRelYieldChange_",Location,"_",Crop,".csv", sep="")

#############################
#reading data from file
x<-read.csv(csv_input_file,header=TRUE)

#generate year for each simulation based on start of simulation date
x$year <- floor(x$SDAT / 1000)

# weather station codes
u_wsta <- unique(x$WSTA)

# years
u_year <- as.numeric(unique(x$year))

# trim white space from TNAM
x$TNAM <- trimws(x$TNAM)
# treatment / farm codes
u_tnam <- unique(x$TNAM)

###############################################################################################
# create variables for absolute and relative change in harvested yield
###############################################################################################
for (m in 2:length(u_wsta))
{
  for (n in 1:length(u_year))
  {
    hwam0 <- x$HWAM[x$WSTA==u_wsta[1]& x$year==u_year[n]]
    
    abs_HWAM <- (x$HWAM[x$WSTA==u_wsta[m]& x$year==u_year[n]]) - hwam0
    x$abs_HWAM[x$WSTA==u_wsta[m]& x$year==u_year[n]] <- abs_HWAM
    
    rel_HWAM <- abs_HWAM*100 / hwam0
    x$rel_HWAM[x$WSTA==u_wsta[m]& x$year==u_year[n]] <- rel_HWAM
  }
}

# set all NaN and Inf values to NA
x$rel_HWAM[is.infinite(x$rel_HWAM)] <- NA
x$rel_HWAM[is.na(x$rel_HWAM)] <- NA

###############################################################################################
#creating a dataframe to save results for absolute and relative change in harvested yield
###############################################################################################

df_abs <- data.frame(x$TNAM, x$WSTA, x$year, x$abs_HWAM)
df_abs <- setNames(df_abs, c("TNAM","WSTA","year","abs_HWAM"))
df_rel <- data.frame(x$TNAM, x$WSTA, x$year, x$rel_HWAM)
df_rel <- setNames(df_rel, c("TNAM","WSTA","year","rel_HWAM"))

#omit lines with NA, which includes all 0XXX 
df_abs <- na.omit(df_abs)   
df_rel <- na.omit(df_rel)  

#get new list of unique weather stations, which now omit 0XXX
urel_wsta <- as.character(unique(df_rel$WSTA))
urel_year <- as.numeric(unique(df_rel$year))

#save median values
nwsta <- as.numeric(length(urel_wsta))
ntrts <- as.numeric(length(u_tnam))

Medians <- data.frame(matrix(ncol = 3))
Medians <- setNames(Medians, c("WSTA", "TrtName", "Median"))
for (i in 1:nwsta){
  for (j in 1: ntrts){
#    Medians[i,j+1] <- median(df_rel$rel_HWAM[df_rel$WSTA == urel_wsta[i] & df_rel$TNAM == u_tnam[j]])
    NewMedian <- median(df_rel$rel_HWAM[df_rel$WSTA == urel_wsta[i] & df_rel$TNAM == u_tnam[j]])
    Medians <- rbind(Medians, c(urel_wsta[i], u_tnam[j], NewMedian))
  }
}

write.csv(Medians, file =OutputFile2)

###################################
#####to export the figure as a file
dir<-OutputFile
jpeg(dir,res=300, width=8, height=11, unit="in")
#####
#number of panels in a figure
par (mfrow=c(3,2),mar=c(0,0,0,0),oma=c(6,6,4,1),mgp = c(0,0.2, 0))
#number of panels in a figure
####size of x-axis and y-axis labels
cex=1.2
cex_lab=1.2
#### limits of y axis
ylim1 <- as.numeric(min(df_rel$rel_HWAM))
ylim2 <- as.numeric(max(df_rel$rel_HWAM))
###################################

###############################################################################################
# loop thru treatments
for (n in (1:length(u_tnam))){
  
  tnam <- as.character(u_tnam[n])
  print(c(n,tnam))
  
  x2 <- subset(df_rel, TNAM == tnam, select=c(TNAM, WSTA, rel_HWAM))
  
  a <- x2$rel_HWAM
  b <- as.character(x2$WSTA)
  
  boxplot(
    a ~ b, 
    xaxt="n",
    yaxt="n",
    ylim=c(ylim1,ylim2),
    las=1,
    at=c(1,2,3,4,5, 7,8,9,10,11), 
    col=c("green","red", "blue","yellow", "pink", "green","red", "blue","yellow", "pink"))
  
  axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
  axis(2, tck=0.01, labels=FALSE, las=1, cex.axis=cex)
  abline(h=c(0),lty=3, col="black")
  
  mtext(tnam, side = 3, line=-1.5, outer = FALSE, at = 6,
        adj = NA, padj = NA, cex =1, col = NA, font = NA)
  
  if (n == 1 | n ==3 | n == 5){
    axis(side=2, cex.axis=cex)
  }
  
  if (n == 5 | n ==6){
    axis(1, at=1:11,
         labels=c("CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
    mtext("+1.5", side = 1, line=-1, outer = FALSE, at = 3,
          adj = NA, padj = NA, cex =1, col = NA, font = NA)
    mtext("+2.0", side = 1, line=-1, outer = FALSE, at = 9,
          adj = NA, padj = NA, cex =1, col = NA, font = NA)
  }
}
# end loops
##############################################################################

mtext("Relative Yield Change (%)", side = 2, line = 3, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)


mtext(Crop, side = 3, line = 1, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)

##to export the figure
dev.off()
##to export the figure

##############################################################################
##############################################################################


