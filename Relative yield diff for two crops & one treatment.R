###############################################################################################
#Cheryl Porter 9/20/2017
#script to compute the relative change in harvested yield for each location in USA
# One "farm" or "treatment", so relative yields are computed for each year to get distribution.
# Two crops in one figure
###############################################################################################

# package which creates pivot table: - (not used after all, but pretty nice tool)
#install.packages('reshape')
#library(reshape)

rm(list=ls(all=TRUE))

#setwd("C:/CGRA_data/US_data/Colorado")
#Location <- "Colorado"
#crop1 <- "maize"
#crop2 <- "wheat"

#setwd("C:/CGRA_data/US_data/Iowa")
#Location <- "Ames"
#crop1 <- "maize"
#crop2 <- "soybean"

setwd("C:/CGRA_data/US_data/Georgia")
Location <- "Camilla"
crop1 <- "peanut"
crop2 <- "soybean"
#crop1 <- "maize"
#crop2 <- "cotton"

csv_input_file1 <- paste("summary_",Location,"_",crop1,".csv", sep="")
csv_input_file2 <- paste("summary_",Location,"_",crop2,".csv", sep="")
OutputFile <- paste("Rel_yield_",Location,"_",crop1,"&",crop2,".jpg", sep="")
OutputFile2 <- paste("MedianRelYieldChange_",Location,"_",crop1,"&",crop2,".csv", sep="")

#####to export plot
dir<-OutputFile
jpeg(dir,res=300, width=5, height=6.5, unit="in")

#par (mfrow=c(2,1),mar=c(0,0.5,0,0),oma=c(4,3,1,1),mgp = c(3,1,0))
#                 plot margins     outer margins  title margins
#                      (b, l ,t,r)      (b,l,t,r)        (title, axis1, axis2)
par (mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,3,3,1),mgp = c(4,0.5,0))
######

# limits on y axis
ylim_low  <- -60
ylim_high <- 60

# location of "+1.5" and "+2.0" text labels
h1 <- 3
h2 <- 9
v1 <- 60

###############################################################################################
###############################################################################################
###############################################################################################
# Crop #1
x<-read.csv(csv_input_file1,header=TRUE)

#generate year for each simulation based on start of simulation date
x$year <- floor(x$SDAT / 1000)

#generate category for weather ("0"="baseline", "V"="+1.5" or "W"="+2.0")
x$WCat <- substring(x$WSTA,5,5)

## create a data frame containing only year, weather station and yield
#df_hwam <- data.frame(x$year, x$WSTA, x$HWAM)
#df_hwam <- setNames(df_hwam, c("year", "wsta", "hwam"))

# Pivot data to year x weather station table of yields
#hwam_raw <- cast(df_hwam, x.year ~ x.WSTA, mean, value = 'x.HWAM')

# give names to the columns using setNames
u_wsta <- as.character(unique(x$WSTA))
#setNames (hwam_raw, c("year", u_wsta))

u_year <- as.numeric(unique(x$year))
u_tnam <- as.character(unique(x$TNAM))
u_wcat <- as.character(unique(x$WCat))

###############################################################################################
#Calculating the change in absolute and relative yield per farm and weather stations
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

df_abs <- data.frame(x$WSTA, x$year, x$abs_HWAM, x$WCat)
df_abs <- setNames(df_abs, c("wsta","year","abs_hwam", "abs_wcat"))
df_rel <- data.frame(x$WSTA, x$year, x$rel_HWAM, x$WCat)
df_rel <- setNames(df_rel, c("wsta","year","rel_hwam", "rel_wcat"))

#omit lines with NA, which includes all 0XXX 
df_abs <- na.omit(df_abs)   
df_rel <- na.omit(df_rel)  

#get new list of unique weather stations, which now omit 0XXX
uabs_wsta <- as.character(unique(df_abs$wsta))
uabs_year <- as.numeric(unique(df_abs$year))
urel_wsta <- as.character(unique(df_rel$wsta))
urel_year <- as.numeric(unique(df_rel$year))

#save max and min ranges for median values
nwsta <- length(urel_wsta)
Medians <- data.frame(matrix(ncol = 3, nrow = nwsta+2))
Medians <- setNames(Medians, c("WSTA", crop1, crop2))
for (i in 1:nwsta){
  Medians$WSTA[i] <- urel_wsta[i]
  Medians[i,2] <- median(df_rel$rel_hwam[df_rel$wsta==urel_wsta[i]])
}

Medians[nwsta+1,1] <- "Maximum"
Medians[nwsta+1,2] <- max(Medians[1:nwsta,2])

Medians[nwsta+2,1] <- "Minimum"
Medians[nwsta+2,2] <- min(Medians[1:nwsta,2])

###############################################################################################
#Whisker plot for relative change per weather station in a specific location
###############################################################################################

a <- df_rel$rel_hwam
b <- as.character(df_rel$wsta)

boxplot(
  a ~ b, 
  ylim = c(ylim_low,ylim_high),
  xaxt="n",
  yaxt="n",
  outline=FALSE,
  las=1,
  at=c(1,2,3,4,5, 7,8,9,10,11), 
  col=c("green","red", "blue","yellow", "pink"))

axis(2, tck=0.01,las=1,cex.axis=1)
axis(4, tck=0.01,las=1,label=FALSE,cex.axis=1)#
abline(v=c(6),lty=3, col="black")
abline(h=c(0),lty=3, col="black")
mtext(crop1, side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)
###text for every panel
text(h1,v1, "+1.5 �C",cex=1)
text(h2,v1, "+2.0 �C",cex=1)
###########################

###############################################################################################
###############################################################################################
###############################################################################################
# Crop #2
x<-read.csv(csv_input_file2,header=TRUE)

#generate year for each simulation based on start of simulation date
x$year <- floor(x$SDAT / 1000)

#generate category for weather ("0"="baseline", "V"="+1.5" or "W"="+2.0")
x$WCat <- substring(x$WSTA,5,5)

# give names to the columns using setNames
u_wsta <- as.character(unique(x$WSTA))
#setNames (hwam_raw, c("year", u_wsta))

u_year <- as.numeric(unique(x$year))
u_tnam <- as.character(unique(x$TNAM))
u_wcat <- as.character(unique(x$WCat))

###############################################################################################
#Calculating the change in absolute and relative yield per farm and weather stations
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

df_abs <- data.frame(x$WSTA, x$year, x$abs_HWAM, x$WCat)
df_abs <- setNames(df_abs, c("wsta","year","abs_hwam", "abs_wcat"))
df_rel <- data.frame(x$WSTA, x$year, x$rel_HWAM, x$WCat)
df_rel <- setNames(df_rel, c("wsta","year","rel_hwam", "rel_wcat"))

#omit lines with NA, which includes all 0XXX 
df_abs <- na.omit(df_abs)   
df_rel <- na.omit(df_rel)  

#get new list of unique weather stations, which now omit 0XXX
uabs_wsta <- as.character(unique(df_abs$wsta))
uabs_year <- as.numeric(unique(df_abs$year))
urel_wsta <- as.character(unique(df_rel$wsta))
urel_year <- as.numeric(unique(df_rel$year))

#save max and min ranges for median values
nwsta <- length(urel_wsta)
for (i in 1:nwsta){
  Medians$WSTA[i] <- urel_wsta[i]
  Medians[i,3] <- median(df_rel$rel_hwam[df_rel$wsta==urel_wsta[i]])
}

Medians[nwsta+1,3] <- max(Medians[1:nwsta,3])
Medians[nwsta+2,3] <- min(Medians[1:nwsta,3])

write.csv(Medians, file =OutputFile2)

###############################################################################################
#Whisker plot for relative change per weater station in a specific location
###############################################################################################

a <- df_rel$rel_hwam
b <- as.character(df_rel$wsta)

boxplot(
  a ~ b, 
  ylim = c(ylim_low,ylim_high),
  xaxt="n",
  yaxt="n",
  outline=FALSE,
  las=1,
  at=c(1,2,3,4,5, 7,8,9,10,11), 
  col=c("green","red", "blue","yellow", "pink"))

axis(1, at=1:11,labels=c("CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M" ), tck=0.01,las=2,cex.axis=1)
axis(2, tck=0.01,las=1,cex.axis=1)
axis(4, tck=0.01,las=1,label=FALSE,cex.axis=1)#
abline(v=c(6),lty=3, col="black")
abline(h=c(0),lty=3, col="black")

mtext(crop2, side = 2, line = 2, outer = FALSE, at = NA,
adj = NA, padj = NA, cex =1, col = NA, font = NA)

# Overall title for plot
mtext("Relative yield change (%)", side = 3, outer = TRUE, cex = 1.5, line=0.5)

###text for every panel
text(h1,v1, "+1.5 �C",cex=1)
text(h2,v1, "+2.0 �C",cex=1)
###########################

###############################################################################################
###############################################################################################
###############################################################################################


#####
#to export plot
##### 
dev.off()
#####

