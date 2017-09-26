###############################################################################################
#Cheryl Porter 9/20/2017
#script to compute the relative change in harvested yield for one farm
# One "farm" or "treatment", so relative yields are computed for each year to get distribution.
###############################################################################################

# package which creates pivot table: - (not used after all, but pretty nice tool)
#install.packages('reshape')
#library(reshape)

rm(list=ls(all=TRUE))

x<-read.csv("summary_Colorado_maize.csv",header=TRUE)

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

# this:
#hwam_raw$USGRV4XF
# is equivalent to this:
#n <- 2
#u_wsta[n]
#hwam_raw[u_wsta[n]]

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

#df<-data.frame(sce_0,ws_0, abs_HWAM_0, rel_HWAM_0)
#df1 <- setNames(df, c("temp_code","WSTA", "abs_HWAM", "rel_HWAM"))
#head(df1,30)

#temp_code<-as.character(unique(df1$temp_code))
#temp_code


#
#####to export plot
dir<-"Relative_yield_Colorado_maize-C.jpeg"
jpeg(dir,res=300, width=5, height=3.5, unit="in")

#par (mfrow=c(1,1),mar=c(0,0.5,0,0),oma=c(4,3,1,1),mgp = c(0,0.2, 0))
#                      (b, l ,t,r)       
######

###############################################################################################
#Whisker plot for relative change per weater station in a specific location
###############################################################################################

#boxplot(
#  x$rel_HWAM~x$WSTA,
#  data=df1, 
#  ylim=c(ylim_n,ylim_x),
#  xaxt="n",
#  yaxt="n",
#  las=1,
#  at=c(1,2,3,4,5, 7,8,9,10,11),
#  col=c("green","red", "blue","yellow", "pink"))
#axis(1, at=1:11,labels=c("CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M" ), tck=0.01,las=2,cex.axis=1)
##axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=1)
#axis(2, tck=0.01,las=1,cex.axis=1)
#axis(4, tck=0.01,las=1,label=FALSE,cex.axis=1)#

#abline(v=c(6),lty=3, col="black")
##abline(v=c(4.5,8.5,12.5), lty=3, col="black")

###text for every panel
#text(3,58, "+1.5 °C",cex=1)
#text(9,58, "+2.0 °C",cex=1)
###########################
#write.csv(df_rel, "Temp.csv")

a <- df_rel$rel_hwam
b <- as.character(df_rel$wsta)


boxplot(
  a ~ b, 
  ylim = c(-60,60),
  xaxt="n",
  yaxt="n",
  las=1,
  at=c(1,2,3,4,5, 7,8,9,10,11), 
  col=c("green","red", "blue","yellow", "pink"))


#outline=F
axis(1, at=1:11,labels=c("CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M" ), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:11,labels=c("OXFX","V4XF","V5XF","V8XF","VOXF","VTXF","W4XF","W5XF","W8XF","WOXF","WTXF"), tck=0.01,las=2,cex.axis=1)#AN00021
#axis(1, at=1:12,labels=c("Base","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M", "","CanAM4","CAM4-2deg","HadAM3P"," MIROC5","NorESM1-M"), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=cex)
#axis(2, tck=0.01,las=1,cex.axis=cex)
axis(2, tck=0.01,las=1,cex.axis=1)
axis(4, tck=0.01,las=1,label=FALSE,cex.axis=1)#
abline(v=c(6),lty=3, col="black")
#mtext("Harv. yield (kg/ha)", side = 2, line = 4, outer = FALSE, at = NA,
#      adj = NA, padj = NA, cex =cex_lab, col = NA, font = NA)


mtext("Yield Relative change Yield (%)", side = 2, line = 2, outer = FALSE, at = NA,
adj = NA, padj = NA, cex =1, col = NA, font = NA)

#####
#to export plot
##### 
dev.off()
#####

