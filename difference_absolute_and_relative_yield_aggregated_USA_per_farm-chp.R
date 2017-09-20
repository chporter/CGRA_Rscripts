###############################################################################################
#Rubi Raymundo 07/12/2017
#script to compute the relative change in harvested yield for each location in USA
###############################################################################################

rm(list=ls(all=TRUE))
#
x<-read.csv("summary_Colorado_wheat.csv",header=TRUE)

#####to export plot
dir<-"Relative_yield_Colorado_wheat_F.jpeg"
jpeg(dir,res=300, width=6, height=3.5, unit="in")
######

par (mfrow=c(1,1),mar=c(0,0.5,0,0),oma=c(4,3,1,1),mgp = c(0,0.2, 0))


#for maize and soybean
######minimum and maximum relative change for "y axis"
ylim_n=-40
ylim_x=20
######################  

##########################################################
#HY: mean of harvested yield per farm and weather station 
##########################################################

HY<-aggregate(x$HWAM~x$TNAM+x$WSTA, data=x, mean, na.rm=TRUE)
HY1<-data.frame(HY[1],HY[2],HY[3])
HY1
HY2 <- setNames(HY1, c("TNAM","WSTA","HWAM"))
head(HY2)

u_wsta<-as.character(unique(HY2$WSTA))
u_wsta
u_tnam<-as.character(unique(HY2$TNAM))
u_tnam

abs_HWAM_0 = NULL
rel_HWAM_0 = NULL
ws_0= NULL
tnam_0=NULL
sce_0=NULL

###############################################################################################
#Calculating the change in absolute and relative yield per farm and weather stations
###############################################################################################

for (m in 2:length(u_wsta))
{
for (n in 1:length(u_tnam))
{
  abs_HWAM_1<-(HY2$HWAM[HY2$WSTA==u_wsta[m]& HY2$TNAM==u_tnam[n]])-(HY2$HWAM[HY2$WSTA==u_wsta[1]& HY2$TNAM==u_tnam[n]])
  abs_HWAM_0<-c(abs_HWAM_0,abs_HWAM_1)
  rel_HWAM_1<-abs_HWAM_1*100/(HY2$HWAM[HY2$WSTA==u_wsta[1]& HY2$TNAM==u_tnam[n]])
  rel_HWAM_0<-c(rel_HWAM_0,rel_HWAM_1)
  ws_1<-u_wsta[m]
  ws_0<-c(ws_0,ws_1)
  tnam_1<-u_tnam[n]
  tnam_0<-c(tnam_0,tnam_1)
  sce_1<-substring(u_wsta[m],5,5)
  sce_0<-c(sce_0,sce_1)
}
}
head(abs_HWAM_0)
head(rel_HWAM_0)
head(ws_0)
head(tnam_0)

###############################################################################################
#creating a dataframe to save results for absolute and relative change in harvested yield
###############################################################################################

df<-data.frame(sce_0,ws_0,tnam_0, abs_HWAM_0, rel_HWAM_0)
df1 <- setNames(df, c("temp_code","WSTA","tnam", "abs_HWAM", "rel_HWAM"))
head(df1)

temp_code<-as.character(unique(df1$temp_code))
temp_code

###############################################################################################
#Whisker plot for relative change per weater station in a specific location
###############################################################################################

boxplot(
  rel_HWAM~WSTA,
  data=df1, 
  ylim=c(ylim_n,ylim_x),
  xaxt="n",
  yaxt="n",
  las=1,
  at=c(1,2,3,4,5, 7,8,9,10,11),
  col=c("green","red", "blue","yellow", "pink"))

axis(1, at=1:11,labels=c("CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1", "","CanAM4","CAM4","HadAM3P"," MIROC5","NorESM1" ), tck=0.01,las=2,cex.axis=1)
#axis(1, at=1:11, tck=0.01,labels=FALSE,las=2,cex.axis=1)
axis(2, tck=0.01,las=1,cex.axis=1)
abline(v=c(6),lty=3, col="black")
#abline(v=c(4.5,8.5,12.5), lty=3, col="black")

###text for every panel
text(3,48, "+1.5 °C",cex=1)
text(9,48, "+2.0 °C",cex=1)
###########################

mtext("Relative change (%)", side = 2, line = 2, outer = FALSE, at = NA,
       adj = NA, padj = NA, cex =1, col = NA, font = NA)
#####
#to export plot
##### 
dev.off()
#####

