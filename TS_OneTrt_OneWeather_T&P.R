##time series of seasonal tmin, tmax and prec over 30 years (United states)
#graph observed vs simulate

rm(list=ls(all=TRUE))

Location <- "Camilla"
Crop <- "cotton"
trtno <- 6
ID_CLI="0XXX"

#Names of files are constructed from location, crop, climate, and treatment number
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")
OutputFile1 <- paste("TS_",Location,"_",Crop,"_Trt",trtno,"_",ID_CLI,"_T&P.jpg", sep="")

x<-read.csv(csv_input_file,header=TRUE)

##export to file
dir <- OutputFile1
jpeg(dir,res=300, width=4.5, height=8, unit="in")
par (mfrow=c(3,1),mar=c(4,0,0,0),oma=c(1,4,1,1),mgp = c(0,0.2, 0))

x2<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),substring(x$HDAT,1,4),x$HWAM,x$TMAXA,x$TMINA,x$PRCP)
x2<-setNames(x2, c("TNAM","WSTA","ID_CLIM","year","HWAM","TMAXA","TMINA","PRCP"))

a<-unique(x2$ID_CLIM)
b<-unique(x2$TNAM)

tnam <- as.character(b[trtno])

cex_axis=1.2
cex_nam=1.4
ylim_n=5
ylim_x=32

year_crop1<-as.numeric(x2$year[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI ])
tmax<-as.numeric(x2$TMAX[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
tmin<-as.numeric(x2$TMIN[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
prec<-as.numeric(x2$PRCP[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])

#### Maximum temperature plot ####
plot((c(1980:2009)), tmax , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)
abline(lm(tmax ~ c(1980:2009)), lty=3,lwd=0.75)

mtext("Max Temperature (°C)", side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

#### Minimum temperature plot ####
plot((c(1980:2009)), tmin , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)
abline(lm(tmin ~ c(1980:2009)), lty=3,lwd=0.75)

mtext("Min Temperature (°C)", side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

#### Rainfall plot ####
plot((c(1980:2009)), prec , pch= 19, las=1, ylab="",xlab="",axes=F)
box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)
abline(lm(prec ~ c(1980:2009)), lty=3,lwd=0.75)

mtext("Rain (mm)", side = 2, line = 2, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)


##### 
dev.off()
#####

