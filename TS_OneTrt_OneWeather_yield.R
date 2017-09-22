##time series of harvested yield over 30 years (United states)
#graph observed vs simulated
rm(list=ls(all=TRUE))

Location <- "Camilla"
Crop <- "cotton"
trtno <- 6
ID_CLI="0XXX"

#Names of files are constructed from location, crop, climate, and treatment number
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")
OutputFile1 <- paste("TS_",Location,"_",Crop,"_Trt",trtno,"_",ID_CLI,"_yield.jpg", sep="")

x<-read.csv(csv_input_file,header=TRUE)

##export to file
dir <- OutputFile1
jpeg(dir,res=300, width=6, height=4, unit="in")
par (mfrow=c(1,1),mar=c(0,0,0,0),oma=c(3,4,1,1),mgp = c(0,0.2, 0))
cex_axis=1

x2<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),substring(x$HDAT,1,4), x$HWAM)
x2<-setNames(x2, c("TNAM","WSTA","ID_CLIM","year","HWAM"))

a<-unique(x2$ID_CLIM)
b<-unique(x2$TNAM)

tnam <- as.character(b[trtno])

#####
year_crop1 <- as.numeric(x2$year[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])
hwam_crop1 <- as.numeric(x2$HWAM[x2$TNAM==tnam & x2$ID_CLIM==ID_CLI])

plot((c(1980:2009)), hwam_crop1 , pch= 19, las=1, ylab="",xlab="",axes=F)

box()
axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

abline(lm(hwam_crop1 ~ (c(1980:2009))), lty=3,lwd=0.75)

summary(lm(hwam_crop1 ~ (c(1980:2009))))

mtext("Harvested yield (kg/ha)", side = 2, line = 3, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

##### 
dev.off()
#####