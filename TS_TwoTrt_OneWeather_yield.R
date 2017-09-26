##time series of harvested yield over 30 years (United states)
#graph observed vs simulated
rm(list=ls(all=TRUE))

setwd("C:/CGRA_data/US_data/Georgia/GenericCotton")
Location <- "Camilla"
Crop <- "cotton"
trtno1 <- 5
trtno2 <- 6
ID_CLI="0XXX"

#Names of files are constructed from location, crop, climate, and treatment number
csv_input_file <- paste("summary_",Location,"_",Crop,".csv", sep="")
OutputFile1 <- paste("TS_",Location,"_",Crop,"_",ID_CLI,"_yield.jpg", sep="")

x<-read.csv(csv_input_file,header=TRUE)

x2<-data.frame(x$TNAM, x$WSTA, substring(x$WSTA,5,8),x$PDAT, x$HWAM)
x2<-setNames(x2, c("TNAM","WSTA","ID_CLIM","PDAT","HWAM"))
x2$year <- floor(as.numeric(x2$PDAT)/1000)

a<-unique(x2$ID_CLIM)
b<-unique(x2$TNAM)

tnam1 <- as.character(b[trtno1])
tnam2 <- as.character(b[trtno2])

#####
hwam1 <- as.numeric(x2$HWAM[x2$TNAM==tnam1 & x2$ID_CLIM==ID_CLI])
year1 <- as.numeric(x2$year[x2$TNAM==tnam1 & x2$ID_CLIM==ID_CLI])

hwam2 <- as.numeric(x2$HWAM[x2$TNAM==tnam2 & x2$ID_CLIM==ID_CLI])
year2 <- as.numeric(x2$year[x2$TNAM==tnam2 & x2$ID_CLIM==ID_CLI])

#############################################################################################
#############################################################################################
##export to file
dir <- OutputFile1
jpeg(dir,res=300, width=6, height=4, unit="in")
par (mfrow=c(1,1),mar=c(0,0,0,0),oma=c(3,4,1,1),mgp = c(0,0.2, 0))
cex_axis=1

#####################################################################################
# plot hwam1 vs year1 
plot(year1, hwam1 , 
     pch= 19, col="blue", las=1, ylab="",xlab="",axes=F,
     ylim = c(min(hwam1-250,hwam2-250),max(hwam1+500,hwam2+500)),
     xlim = c(min(year1,year2),max(year1,year2)))

# add points for treatment 2
points(year2,hwam2, pch=19, col="red")

# add trendlines
abline(lm(hwam1 ~ year1), col="blue")
abline(lm(hwam2 ~ year2), col="red")

legend("bottomright", legend=c("irrigated","rainfed"), pch=c(19,19), col = c("red","blue"), horiz=TRUE)
box()

axis(side = 1, tck = -0.01, labels= TRUE,cex.axis=cex_axis)
axis(side = 2, las = 1, tck = -0.01, labels=TRUE,cex.axis=cex_axis)
axis(side = 4, las = 1, tck = 0.01, labels=FALSE,cex.axis=cex_axis)

mtext("Year", side = 1, line = 1.5, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)
mtext("Harvested yield (kg/ha)", side = 2, line = 2.5, outer = TRUE, at = NA,
      adj = NA, padj = NA, cex =1, col = NA, font = NA)

##### 
dev.off()
#####