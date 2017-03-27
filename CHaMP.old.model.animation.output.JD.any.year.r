
##############
#This parts formats the error by day/site info
############

library(timeSeries)
library(lattice)
library(foreign)
library(doBy)
library(qpcR)
library(pls)
library(boot)
library(Hmisc)
library(readxl)
library(lubridate)
library(zoo)
library(car)
library(gvlma)
library(ggplot2)
library(rgdal)
library(raster)
library(RColorBrewer)
library(classInt)

basin <- "JD"
midBasin <- "JohnDay"
longBasin <- "John_Day"
yrPath <- "11"
yearPath <- "2011"
mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"
var <- "mn"

modelPath <- paste0(mainPath, yearPath, "_temp_CHaMP")
gisPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"

###########################################
# Animation output - no error points, full network
############################################

  netname <- paste0(basin, "_", yearPath, "_8D_", var)

  network <- readOGR(dsn=modelPath, layer = netname)

  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)
  
  names.out <- colnames(network@data[2:47])
  namesnum <- as.numeric(gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data[2:47])))
  means <- colMeans(network@data[2:47], na.rm = TRUE)
  SDs <- colStdevs(network@data[2:47], na.rm = TRUE)
  yplus <- means + SDs
  yminus <- means - SDs
  df <- data.frame(means=means, SDs=SDs, names=namesnum)
  sequ <- c(1:46)
  namer <- sprintf('%03d', sequ)
  
  fix4 <- classIntervals(means, n = 12, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18,20,22))
  fix4.colors <- findColours(fix4,pal=seis)
  
    for (i in 2:47)
      {
        
        namey <- gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data)[i])
        
        filename <- paste0(modelPath, "/graphics/", namer[i-1], ".png", sep="")
        png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="black")
        
        fix3 <- classIntervals(network@data[,i], n = 12, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18,20,22))
        fix3.colors <- findColours(fix3,pal=seis)
        
        plot(network, col=fix3.colors, bg="black", fg="white")
        
        legend("bottomright", fill = attr(fix3.colors, "palette"), legend = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", "20+"), bty = "n", cex=.5, inset=c(0,0), text.col="white");
        
        title(paste0(longBasin, " 8-day ", var, " ", yearPath, " (°C)"), line=-0.6, adj=1, col.main="white", col.sub="white", outer=FALSE, cex.main=0.5)
        mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-5, col="white", cex=.5)
        tmp2 <- subplot(
          plot(namesnum[1:(i-2)], means[1:(i-2)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,20), cex.main=.8, main="Basin mean", adj=0, xlab='',ylab='', col.lab="white", cex.axis=0.5, cex.lab = 0.25, col.axis="white", col.main = "white", bg="black"), 
          x=grconvertX(c(0.1,0.40), from='npc'), 
          y=grconvertY(c(0.1, 0.25), from='npc'),
          size=c(1,1.5), vadj=0.5, hadj=0.5, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
        
        dev.off()
      }


#####################
# This system call builds an mpeg out of the still graphics
#######################
  
setwd(paste0(modelPath, "/graphics/"))

  system('"C:/Program Files/ImageMagick-7.0.1-Q16/convert.exe" -delay 20 -morph 3 *.png JohnDay_2012_8D_Mn.mpeg')

###########################################
# Animation output - no error points, fish network
############################################
  
  netname <- paste0(basin, "_", yearPath, "_8D_", var)
  fishNetName <- paste0(basin, "_fishExtent_RCAs")
  fishNetPath <- paste0(gisPath, longBasin)
  
  fishRCAs <- read.csv(paste0(mainPath, midBasin, "/", basin, "_fishNet_RCAs.csv"), stringsAsFactors=FALSE)
  
  network <- readOGR(dsn=modelPath, layer = netname)
  fishNet <- readOGR(dsn=fishNetPath, layer = fishnetName)
  rcas <- unique(fishRCAs$RCAID)
  
  baseNet <- network
  network <- network[network$RCAID %in% rcas,]
  
  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)
  
  names.out <- colnames(network@data[2:47])
  namesnum <- as.numeric(gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data[2:47])))
  means <- colMeans(network@data[2:47], na.rm = TRUE)
  SDs <- colStdevs(network@data[2:47], na.rm = TRUE)
  yplus <- means + SDs
  yminus <- means - SDs
  df <- data.frame(means=means, SDs=SDs, names=namesnum)
  sequ <- c(1:46)
  namer <- sprintf('%03d', sequ)
  
  fix4 <- classIntervals(means, n = 12, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18,20,22))
  fix4.colors <- findColours(fix4,pal=seis)
  
  for (i in 2:47)
    {
      
      namey <- gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data)[i])
      
      filename <- paste0(modelPath, "/graphics/", namer[i-1], ".png", sep="")
      png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="black")
      
      fix3 <- classIntervals(network@data[,i], n = 12, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18,20,22))
      fix3.colors <- findColours(fix3,pal=seis)
      
      
      plot(network, col=fix3.colors, bg="black", fg="white")
      
      legend("topright", fill = attr(fix3.colors, "palette"), legend = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", "20-22", "22+"), bty = "n", cex=.5, inset=c(0,0.1), text.col="white");
      
      title(paste0(longBasin, " 8-day ", var, " ", yearPath, " (°C)"), line=-0.6, adj=1, col.main="white", col.sub="white", outer=FALSE, cex.main=0.5)
      mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-5, col="white", cex=.5)
      
      tmp2 <- subplot(
        plot(namesnum[1:(i-1)], means[1:(i-1)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,20), cex.main=.8, main="Basin mean", adj=0, xlab='',ylab='', col.lab="white", cex.axis=0.5, cex.lab = 0.25, col.axis="white", col.main = "white", bg="black"), 
        x=grconvertX(c(0,0.30), from='npc'), 
        y=grconvertY(c(0, 0.20), from='npc'),
        size=c(1,1.0), vadj=0.2, hadj=0.5, 
        pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
      
      dev.off()
    }
  
  
###########################################
# Animation output - with error points
############################################

seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
seis <- rev(seis)

names.out <- colnames(network@data[3:48])
namesnum <- as.numeric(gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data[3:48])))
means <- colMeans(network@data[3:48], na.rm = TRUE)
SDs <- colStdevs(network@data[3:48], na.rm = TRUE)
yplus <- means + SDs
yminus <- means - SDs
df <- data.frame(means=means, SDs=SDs, names=namesnum)
sequ <- c(1:46)
namer <- sprintf('%03d', sequ)
fix4 <- classIntervals(means, n = 10, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18))
fix4.colors <- findColours(fix4,pal=seis)

for (i in 3:48)
{
  
  namey <- gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data)[i])
  
  filename <- paste0(modelPath, "/graphics/", namer[i-2], ".png", sep="")
  png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="black")
  
  fix3 <- classIntervals(network@data[,i], n = 11, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18, 20))
  fix3.colors <- findColours(fix3,pal=seis)
  
  cexEr <-ifelse(abs(error_pts@data[,i]) == 0, 0,
                 ifelse(abs(error_pts@data[,i])>0 & error_pts@data[,i-1]<1, 0.5,
                        ifelse(abs(error_pts@data[,i])>1 & error_pts@data[,i-1]<2, 0.75,
                               ifelse(abs(error_pts@data[,i])>2 & error_pts@data[,i]<3, 1.0,
                                      ifelse(abs(error_pts@data[,i])>3, 1.25,
                                             ifelse(abs(error_pts@data[,i])== NA, 0, NA))))))
  
  plot(network, col=fix3.colors, bg="black", fg="white")
  points(error_pts, pch=16, col="gray40", cex=cexEr)
  
  legend("topleft", fill = attr(fix3.colors, "palette"), legend = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18+"), bty = "n", cex=.5, inset=c(.05,0.1), text.col="white");
  legend("topright", pch=16, col="gray40", pt.cex=c(0.5, 0.75, 1.0, 1.25), title="Model error (°C)", legend = c("0-1","1-2","2-3","3+"), bty = "n", cex=.5, inset=c(0.05,0.5), text.col="white");
  
  
  title(paste0(longBasin, " 8-day ", var, " ", yearPath, " (°C)"), line=-0.6, adj=0, col.main="white", col.sub="white", outer=FALSE, cex.main=0.5)
  mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-5, col="white", cex=.5)
  tmp2 <- subplot(
    plot(namesnum[1:(i-2)], means[1:(i-2)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,20), cex.main=.8, main="Basin mean", adj=0, xlab='',ylab='', col.lab="white", cex.axis=0.5, cex.lab = 0.25, col.axis="white", col.main = "white", bg="black"), 
    x=grconvertX(c(0.1,0.45), from='npc'), 
    y=grconvertY(c(0.1, 0.25), from='npc'),
    size=c(1,1.5), vadj=0.5, hadj=0.5, 
    pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
  
  dev.off()
}


########
# This system call builds an mpeg out of the still graphics
########
setwd(paste0(modelPath, "/graphics/"))

system('"C:/Program Files/ImageMagick-7.0.1-Q16/convert.exe" -delay 20 -morph 3 *.png JD_2012_8D_Mn.mpeg')
########
#Still graphic output for wiki or web or wev
#######
i <- 22  
namey <- gsub(paste0("T", var, "_", yrPath, "_"), "", colnames(network@data)[i])
fix3 <- classIntervals(network@data[,i], n = 11, style = "fixed",fixedBreaks=c(-1,6,8,10,12,14,16,18,20,22,24))
fix3.colors <- findColours(fix3,pal=seis)

filename <- paste0(modelPath, "/graphics/Stills/", basin, "_", yearPath, "_8D_", var, ".png", sep="")
png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="white")

cexEr <- ifelse(abs(error_pts@data[,i]) <= 1, 0.5,
                ifelse(abs(error_pts@data[,i])>1, 0.75,
                       ifelse(abs(error_pts@data[,i])>2, 1.0,
                              ifelse(abs(error_pts@data[,i])>3, 1.25, NA))))

plot(network, col=fix3.colors, bg="white", fg="black")
points(error_pts, pch=16, col="gray40", cex=cexEr)

legend("topright", fill = attr(fix3.colors, "palette"), title=paste0("8-day ", var, " (°C)"),legend = c("0-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", "20-22", "22+"), bty = "n", cex=.5, inset=c(0,0), text.col="black");
legend("bottomright", pch=16, col="gray40", pt.cex=c(0.5, 0.75, 1.0, 1.25), title="Model error (°C)", legend = c("0-1","1-2","2-3","3+"), bty = "n", cex=.5, inset=c(0,0), text.col="black");

mtext(paste0(longBasin, " ",  yearPath, " 8-day ", var, " (°C)"),side =3, outer=TRUE, line=-3, col="black", cex=.7)
mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-3, col="black", cex=.7)

tmp2 <- subplot(
  plot(namesnum[1:(i-2)], means[1:(i-2)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,22), cex.main=.8, main="Basin mean", adj=0, xlab='',ylab='', col.lab="black", cex.axis=0.5, cex.lab = 0.25, col.axis="black", col.main = "black", bg="white"), 
  x=grconvertX(c(0.0,0.25), from='npc'), 
  y=grconvertY(c(0.80,0.95), from='npc'),
  size=c(1,1.5), vadj=0.5, hadj=0.5, 
  pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
```

