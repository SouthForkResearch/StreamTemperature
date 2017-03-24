##############################################
# shapefile output
# ##############################################
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
library(RColorBrewer)
library(classInt)

basin <- "Aso"
midBasin <- "Asotin"
longBasin <- "Asotin"
yrPath <- "14"
yearPath <- "2014"
subDir <- "EP_temp/"
dataPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"
mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"

seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
seis <- rev(seis)

setwd(paste0(dataPath, longBasin))

netname <- paste0(basin, "_STHD_net")
network <- readOGR(dsn=".", layer = netname)

ptsname <- paste0(basin, "_sites_rca")
pts <- readOGR(dsn=".", layer=ptsname)
pts@data <- pts@data[,c(4,6)]

setwd(paste0(mainPath, "/Summer_mean_models/"))

network <- spTransform(network, proj4string(pts))

#############repeat this part for each year################

setwd(paste0(mainPath, longBasin, "/", yearPath))

preds <- read.dbf(paste0("predt", yearPath, "_", basin, "_8D_Mn.dbf"))
summ_mn <- as.data.frame(apply(preds[,24:31], 1, mean))
colnames(summ_mn)[1] <- "Summ_mn"
summ_mn$RCAID <- preds$RCAID

#####################################
data.holder <- as.data.frame(preds$RCAID)
colnames(data.holder)[1] <- "RCAID"

data.holder <- cbind(data.holder, preds[,24:31])

multi_mean <- as.data.frame(apply(data.holder[,2:33], 1, mean))
colnames(multi_mean)[1] <- "11_14_summer_mn"

multi_mean$RCAID <- data.holder$RCAID
#######################################

netmerge <- merge(network, summ_mn, by.x='rca_id', by.y = 'RCAID')
netmerge@data$ReachCode <- as.character(netmerge@data$ReachCode)

# #### plot it to make sure it looks right ######### 


fix3 <- classIntervals(netmerge@data[,3], n = 11, style = "fixed",fixedBreaks=c(8,10,12,14,16,17,18,19,20,22,24))
fix3.colors <- findColours(fix3,pal=seis)
plot(netmerge, col=fix3.colors, bg="black", fg="white")


# #################################################  

setwd(paste0(mainPath,  "/Summer_mean_models/", yearPath))

layername <- paste0(basin, "_", yearPath, "_summer_mn")

writeOGR(netmerge, dsn=".", layer=layername, driver="ESRI Shapefile")

