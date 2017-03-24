############################################################################################################
# This set of R scripts processes winter min temp model shapefiles for reprojection
# Created: 25 Oct 2016
# Used for all watersheds and all years by changing the basin names and year paths 
          

##############################################################################################

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

  basin <- "Ent"
  midBasin <- "Entiat"
  longBasin <- "Entiat"
  yrPath1 <- "14"
  yrPath2 <- "15"
  dataPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"
  mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"
  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)
  
  projname <- paste0(longBasin, "_net_rca")
  
  setwd(paste0(dataPath, longBasin))
  
  proj_layer <- readOGR(dsn=".", layer = projname)
  
  
  netname <- paste0(basin, "_STHD_net")
  network <- readOGR(dsn=".", layer = netname)
  network <- spTransform(network, proj4string(proj_layer))
  
### change years and repeat this part #######
  
  setwd(paste0(mainPath, longBasin, "/Min_models/", yrPath1, "_", yrPath2, "_Min/"))
  
  preds <- read.dbf(paste0("predt_", yrPath1, "_", yrPath2, "_", basin, "_8D_Min.dbf"))
  netmerge <- merge(network, preds, by.x='RCAID', by.y = 'RCAID')

  
# #### plot it to make sure it looks right ######### 
  
  
  fix3 <- classIntervals(netmerge@data[,10], n = 11, style = "fixed",fixedBreaks=c(-1,0,1,2,3,4,6,8,10,12,14))
  fix3.colors <- findColours(fix3,pal=seis)
  plot(netmerge, col=fix3.colors, bg="black", fg="white")
  
# #################################################  
  
  setwd(paste0(mainPath, "Min_temp_models/"))
  layername <- paste0(basin, "_", yrPath1, "_", yrPath2, "_8D_winter_min")
  writeOGR(netmerge, dsn=".", layer=layername, driver="ESRI Shapefile")
  
  
  