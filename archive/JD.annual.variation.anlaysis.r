############################################################################################################
# This set of R scripts combines a the series of scripts that make up the work flow to predict and validate 
# stream temperature for annual 8-day means from MODIS 1km LST data
# The initial input is an LST_YY.dbf which is a table (columns = days, rows = sites) with the grid value for each grid cell in the spatial extent
# 
# Edited Aug 2014 to add the PRESS statistic

# Edited Jan 2016 to update the gap-filling interpolation functions
# Edited April 2016 to add model quality testing and pull hard-coded file paths out of the code.

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
library(TeachingDemos)
library(calibrate)
library(rgdal)
library(raster)
library(rNOMADS)
library(gdalUtils)


  basin <- "JD"
  midBasin <- "JohnDay"
  longBasin <- "JohnDay"
  dataType <- "Temp"
  yrPath <- "00"
  monthPath <- "01"
  yearPath <- "2000"
  dataPath <- "D:/OneDrive/work/GIS/PRISM/"
  mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/Climate_analysis/"
    
           
#################################################################
# Data investigation 

# ################################################################
  
  
  setwd(paste0(dataPath, "Tables"))
    
  
  TempMn.out <- NULL
  TempMd.out <- NULL
  Yr.out <- NULL
  PrecipMn.out <- NULL
  PrecipMd.out <- NULL
  
  yrPath <- "09"
  
  
    Temp <- data.frame(mup=NULL)
    for (i in 0:11)
      {
        Temp.in <- read.dbf(paste0(basin, "_Temp_", yrPath, "_", i, ".dbf"))
      Temp <- rbind(Temp, Temp.in)
      }
    
    Temp$Year <- yrPath
    Temp09 <- Temp
    TempMn <- mean(unlist(Temp$Mean)) - 273.15
    TempMd <- median(unlist(Temp$Mean)) - 273.15
    TempMn.out <- append(TempMn.out, TempMn)
    TempMd.out <- append(TempMd.out, TempMd)
    Yr.out <- append(Yr.out, yrPath)
    
    Precip <- data.frame(mup=NULL)
    for (i in 0:11)
    {
      Precip.in <- read.dbf(paste0(basin, "_Precip_", yrPath, "_", i, ".dbf"))
      Precip <- rbind(Precip, Precip.in)
    }
    
    Precip$Year <- yrPath
    Precip09 <- Precip 
    PrecipMn <- mean(unlist(Precip$Mean))
    PrecipMd <- median(unlist(Precip$Mean))
    PrecipMn.out <- append(PrecipMn.out, PrecipMn)
    PrecipMd.out <- append(PrecipMd.out, PrecipMd)
    

##############
# some plots to look at 
# ##############
    
  plot(1:12, Temp00$Mean, main = paste0("Mean Monthly Surface Air Temperature, ", longBasin," 2000-2009", xlab="Month", ylab="Degrees C", ylim=c(265, 295))
  points(1:12, Temp01$Mean, pch=16, col="blue")
  points(1:12, Temp02$Mean, pch=16, col="red")
  points(1:12, Temp03$Mean, pch=16, col="lightblue")
  points(1:12, Temp04$Mean, pch=16, col="orange")
  points(1:12, Temp05$Mean, pch=16, col="pink")
  points(1:12, Temp06$Mean, pch=16, col="purple")
  points(1:12, Temp07$Mean, pch=16, col="palevioletred")
  points(1:12, Temp08$Mean, pch=16, col="grey50")
  points(1:12, Temp09$Mean, pch=16, col="green")
  lines(1:12, Temp00$Mean, col="black")
  lines(1:12, Temp01$Mean, col="blue")
  lines(1:12, Temp02$Mean, col="red")
  lines(1:12, Temp03$Mean, col="lightblue")
  lines(1:12, Temp04$Mean, col="orange")
  lines(1:12, Temp05$Mean, col="pink")
  lines(1:12, Temp06$Mean, col="purple")
  lines(1:12, Temp07$Mean, col="palevioletred")
  lines(1:12, Temp08$Mean, col="grey50")
  lines(1:12, Temp09$Mean, col="green")
  legend("topright", pch=16, title="(K)", legend = c("00","01","02","03","04","05","06","07","08","09"), col=c("black", "blue", "red", "lightblue", "orange", "pink", "purple", "palevioletred", "grey50", "green"), cex=.6)
  
  plot(1:12, Precip00$Mean, main = "Mean Monthly Total Precip, Wenatchee, 2000-2009", xlab="Month", ylab="kgm^2", ylim=c(0, 1.9))
  points(1:12, Precip01$Mean, pch=16, col="blue")
  points(1:12, Precip02$Mean, pch=16, col="red")
  points(1:12, Precip03$Mean, pch=16, col="lightblue")
  points(1:12, Precip04$Mean, pch=16, col="orange")
  points(1:12, Precip05$Mean, pch=16, col="pink")
  points(1:12, Precip06$Mean, pch=16, col="purple")
  points(1:12, Precip07$Mean, pch=21, col="palevioletred")
  points(1:12, Precip08$Mean, pch=16, col="grey50")
  points(1:12, Precip09$Mean, pch=16, col="green")
  lines(1:12, Precip00$Mean, col="black")
  lines(1:12, Precip01$Mean, col="blue")
  lines(1:12, Precip02$Mean, col="red")
  lines(1:12, Precip03$Mean, col="lightblue")
  lines(1:12, Precip04$Mean, col="orange")
  lines(1:12, Precip05$Mean, col="pink")
  lines(1:12, Precip06$Mean, col="purple")
  lines(1:12, Precip07$Mean, col="palevioletred")
  lines(1:12, Precip08$Mean, col="grey50")
  lines(1:12, Precip09$Mean, col="green")
  legend(x=grconvertX(c(1.0,1.4), from='npc'), 
        y=grconvertY(c(0.6, 0.8), from='npc'), pch=16, title="kg/m^2", bty="n", legend = c("00","01","02","03","04","05","06","07","08","09"), col=c("black", "blue", "red", "lightblue", "orange", "pink", "purple", "palevioletred", "grey50", "green"), cex=.6, xpd=NA)
  
  mod <- lm(Precip$Mean~Temp$MEAN)
  plot(Temp00$MEAN, Precip$Mean, pch=16, main = "Precipitation by Temperature, Wenatchee, 2000", xlab="Temp", ylab="Precip")
  abline(mod)
  
  plot(Temp00$Mean, Precip00$Mean, main = "Mean Monthly Total Precip by Temp, Wenatchee, 2000-2009", xlab="Month", ylab="kgm^2", ylim=c(0, 1.9), xlim=c(265, 295))
  points(Temp01$Mean, Precip01$Mean, pch=16, col="blue")
  points(Temp02$Mean, Precip02$Mean, pch=16, col="red")
  points(Temp03$Mean, Precip03$Mean, pch=16, col="lightblue")
  points(Temp04$Mean, Precip04$Mean, pch=16, col="orange")
  points(Temp05$Mean, Precip05$Mean, pch=16, col="pink")
  points(Temp06$Mean, Precip06$Mean, pch=16, col="purple")
  points(Temp07$Mean, Precip07$Mean, pch=21, col="palevioletred")
  points(Temp08$Mean, Precip08$Mean, pch=16, col="grey50")
  points(Temp09$Mean, Precip09$Mean, pch=16, col="green")
  abline(lm(Precip00$Mean ~ Temp00$Mean), col="black")
  abline(lm(Precip01$Mean ~ Temp01$Mean), col="blue")
  abline(lm(Precip02$Mean ~ Temp02$Mean), col="red")
  abline(lm(Precip03$Mean ~ Temp03$Mean), col="lightblue")
  abline(lm(Precip04$Mean ~ Temp04$Mean), col="orange")
  abline(lm(Precip05$Mean ~ Temp05$Mean), col="pink")
  abline(lm(Precip06$Mean ~ Temp06$Mean), col="purple")
  abline(lm(Precip07$Mean ~ Temp07$Mean), col="palevioletred")
  abline(lm(Precip08$Mean ~ Temp08$Mean), col="grey50")
  abline(lm(Precip09$Mean ~ Temp09$Mean), col="green")
  legend(x=grconvertX(c(1.0,1.4), from='npc'), 
         y=grconvertY(c(0.6, 0.8), from='npc'), pch=16, bty="n", legend = c("00","01","02","03","04","05","06","07","08","09"), col=c("black", "blue", "red", "lightblue", "orange", "pink", "purple", "palevioletred", "grey50", "green"), cex=.6, xpd=NA)
  
  AllTemp <- rbind(Temp00, Temp01, Temp02, Temp03, Temp04, Temp05, Temp06, Temp07, Temp08, Temp09)
  plot(AllTemp$Mean)
  lines(1:120, AllTemp$Mean)
  allPrecip <- rbind(Precip00, Precip01, Precip02, Precip03, Precip04, Precip05, Precip06, Precip07, Precip08, Precip09)
  plot(allPrecip$Mean)
  lines(1:120, allPrecip$Mean)
  
  sumPrecip <- NULL
  sumP <- colSums(Precip00$Mean)
  sumPrecip <- append(sumPrecip, sum(Precip09$Mean))
  plot(TempMn.out, sumPrecip, main="Sum precip by Mean temp")
  
# ###############################
# PRISM raster processing
# ##################################


  basinPts <- read.dbf(paste0(mainPath, basin, "_PRISM_pts.dbf"))
  pts <- data.frame(basinPts[,1], basinPts[,2])
  
  
  #### Precip #####
##################
  # precip
# #################
  
  
  for (j in 1998:2015)
    {  
        year <- j
        yr <- j
        setwd(paste0("D:/OneDrive/work/GIS/PRISM/", year))
        varName <- paste0("Cppt", yr)
        ##### create a list of only the grid files in a directory
        
        allFiles <- list.files(pattern="*bil.bil", full.names=TRUE)
        xmlFiles <- list.files(pattern="*bil.bil.aux.xml", full.names=TRUE)
        fileList <- allFiles[!allFiles %in% xmlFiles]
        
        ###### read in one grid to get the structure
        
        #r <- readGDAL(fileList[1])
        rRaster <- raster(fileList[1])
        
        ##### clip the raster to a reasonable PNW extent
        
        # ext <- extent(-125, -107, 40, 50)
        # rExt <- crop(rRaster, ext)
        # 
        ##### convert the raster to points and build the data structure for the loop
        
        # rPoints <- rasterToPoints(rExt)
        # 
       
        data.out <- data.frame(pts, extract(rRaster, pts))
        colnames(data.out) <- c("x", "y", paste0(varName,"001"))
        #rPoints1 <- SpatialPoints(coordinates(rPoints), proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
        
        ##### loop thru all the files in the list and add them iteratively
        
        for (i in 2:365)
          {
          
          r2 <- raster(fileList[i])
          extData <- extract(r2, pts)
          sumData <- extData + data.out[,i+1]
          data.out <- cbind(data.out, sumData)
          namer <- sprintf('%03d', i)
          colnames(data.out)[i+2] <- paste0(varName, namer)
          }
        
        data.out$PtID <- basinPts$PtID
        write.dbf(data.out, file= paste0("SumPpt", year, ".dbf"))
        
        
        assign(paste0("Ppt_", basin, "_", year), colMeans(data.out))
        
    }

  list <- mget(paste0("Ppt_", basin, "_", 1981:2010))
  all <- do.call(rbind, list)
  
    
############# that one time I needed to reread everything ##########
  
  for (j in 2000:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/", year))
      test.in <- read.dbf(paste0("SumPpt", year, ".dbf"))
      data.out_JD <- test.in[test.in$PtID %in% basinPts$PtID,]
      assign(paste0("Ppt_JD_", year), colMeans(data.out_JD))
    }
  
#################### This part just creates a list of all the vector names and row binds them ##############
  
  list <- mget(paste0("Ppt_JD_", 2000:2015))
  all <- do.call(rbind, list)
  
  y01_15_ppt <- all[2:16, 367]
  write.csv(y01_15_ppt, file=paste0(basin, "_01_15_mn_sum_precip.csv"))
  
######################
# parsing out basin
# #####################
  
  for (j in 1981:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/Temp/", year))
      data.in <- read.dbf(paste0("SumTmp", year, ".dbf"))
      data.out <- data.in[data.in$PtID %in% basinPts$PtID,]
      write.dbf(data.out, file= paste0("SumTmpJD", year, ".dbf"))
    }
 
######################
# if going back later 
# #####################
#### Temperature #####
  
  for (j in 1981:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/Temp/", year))
      data.in <- read.dbf(paste0("SumTmp", basin, year, ".dbf"))
      assign(paste0("Tmp_", basin, "_", year), colMeans(data.in[3:367]))
    }
  
  orderedcolors2 <- rainbow(length(data.in$Ctmp15365))[order(order(data.in$Ctmp15365))]
  plot(data.in$x, data.in$y, col = orderedcolors2)
  
  list <- mget(paste0("Tmp_", basin, "_", 2001:2015))
  all <- do.call(rbind, list)
  
  y01_15_temp <- all[1:15, 367] #this is the cumulative mean temperature by year
  
 
#### Precip ######
  
  for (j in 1981:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/", year))
      data.in <- read.dbf(paste0("SumPpt", basin, year, ".dbf"))
      assign(paste0("Ppt_", basin, "_", year), colMeans(data.in[3:367]))
    }
  
  list <- mget(paste0("Ppt_", basin, "_", 2001:2015))
  all <- do.call(rbind, list)
  
  y01_15_ppt <- all[1:15, 367] # this is the cumulative mean for each year
  
  col.rainbow <- rainbow(15)
  
  plot(y01_15_temp, y01_15_ppt, pch=c(21,22,23,24, 25), col="black", bg=col.rainbow, main=paste0(basin, " cumulative mean temp/precip (calendar year)"), xlab = "Mean temp", ylab = "Mean precip", cex=1.5,)
  points(y30_Tmp_mn, y30_Ppt_mn, pch=8, col="black", cex=1.5)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'), 
         y=grconvertY(c(0.1, 1.0), from='npc'), pch=c(21, 22, 23, 24, 25), bty="n", legend = c("01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col="black", pt.bg=col.rainbow, cex=.75, xpd=NA)
  
# #####
# # Subsetting and summarizing by basin ppt
# #####
#   
  y30_precip <- c(mean(Ppt_JD_1981), mean(Ppt_JD_1982), mean(Ppt_JD_1983), mean(Ppt_JD_1984), mean(Ppt_JD_1985), mean(Ppt_JD_1986), mean(Ppt_JD_1987), mean(Ppt_JD_1988), mean(Ppt_JD_1989), mean(Ppt_JD_1990), mean(Ppt_JD_1991), mean(Ppt_JD_1992), mean(Ppt_JD_1993), mean(Ppt_JD_1994), mean(Ppt_JD_1995), mean(Ppt_JD_1996), mean(Ppt_JD_1997), mean(Ppt_JD_1998), mean(Ppt_JD_1999), mean(Ppt_JD_2000), mean(Ppt_JD_2001), mean(Ppt_JD_2002), mean(Ppt_JD_2003), mean(Ppt_JD_2004), mean(Ppt_JD_2005), mean(Ppt_JD_2006), mean(Ppt_JD_2007), mean(Ppt_JD_2008), mean(Ppt_JD_2009), mean(Ppt_JD_2010))
  y30_temp <- c(mean(Tmp_JD_1981), mean(Tmp_JD_1982), mean(Tmp_JD_1983), mean(Tmp_JD_1984), mean(Tmp_JD_1985), mean(Tmp_JD_1986), mean(Tmp_JD_1987), mean(Tmp_JD_1988), mean(Tmp_JD_1989), mean(Tmp_JD_1990), mean(Tmp_JD_1991), mean(Tmp_JD_1992), mean(Tmp_JD_1993), mean(Tmp_JD_1994), mean(Tmp_JD_1995), mean(Tmp_JD_1996), mean(Tmp_JD_1997), mean(Tmp_JD_1998), mean(Tmp_JD_1999), mean(Tmp_JD_2000), mean(Tmp_JD_2001), mean(Tmp_JD_2002), mean(Tmp_JD_2003), mean(Tmp_JD_2004), mean(Tmp_JD_2005), mean(Tmp_JD_2006), mean(Tmp_JD_2007), mean(Tmp_JD_2008), mean(Tmp_JD_2009), mean(Tmp_JD_2010))
   y30_mn_temp <- mean(y30_temp)
   y30_mn_precip <- mean(y30_precip)
  # this is the mean cumulative mean for each year 
  y01_15_precip <- c(mean(Ppt_JD_2001), mean(Ppt_JD_2002), mean(Ppt_JD_2003), mean(Ppt_JD_2004), mean(Ppt_JD_2005), mean(Ppt_JD_2006), mean(Ppt_JD_2007), mean(Ppt_JD_2008), mean(Ppt_JD_2009), mean(Ppt_JD_2010), mean(Ppt_JD_2011), mean(Ppt_JD_2012), mean(Ppt_JD_2013), mean(Ppt_JD_2014), mean(Ppt_JD_2015))
  y01_15_temp <- c(mean(Tmp_JD_2001), mean(Tmp_JD_2002), mean(Tmp_JD_2003), mean(Tmp_JD_2004), mean(Tmp_JD_2005), mean(Tmp_JD_2006), mean(Tmp_JD_2007), mean(Tmp_JD_2008), mean(Tmp_JD_2009), mean(Tmp_JD_2010), mean(Tmp_JD_2011), mean(Tmp_JD_2012), mean(Tmp_JD_2013), mean(Tmp_JD_2014), mean(Tmp_JD_2015))
  
  plot(y01_15_temp, y01_15_precip, pch=c(21,22,23,24, 25), col="black", bg=col.rainbow, main=paste0(basin, " mean cumulative mean temp/precip (calendar year)"), xlab = "Mean temp", ylab = "Mean precip", cex=1.5,)
  points(y30_mn_temp, y30_mn_precip, pch=8, col="black", cex=1.5)
  
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'), 
         y=grconvertY(c(0.1, 1.0), from='npc'), pch=c(21, 22, 23, 24, 25), bty="n", legend = c("01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col="black", pt.bg=col.rainbow, cex=.75, xpd=NA)
  
  #list <- mget(paste0("Ppt_", basin, "_", 1981:2011))
  #all <- do.call(rbind, list)
  #y30_precip <- colMeans(all[,3:367])
  #y30_Ppt_mn <- mean(y30_precip)
#####  
  plot(Ppt_JD_2000[3:367], pch=16, col="cornflowerblue", cex=.7, main = "Mean cumulative precip, John Day basin, 2000-2015", xlab="Julian Day", ylab="Precip", ylim=c(0,600))
  points(Ppt_JD_2001[3:367], pch=16, col="cyan4", cex=.7)
  points(Ppt_JD_2002[3:367], pch=16, col="khaki", cex=.7)
  points(Ppt_JD_2003[3:367], pch=16, col="deeppink4", cex=.7)
  points(Ppt_JD_2004[3:367], pch=16, col="deeppink", cex=.7)
  points(Ppt_JD_2005[3:367], pch=16, col="darkgrey", cex=.7)
  points(Ppt_JD_2006[3:367], pch=16, col="lightblue", cex=.7)
  points(Ppt_JD_2007[3:367], pch=16, col="red", cex=.7)
  points(Ppt_JD_2008[3:367], pch=16, col="palevioletred", cex=.7)
  points(Ppt_JD_2009[3:367], pch=16, col="darkgoldenrod1", cex=.7)
  points(Ppt_JD_2010[3:367], pch=16, col="blueviolet", cex=.7)
  points(Ppt_JD_2011[3:367], pch=16, col="lightgrey", cex=.7)
  points(Ppt_JD_2012[3:367], pch=16, col="darkolivegreen4", cex=.7)
  points(Ppt_JD_2013[3:367], pch=16, col="darkturquoise", cex=.7)
  points(Ppt_JD_2014[3:367], pch=16, col="chartreuse2", cex=.7)
  points(Ppt_JD_2015[3:367], pch=15, col="blue2", cex=.7)

  points(y30_precip[1:365], pch=16, col="black", cex=.8)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'),
         y=grconvertY(c(0.6, 0.9), from='npc'), pch=16, bty="n", legend = c("00", "01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col=c("cornflowerblue", "cyan4", "khaki", "deeppink4", "deeppink", "darkgrey", "lightblue", "red", "palevioletred", "darkgoldenrod1", "blueviolet", "lightgrey", "darkolivegreen4", "darkturquoise", "chartreuse2", "blue2"), cex=.8, xpd=NA)

  
  ######### tying out different color ramps ##### 
  col.spec <- colorRamps::blue2green2red(16)
  col.topo <- topo.colors(16)
  col.heat <- heat.colors(16)
  col.terrain <- terrain.colors(16)
  col.cm <- cm.colors(16)
  col.hcl <- diverge_hcl(16, h = c(246, 40), c = 96, l = c(65, 90))
  
  col.hsv <- diverge_hsv(16)
  
  plot(Ppt_JD_2000[3:367], pch=16, col=col.spec[1], cex=.7, main = "Mean cumulative precip, John Day basin, 2000-2015", xlab="Julian Day", ylab="Precip", ylim=c(0,600))
  points(Ppt_JD_2001[3:367], pch=16, col=col.spec[2], cex=.7)
  points(Ppt_JD_2002[3:367], pch=16, col=col.spec[3], cex=.7)
  points(Ppt_JD_2003[3:367], pch=16, col=col.spec[4], cex=.7)
  points(Ppt_JD_2004[3:367], pch=16, col=col.spec[5], cex=.7)
  points(Ppt_JD_2009[3:367], pch=16, col=col.spec[6], cex=.7)
  points(Ppt_JD_2008[3:367], pch=16, col=col.spec[7], cex=.7)
  points(Ppt_JD_2007[3:367], pch=16, col=col.spec[8], cex=.7)
  points(Ppt_JD_2011[3:367], pch=16, col=col.spec[9], cex=.7)
  points(Ppt_JD_2006[3:367], pch=16, col=col.spec[10], cex=.7)
  points(Ppt_JD_2005[3:367], pch=16, col=col.spec[11], cex=.7)
  points(Ppt_JD_2010[3:367], pch=16, col=col.spec[12], cex=.7)
  points(Ppt_JD_2012[3:367], pch=16, col=col.spec[13], cex=.7)
  points(Ppt_JD_2013[3:367], pch=16, col=col.spec[14], cex=.7)
  points(Ppt_JD_2014[3:367], pch=16, col=col.spec[15], cex=.7)
  points(Ppt_JD_2015[3:367], pch=15, col=col.spec[16], cex=.7)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'),
         y=grconvertY(c(0.6, 0.8), from='npc'), pch=16, bty="n", legend = c("00", "01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col=col.spec, cex=.65, xpd=NA)
  
##################
# Temperature
# #################
  
  
  for (j in 2010:2011)
    {  
      year <- j
      yr <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/Temp/", year))
      varName <- paste0("Ctmp", yr)
      ##### create a list of only the grid files in a directory
      
      allFiles <- list.files(pattern="*bil.bil", full.names=TRUE)
      xmlFiles <- list.files(pattern="*bil.bil.aux.xml", full.names=TRUE)
      fileList <- allFiles[!allFiles %in% xmlFiles]
      
      ###### read in one grid to get the structure
      
      rRaster <- raster(fileList[1])
      
    
      
      data.out <- data.frame(pts, extract(rRaster, pts))
      colnames(data.out) <- c("x", "y", paste0(varName,"001"))
      data.out[,3][data.out[,3] < 0]  <- 0
      
      ##### loop thru all the files in the list and add them iteratively
      
      for (i in 2:365)
      {
        
        r2 <- raster(fileList[i])
        extData <- extract(r2, pts)
        extData[extData < 0] <- 0
        sumData <- extData + data.out[,i+1]
        data.out <- cbind(data.out, sumData)
        namer <- sprintf('%03d', i)
        colnames(data.out)[i+2] <- paste0(varName, namer)
      }
      
      data.out$PtID <- basinPts$PtID
      write.dbf(data.out, file= paste0("SumTmp", year, ".dbf"))
      
      
      assign(paste0("Tmp_", basin, "_", year), colMeans(data.out))
      
    }

  
  y30_temp <- c(mean(Tmp_JD_1981), mean(Tmp_JD_1982), mean(Tmp_JD_1983), mean(Tmp_JD_1984), mean(Tmp_JD_1985), mean(Tmp_JD_1986), mean(Tmp_JD_1987), mean(Tmp_JD_1988), mean(Tmp_JD_1989), mean(Tmp_JD_1990), mean(Tmp_JD_1991), mean(Tmp_JD_1992), mean(Tmp_JD_1993), mean(Tmp_JD_1994), mean(Tmp_JD_1995), mean(Tmp_JD_1996), mean(Tmp_JD_1997), mean(Tmp_JD_1998), mean(Tmp_JD_1999), mean(Tmp_JD_2000), mean(Tmp_JD_2001), mean(Tmp_JD_2002), mean(Tmp_JD_2003), mean(Tmp_JD_2004), mean(Tmp_JD_2005), mean(Tmp_JD_2006), mean(Tmp_JD_2007), mean(Tmp_JD_2008), mean(Tmp_JD_2009), mean(Tmp_JD_2010), mean(Tmp_JD_2011), mean(Tmp_JD_2012), mean(Tmp_JD_2013), mean(Tmp_JD_2014), mean(Tmp_JD_2015))

  # this is the mean cumulative mean by year
  
  y01_15_temper <- c(mean(Tmp_JD_2001), mean(Tmp_JD_2002), mean(Tmp_JD_2003), mean(Tmp_JD_2004), mean(Tmp_JD_2005), mean(Tmp_JD_2006), mean(Tmp_JD_2007), mean(Tmp_JD_2008), mean(Tmp_JD_2009), mean(Tmp_JD_2010), mean(Tmp_JD_2011), mean(Tmp_JD_2012), mean(Tmp_JD_2013), mean(Tmp_JD_2014), mean(Tmp_JD_2015))
  
  list <- mget(paste0("Tmp_", basin, "_", 1981:2010))
  all <- do.call(rbind, list)
  y30_Tmp <- colMeans(all[,3:367])
  
  plot(Tmp_JD_2000[3:367], pch=16, col="cornflowerblue", cex=.7, main = "Mean cumulative temp, John Day basin, 2000-2015", xlab="Julian Day", ylab="Precip", ylim=c(0,4000))
  points(Tmp_JD_2001[3:367], pch=16, col="cyan4", cex=.7)
  points(Tmp_JD_2002[3:367], pch=16, col="khaki", cex=.7)
  points(Tmp_JD_2003[3:367], pch=16, col="deeppink4", cex=.7)
  points(Tmp_JD_2004[3:367], pch=16, col="deeppink", cex=.7)
  points(Tmp_JD_2009[3:367], pch=16, col="darkgrey", cex=.7)
  points(Tmp_JD_2008[3:367], pch=16, col="lightblue", cex=.7)
  points(Tmp_JD_2007[3:367], pch=16, col="red", cex=.7)
  points(Tmp_JD_2011[3:367], pch=16, col="palevioletred", cex=.7)
  points(Tmp_JD_2006[3:367], pch=16, col="darkgoldenrod1", cex=.7)
  points(Tmp_JD_2005[3:367], pch=16, col="blueviolet", cex=.7)
  points(Tmp_JD_2010[3:367], pch=16, col="lightgrey", cex=.7)
  points(Tmp_JD_2012[3:367], pch=16, col="darkolivegreen4", cex=.7)
  points(Tmp_JD_2013[3:367], pch=16, col="darkturquoise", cex=.7)
  points(Tmp_JD_2014[3:367], pch=16, col="chartreuse2", cex=.7)
  points(Tmp_JD_2015[3:367], pch=15, col="blue2", cex=.7)
  
  points(y30_Tmp[3:367], pch=16, col="black", cex=.8)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'),
         y=grconvertY(c(0.6, 0.8), from='npc'), pch=16, bty="n", legend = c("00", "01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col=c("cornflowerblue", "cyan4", "khaki", "deeppink4", "deeppink", "blueviolet", "darkgoldenrod1", "red", "lightblue", "black", "lightgrey", "palevioletred", "darkolivegreen4", "darkturquoise", "chartreuse2", "blue2"), cex=.65, xpd=NA)
  
 

############# that one time I needed to rename everything ##########
  
  for (j in 1981:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/", year))
      data.in <- read.dbf(paste0("SumPpt", year, ".dbf"))
      write.dbf(data.out, file= paste0("SumPptJD", year, ".dbf"))
    }
#################### This part just creates a list of all the vector names and row binds them ##############
  
  lister <- mget(paste0("Ppt_JD_", 1981:2011))
  all <- do.call(rbind, list)
  y30_precip <- colMeans(all)
  
  y30_Ppt_mn<- mean(y30_precip[3:367])
  
  lister <- mget(paste0("Tmp_JD_", 1981:2011))
  all <- do.call(rbind, list)
  y30_temp <- colMeans(all)
  
  y30_Tmp_mn<- mean(y30_precip[3:367])
  
  col.rainbow <- rainbow(15)
  
###################
# if all I need to do is re-read the files in
##################
  
  #### temp### 
  
  for (j in 1981:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/Temp/", year))
      data.in <- read.dbf(paste0("SumTmp", basin, year, ".dbf"))
      assign(paste0("Tmp_", basin, "_", year), colMeans(data.in))
    }
  
  #### Precip ######
  
  for (j in 1981:2015)
    {
      year <- j
      setwd(paste0("D:/OneDrive/work/GIS/PRISM/", year))
      data.in <- read.dbf(paste0("SumPpt", basin, year, ".dbf"))
      assign(paste0("Ppt_", basin, "_", year), colMeans(data.in))
    }
  
  ########################
  
######### plot ###############
  
  y30_Tmp_mn <- colMeans(all[,3:367])

  
  ##### Subsetting and summarizing by basin temp

  
  
  plot(1:365, Tmp_JD_2000[3:367], pch=16, col="cornflowerblue", cex=.7, main = "Mean cumulative temp, John Day basin, 2000-2015", xlab="Julian Day", ylab="Temp", ylim=c(0,4000))
  points(Tmp_JD_2001[3:367], pch=16, col="cyan4", cex=.7)
  points(Tmp_JD_2002[3:367], pch=16, col="khaki", cex=.7)
  points(Tmp_JD_2003[3:367], pch=16, col="deeppink4", cex=.7)
  points(Tmp_JD_2004[3:367], pch=16, col="deeppink", cex=.7)
  points(Tmp_JD_2005[3:367], pch=16, col="darkgrey", cex=.7)
  points(Tmp_JD_2006[3:367], pch=16, col="lightblue", cex=.7)
  points(Tmp_JD_2007[3:367], pch=16, col="red", cex=.7)
  points(Tmp_JD_2008[3:367], pch=16, col="palevioletred", cex=.7)
  points(Tmp_JD_2009[3:367], pch=16, col="darkgodenrod1", cex=.7)
  points(Tmp_JD_2010[3:367], pch=16, col="blueviolet", cex=.7)
  points(Tmp_JD_2011[3:367], pch=16, col="lightgrey", cex=.7)
  points(Tmp_JD_2012[3:367], pch=16, col="darkolivegreen4", cex=.7)
  points(Tmp_JD_2013[3:367], pch=16, col="darkturquoise", cex=.7)
  points(Tmp_JD_2014[3:367], pch=16, col="chartreuse2", cex=.7)
  points(Tmp_JD_2015[3:367], pch=15, col="blue2", cex=.7)
  
  #points(y30_Tmp_mn[1:365], pch=16, col="black", cex=.8)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'),
         y=grconvertY(c(0.6, 0.9), from='npc'), pch=16, bty="n", legend = c("00", "01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col=c("cornflowerblue", "cyan4", "khaki", "deeppink4", "deeppink", "darkgrey", "lightblue", "red", "palevioletred", "darkgoldenrod1", "blueviolet", "lightgrey", "darkolivegreen4", "darkturquoise", "chartreuse2", "blue2"), cex=.8, xpd=NA)
  
  
##### vector of annual mean temps ####
  y30_temp <- c(mean(Tmp_Wen_1981), mean(Tmp_Wen_1982), mean(Tmp_Wen_1983), mean(Tmp_Wen_1984), mean(Tmp_Wen_1985), mean(Tmp_Wen_1986), mean(Tmp_Wen_1987), mean(Tmp_Wen_1988), mean(Tmp_Wen_1989), mean(Tmp_Wen_1990), mean(Tmp_Wen_1991), mean(Tmp_Wen_1992), mean(Tmp_Wen_1993), mean(Tmp_Wen_1994), mean(Tmp_Wen_1995), mean(Tmp_Wen_1996), mean(Tmp_Wen_1997), mean(Tmp_Wen_1998), mean(Tmp_Wen_1999), mean(Tmp_Wen_2000), mean(Tmp_Wen_2001), mean(Tmp_Wen_2002), mean(Tmp_Wen_2003), mean(Tmp_Wen_2004), mean(Tmp_Wen_2005), mean(Tmp_Wen_2006), mean(Tmp_Wen_2007), mean(Tmp_Wen_2008), mean(Tmp_Wen_2009), mean(Tmp_Wen_2010), mean(Tmp_Wen_2011), mean(Tmp_Wen_2012), mean(Tmp_Wen_2013), mean(Tmp_Wen_2014), mean(Tmp_Wen_2015))
  #
  Ann_mn_temp <- c(mean(Wen_00_mn), mean(Wen_01_mn), mean(Wen_02_mn), mean(Wen_03_mn), mean(Wen_04_mn), mean(Wen_05_mn), mean(Wen_06_mn), mean(Wen_07_mn), mean(Wen_08_mn), mean(Wen_09_mn), mean(Wen_10_mn), mean(Wen_11_mn), mean(Wen_12_mn), mean(Wen_13_mn), mean(Wen_14_mn), mean(Wen_15_mn))

  col.rainbow <- rainbow(15)
  
  plot(y30_temp[1:30], y30_precip[1:30], pch=21, col="black", bg=col.rainbow, main="Mean sum temp/precip (calendar year)", xlab = "Mean temp", ylab = "Mean precip", cex=1.5, xlim=c(900,1600),)
  points(y30_temp[31:35], y30_precip[31:35], pch=22, col="black", bg=col.rainbow, cex=1.5)
  
  y30_mn_temp <- mean(y30_temp)
  y30_mn_precip <- mean(y30_precip)
  points(y30_mn_temp, y30_mn_precip, pch=8, cex=1.4)
  
  plot(y30_temp[20:35], y30_precip[20:35], pch=c(21, 22, 24), col="black", bg=col.rainbow, main="Mean sum temp/precip (calendar year)", xlab = "Mean temp", ylab = "Mean precip", cex=1.5, xlim=c(900,1600),)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'), 
         y=grconvertY(c(0.6, 0.8), from='npc'), pch=c(16,15,17), bty="n", legend = c("00", "01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col=col.rainbow, cex=.65, xpd=NA)
  
#########
# calendar year
#########
 
  plot(Ann_mn_tmp_cy, Ann_mn_ppt_cy, pch=c(21,22,24)[, col="black", bg=col.rainbow, main="Mean sum temp/precip (calendar year)", xlab = "Mean temp", ylab = "Mean precip", cex=1.5,)
  points(y30_Tmp_mn, y30_Ppt_mn, 
  # legend(x=grconvertX(c(1.0,1.4), from='npc'), 
  #        y=grconvertY(c(0.5, 0.9), from='npc'), pch=c(16,15,17), col=col.rainbow, bty="n", legend = c("01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), cex=.75, xpd=NA)

  legend("topright", pch=c(15,16,17), col=col.rainbow, bty="n", legend = c("01", "02", "03", "04", "05", "06", "07", "08","09","10", "11", "12", "13", "14", "15"), cex=.7, pt.cex=.95)
  textxy(clust_out$temp, clust_out$precip, clust_out$group, cex=.8, offset = 1.0, pos=2)
#########
# little chunk of code to add a climate year categorical variable.  This is based on hand-waving eye-ballin'. Change and iterate as needed
##########
  
  for(i in 1:15) 
    {
      if((coeffs.out.all$Ann_ppt_cy[i] >650) & (coeffs.out.all$Ann_tmp_cy[i] >1250)) {
        coeffs.out.all$ClimateYr[i] <- "HW"}
    }
  
######
# more fomal clustering analysis:
# #######
  library(fpc)
  library(vegan)
  library(mclust)
  
  mydata <- cbind(y01_15_temper, y01_15_precip)
  mydata <- as.data.frame(mydata)
  
  #####  for determining the number of clusters #######  
 
  pamk.best <- pamk(mydata)
  cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
  
  
  fit <- cascadeKM(scale(mydata, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
  plot(fit, sortg = TRUE, grpmts.plot = TRUE)
  calinski.best <- as.numeric(which.max(fit$results[2,]))
  cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
  
  
  # Run the function to see how many clusters
  # it finds to be optimal, set it to search for
  # at least 1 model and up 20.
  d_clust <- Mclust(as.matrix(mydata), G=1:20)
  m.best <- dim(d_clust$z)[2]
  cat("model-based optimal number of clusters:", m.best, "\n")
  
  plot(d_clust)
  
  
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:14) wss[i] <- sum(kmeans(mydata, 
                                       centers=i)$withinss)
  plot(1:14, wss, type="b", main="Within group sums of squares by # of clusters", xlab="Number of Clusters", ylab="Within groups sum of squares")
  
  d <- dist(mydata, method = "euclidean")
  fit <- hclust(d, method="ward.D2")
  plot(fit)
  groups <- cutree(fit, k=3)
  rect.hclust(fit, k=3, border="red")
  
  clust_out <- mydata
  clust_out$year <- 1:15
  clust_out <- cbind(clust_out, groups)
  colnames(clust_out) <- c("temp", "precip", "year", "group")
                           
  plot(clust_out$temp, clust_out$precip, pch=c(21,22,23,24,25), col="black", bg=col.rainbow, main=paste0(basin, " mean cumulative temp/precip (calendar year)"), xlab = "Mean cumulative temp", ylab = "Mean cumulative precip", cex=1.5)
  
  legend(x=grconvertX(c(1.0,1.4), from='npc'), 
         y=grconvertY(c(0.6, 0.9), from='npc'), pch=c(21,22,23,24,25), bty="n", legend = c("01", "02", "03", "04", "05", "06", "07","08","09", "10", "11", "12", "13", "14", "15"), col="black", pt.bg=col.rainbow, cex=.8, xpd=NA)
  
  textxy(clust_out$temp, clust_out$precip, clust_out$group, cex=.8, offset = 1.0, pos=2)
  #textxy(clust_out$temp, clust_out$precip, clust_out$year, cex=.8, offset = 1.0, pos=1)
  
  
  dist <- apply(mydata, 1, function(x) sqrt((x[1]-1172.935)^2 + (x[2]-669.6563)^2)) 
  mydata <- as.data.frame(mydata)
  mydata$year <- rownames(mydata)
  mydata$year[1:15] <- 1:15
  mydata$dist <- dist
####################
  
  plot(Ann_mn_tmp_cy, Ann_mn_ppt_cy, pch=c(21,22,24), col="black", bg=col.rainbow, main="Mean sum temp/precip (calendar year)", xlab = "Mean temp", ylab = "Mean precip", cex=1.5,)
  
  legend("topright", pch=c(15,16,17), col=col.rainbow, bty="n", legend = c("01", "02", "03", "04", "05", "06", "07", "08","09","10", "11", "12", "13", "14", "15"), cex=.7, pt.cex=.95)
  textxy(clust_out$Ann_tmp_cy, clust_out$Ann_ppt_cy, clust_out$ward_dist, cex=.8, offset = 1.0, pos=2)

########################################
# parsing basin-wide data into water-year
# ##################################
  rm(list=ls(pattern="Tmp_JD_")) #first, get rid of a bunch of memory hogging stuff

  #to get a layer that has the CRB Albers projection info  
  longBasin <- "Entiat"
  dataPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"
  projname <- paste0(longBasin, "_net_rca")
  setwd(paste0(dataPath, longBasin))
  proj_layer <- readOGR(dsn=".", layer = projname)
  ##
  
  pts <- basinPts$PtID
  
  setwd(paste0("D:/OneDrive/work/GIS/PRISM/water_year_output/"))
  coordinates(basinPts) <- cbind(basinPts$x, basinPts$y)
  proj4string(basinPts) = proj4string(proj_layer)

  ### temperature ####
  
  for (k in 0:8)
    {
      
      yr1 <- paste0("0", k)
      
      yr2 <- paste0("0", k+1)
      
      data.in <- read.dbf(paste0("SumTmp", yr1, yr2, ".dbf"))
      coordinates(data.in) <- cbind(data.in$x, data.in$y)
      proj4string(data.in) = proj4string(proj_layer)
      data.out <- data.in[basinPts, ]
      flat.data <- as.data.frame(data.out)
      assign(paste0("Tmp_", basin, "_", yr1, yr2), colMeans(flat.data[,3:367]))
      write.dbf(flat.data, file= paste0("SumTmp", basin, yr1, yr2, ".dbf"))
    }
    
    k <- 9
    yr1 <- paste0("0", k)
    
    yr2 <- paste0(k+1)
    data.in <- read.dbf(paste0("SumTmp", yr1, yr2, ".dbf"))
    coordinates(data.in) <- cbind(data.in$x, data.in$y)
    proj4string(data.in) = proj4string(proj_layer)
    data.out <- data.in[basinPts, ]
    flat.data <- as.data.frame(data.out)
    assign(paste0("Tmp_", basin, "_", yr1, yr2), colMeans(flat.data[,3:367]))
    write.dbf(flat.data, file= paste0("SumTmp", basin, yr1, yr2, ".dbf"))
     
  
  for (k in 10:14)
    {
      
      yr1 <- paste0(k)
      
      yr2 <- paste0(k+1)
      
      data.in <- read.dbf(paste0("SumTmp", yr1, yr2, ".dbf"))
      coordinates(data.in) <- cbind(data.in$x, data.in$y)
      proj4string(data.in) = proj4string(proj_layer)
      data.out <- data.in[basinPts, ]
      flat.data <- as.data.frame(data.out)
      assign(paste0("Tmp_", basin, "_", yr1, yr2), colMeans(flat.data[,3:367]))
      write.dbf(flat.data, file= paste0("SumTmp", basin, yr1, yr2, ".dbf"))
    } 

  ### precip ####
    
    for (k in 0:8)
      {
        
        yr1 <- paste0("0", k)
        
        yr2 <- paste0("0", k+1)
        
        data.in <- read.dbf(paste0("SumPpt", yr1, yr2, ".dbf"))
        coordinates(data.in) <- cbind(data.in$x, data.in$y)
        proj4string(data.in) = proj4string(proj_layer)
        data.out <- data.in[basinPts, ]
        flat.data <- as.data.frame(data.out)
        assign(paste0("Ppt_", basin, "_", yr1, yr2), colMeans(flat.data[,3:367]))
        write.dbf(flat.data, file= paste0("SumPpt", basin, yr1, yr2, ".dbf"))
      }
    
      k <- 9
      yr1 <- paste0("0", k)
      
      yr2 <- paste0(k+1)
      data.in <- read.dbf(paste0("SumPpt", yr1, yr2, ".dbf"))
      coordinates(data.in) <- cbind(data.in$x, data.in$y)
      proj4string(data.in) = proj4string(proj_layer)
      data.out <- data.in[basinPts, ]
      flat.data <- as.data.frame(data.out)
      assign(paste0("Ppt_", basin, "_", yr1, yr2), colMeans(flat.data[,3:367]))
      write.dbf(flat.data, file= paste0("SumPpt", basin, yr1, yr2, ".dbf"))
      
    
    for (k in 10:14)
      {
        
        yr1 <- paste0(k)
        
        yr2 <- paste0(k+1)
        
        data.in <- read.dbf(paste0("SumPpt", yr1, yr2, ".dbf"))
        coordinates(data.in) <- cbind(data.in$x, data.in$y)
        proj4string(data.in) = proj4string(proj_layer)
        data.out <- data.in[basinPts, ]
        flat.data <- as.data.frame(data.out)
        assign(paste0("Ppt_", basin, "_", yr1, yr2), colMeans(flat.data[,3:367]))
        write.dbf(flat.data, file= paste0("SumPpt", basin, yr1, yr2, ".dbf"))
      }  
    
  lister <- mget(paste0("Tmp_JD_", 2000:2015))
  rm(list=ls(lister))
  
  lister <- mget(paste0("Ppt_JD_", 2000:2015))
  rm(list=ls(lister))
  
  lister <- lapply(ls(pattern="Tmp_JD_"), get)
  all <- do.call(rbind, lister)
  y00_15_wy_Tmp <- rowMeans(all[1:15,])
  
  lister <- lapply(ls(pattern="Ppt_JD_"), get)
  all <- do.call(rbind, lister)
  y00_15_wy_Ppt <- rowMeans(all[1:15,])
  
  plot(Ppt_JD_0001[3:367], pch=16, col="cornflowerblue", cex=.7, xaxt="n", main = "Mean cumulative precip (mm) by water-year, JD 2000-2015", xlab="Sept-Aug", ylab="Precip", ylim=c(0,1500))
  points(Ppt_JD_0102[3:367], pch=16, col="cyan4", cex=.7)
  points(Ppt_JD_0203[3:367], pch=16, col="khaki", cex=.7)
  points(Ppt_JD_0304[3:367], pch=16, col="deeppink4", cex=.7)
  points(Ppt_JD_0405[3:367], pch=16, col="deeppink", cex=.7)
  points(Ppt_JD_0506[3:367], pch=16, col="darkgrey", cex=.7)
  points(Ppt_JD_0607[3:367], pch=16, col="lightblue", cex=.7)
  points(Ppt_JD_0708[3:367], pch=16, col="red", cex=.7)
  points(Ppt_JD_0809[3:367], pch=16, col="palevioletred", cex=.7)
  points(Ppt_JD_0910[3:367], pch=16, col="darkgoldenrod1", cex=.7)
  points(Ppt_JD_1011[3:367], pch=16, col="blueviolet", cex=.7)
  points(Ppt_JD_1112[3:367], pch=16, col="lightgrey", cex=.7)
  points(Ppt_JD_1213[3:367], pch=16, col="darkolivegreen4", cex=.7)
  points(Ppt_JD_1314[3:367], pch=16, col="darkturquoise", cex=.7)
  points(Ppt_JD_1415[3:367], pch=16, col="chartreuse2", cex=.7)
  
  
  legend(x=grconvertX(c(0.985,1.3), from='npc'), 
         y=grconvertY(c(0.6, 1.0), from='npc'), pch=16, bty="n", legend = c("0001", "0102", "0203", "0304", "0405", "0506", "0607", "0708","0809","0910", "1011", "1112", "1213", "1314", "1415"), col=c("cornflowerblue", "cyan4", "khaki", "deeppink4", "deeppink", "darkgrey", "lightblue", "red", "palevioletred", "darkgoldenrod1", "blueviolet", "lightgrey", "darkolivegreen4", "darkturquoise", "chartreuse2"), cex=.7, xpd=NA)
  
  
  plot(Tmp_JD_0001[3:367], pch=16, col="cornflowerblue", cex=.7, xaxt="n", main = "Mean cumulative temp by water-year, JD 2000-2015", xlab="Sept-Aug", ylab="Precip", ylim=c(0,3200))
  points(Tmp_JD_0102[3:367], pch=16, col="cyan4", cex=.7)
  points(Tmp_JD_0203[3:367], pch=16, col="khaki", cex=.7)
  points(Tmp_JD_0304[3:367], pch=16, col="deeppink4", cex=.7)
  points(Tmp_JD_0405[3:367], pch=16, col="deeppink", cex=.7)
  points(Tmp_JD_0506[3:367], pch=16, col="darkgrey", cex=.7)
  points(Tmp_JD_0607[3:367], pch=16, col="lightblue", cex=.7)
  points(Tmp_JD_0708[3:367], pch=16, col="red", cex=.7)
  points(Tmp_JD_0809[3:367], pch=16, col="palevioletred", cex=.7)
  points(Tmp_JD_0910[3:367], pch=16, col="darkgoldenrod1", cex=.7)
  points(Tmp_JD_1011[3:367], pch=16, col="blueviolet", cex=.7)
  points(Tmp_JD_1112[3:367], pch=16, col="lightgrey", cex=.7)
  points(Tmp_JD_1213[3:367], pch=16, col="darkolivegreen4", cex=.7)
  points(Tmp_JD_1314[3:367], pch=16, col="darkturquoise", cex=.7)
  points(Tmp_JD_1415[3:367], pch=16, col="chartreuse2", cex=.7)
  
  
  legend(x=grconvertX(c(0.985,1.3), from='npc'), 
         y=grconvertY(c(0.6, 1.0), from='npc'), pch=16, bty="n", legend = c("0001", "0102", "0203", "0304", "0405", "0506", "0607", "0708","0809","0910", "1011", "1112", "1213", "1314", "1415"), col=c("cornflowerblue", "cyan4", "khaki", "deeppink4", "deeppink", "darkgrey", "lightblue", "red", "palevioletred", "darkgoldenrod1", "blueviolet", "lightgrey", "darkolivegreen4", "darkturquoise", "chartreuse2"), cex=.7, xpd=NA)

  
    
  plot(y00_15_wy_Tmp, y00_15_wy_Ppt, pch=c(21,22,23,24, 25), col="black", bg=col.rainbow, main=paste0(basin, " mean cumulative mean temp/precip (calendar year)"), xlab = "Mean temp", ylab = "Mean precip", cex=1.5,)
  
  
  legend(x=grconvertX(c(0.986,1.3), from='npc'), 
         y=grconvertY(c(0.6, 1.0), from='npc'), pch=c(21, 22, 23, 24, 25), bty="n", legend = c("0001", "0102", "0203", "0304", "0405", "0506", "0607", "0708","0809","0910", "1011", "1112", "1213", "1314", "1415"), col="black", pt.bg=col.rainbow, cex=.7, xpd=NA)
  