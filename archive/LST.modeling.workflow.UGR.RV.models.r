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
library(foreign)

  basin <- "UGR"
  longBasin <- "UpperGrandRonde"
  yrPath <- 14
  yearPath <- "2014"
  dataPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/climate_Analysis/"
  mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"

           

# ################################################################
#Logger data processing part
  
# ################################################################
  Alldata <- data.frame(mup=NULL)
  
  setwd(dataPath)
  
 
  ID.in <- read.csv(paste0(basin, "_RV_rcaID.csv"), stringsAsFactors=FALSE)
  colnames(ID.in)[5] <- "RCAID"
  
  newnamesnum <- as.numeric(colnames(LST.in)[1:46])
  ####This section is repeated for each year#######
  
  setwd(paste0(mainPath, longBasin))
  
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))  
  colnames(LST.in)<-gsub("X", "", colnames(LST.in))
  
  Log.in <- read.csv(paste0(basin, "_", yearPath, "_summer_daily_logger_data.csv"), stringsAsFactors=FALSE)
  IDs <- unique(ID.in$SiteName)
  SiteIDs <- Log.in$SiteName[Log.in$SiteName %in% IDs]
  SiteID <- unique(SiteIDs)
  SiteID <- as.matrix(SiteID)
  model.data <- data.frame(mup=NULL)
  LST.Log.out <- data.frame (mup = NULL)
  
  for (i in SiteID) 
    { 
      Log.site <- Log.in[Log.in$SiteName  == i,]
      Log.site <- as.data.frame(Log.site)
      
      if (dim(Log.site)[1] > 90)
        {
          RCAID <- ID.in$RCAID[ID.in$SiteName == i]
          Elev <- ID.in$Elev_M[ID.in$SiteName == i]
          Max <- max(Log.site$DailyMax)
          Mn <- mean(Log.site$DailyMn)
          MnRng <- mean(Log.site$DailyRange)
          AbRng <- Max - min(Log.site$DailyMin)
          LST.site <- unlist(LST.in[LST.in$RCAID == RCAID,23:33])
          mnLST <- mean(LST.site)
          mxLST <- max(LST.site)
          rngLST <- max(LST.site) - min(LST.site)
          year <- Log.site$Year[1]
          
          
          data <- data.frame(RCAID=RCAID, Elev=Elev, Max=Max, Mn=Mn, MnRng=MnRng, AbRng=AbRng, mnLST=mnLST, mxLST=mxLST, rngLST=rngLST, year=year, SiteName=i)
          model.data <- rbind(model.data, data)
         
        } 
    }
  
  
  Alldata <- rbind(Alldata, model.data)
  
# ################################################################
# This section reads in the formatted data from the temperature models
# and merges it with the RV data by site.
# ################################################################
  

  
  data.in <- Alldata
  
  
  LST.RV.out <- merge(Alldata, ID.in, by.x='SiteName', by.y='SiteName', all.x=TRUE)

 Data.means <- LST.RV.out
 colnames(Data.means)[2] <- "RCAID"
 
 setwd(dataPath)
 write.csv(Data.means, file=paste0(basin, "_RV_model_data_12_14.csv"))
 # ################
 # Adding in the NBCD data
 # ################
 
 setwd(dataPath)
 
 Data.means <- read.csv(paste0(basin, "RV_model_data_12_14.csv"))
 View(Data.means)
 setwd(paste0(dataPath, "NBCD/"))
 baw <- read.csv(paste0(basin, "_RCA_BAW.csv"))
 
 Data <- merge(Data.means, baw, by="RCAID", all.x=TRUE, all.y=FALSE)
 Data.means <- Data
 # ###############################
 # full year
 # ##################################
 
 y <- Data.means$Mn
 x <- Data.means$mnLST
 e <- Data.means$Elev
 evt <- Data.means$EVT
 depr <- Data.means$DepR
 bps <- Data.means$BPS
 year <- Data.means$year
 yMnRng <- Data.means$MnRng
 yAbsRng <- Data.means$AbRng
 xRng <- Data.means$rngLST
 yMax <- Data.means$Max
 xMax <- Data.means$mxLST
 baw <- Data.means$NBCD_BAW
 plot(evt, y)
 plot(bps, y)
 plot(depr, y)
 plot(baw, y)
 
 mod <- lm(y ~ x + e + evt + depr + bps + baw + year)
 sum_mod <- summary(mod)
 sum_mod
 
 mod3 <- lm(y ~ x + evt + year)
 sum_mod3 <- summary(mod3)
 sum_mod3
 
 mod4 <- lm(y ~ x + depr + year)
 sum_mod4 <- summary(mod4)
 sum_mod4
 
 mod5 <- lm(y ~ x + bps + year)
 sum_mod5 <- summary(mod5)
 sum_mod5
 
 mod6 <- lm(y ~ x + baw + year)
 sum_mod6 <- summary(mod6)
 sum_mod6
 
 mod7 <- lm(yMnRng ~ xRng + e + evt + depr + bps + baw + year)
 sum_mod7 <- summary(mod7)
 sum_mod7
 
 mod8 <- lm(yMnRng ~ xRng + evt + year)
 sum_mod8 <- summary(mod8)
 sum_mod8
 
 mod9 <- lm(yMnRng ~ xRng + depr + year)
 sum_mod9 <- summary(mod9)
 sum_mod9
 
 mod10 <- lm(yMnRng ~ xRng + bps + year)
 sum_mod10 <- summary(mod10)
 sum_mod10
 
 mod11 <- lm(yMnRng ~ xRng + baw + year)
 sum_mod11 <- summary(mod11)
 sum_mod11

 mod2 <- lm(yMax ~ xMax + e + evt + depr + bps + baw + year)
 sum_mod2 <- summary(mod2)
 sum_mod2
 
 mod12 <- lm(yMax ~ xMax + evt + year)
 sum_mod12 <- summary(mod12)
 sum_mod12
 
 mod13 <- lm(yMax ~ xMax + depr + year)
 sum_mod13 <- summary(mod13)
 sum_mod13
 
 mod14 <- lm(yMax ~ xMax + bps + year)
 sum_mod14 <- summary(mod14)
 sum_mod14
 
 mod15 <- lm(yMax ~ xMax + baw + year)
 sum_mod15 <- summary(mod15)
 sum_mod15
 
 mod16 <- lm(yAbsRng ~ xRng + e + evt + depr + bps + baw + year)
 sum_mod16 <- summary(mod16)
 sum_mod16
 
 mod17 <- lm(yAbsRng ~ xRng + evt + year)
 sum_mod17 <- summary(mod17)
 sum_mod17
 
 mod18 <- lm(yAbsRng ~ xRng + depr + year)
 sum_mod18 <- summary(mod18)
 sum_mod18
 
 mod19 <- lm(yAbsRng ~ xRng + bps + year)
 sum_mod19 <- summary(mod19)
 sum_mod19
 
 mod20 <- lm(yAbsRng ~ xRng + baw + year)
 sum_mod20 <- summary(mod20)
 sum_mod20
 

  ###################################
  
  
  pred.y <- predict(mod)
  plot(pred.y, y, main = "8-day Mean Full Year")
  abline(0,1)
  post_mod <- summary(lm(y ~ pred.y))
  gvmodel <- gvlma(mod)
  summary(gvmodel)
  plot(mod, which= 1:6)
  outlierTest(mod)
  qqPlot(mod, main="QQ Plot Full Year")
  spreadLevelPlot(mod)
  plot(pred.y, mod$residuals, main="Model diagnostics Full Year", xlab="Predicted", ylab="Residuals")
  
  pressstat_sum <- PRESS(mod, verbose = "FALSE")
  RMSEP <- sqrt(mean((pressstat_sum$residuals)^2))
  
  library(pls)
  mod2 <- plsr(y ~ x + I(x^2) + e, validation = "LOO")
  p2 <- R2(mod2)
  detach("package:pls", unload=TRUE)
  
  
  pred.out <- matrix(nrow=length(pred.y), ncol=5)
  pred.out[,1] <- y
  pred.out[,2] <- pred.y
  pred.out[,3] <- z
  pred.out[,4] <- "full year"
  pred.out[,5] <- yearPath
  colnames(pred.out) <- c("Y", "PredY", "JulDay", "Season", "Year")
  write.table (x=pred.out,append=T,row.names=F,file=paste0("jk_pred_v_y_Mean_", basin, "_", yearPath, "_full_year.csv"),sep = ",", col.names=F)  
  
  plot(pred.out[,1], pred.out[,2])
  summer_pred <- subset(pred.out, z > 181 & z < 258)
  points(summer_pred[, 1], summer_pred[,2], pch = 16, col = "green")
  abline(0,1)

  fit <- lm(y~ pred.y)
  plot(fit)
  summary(fit)
