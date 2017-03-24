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
library(rgdal)
library(raster)
library(rNOMADS)
library(gdalUtils)
library(data.table)


  basin <- "YF"
  midBasin <- "YankeeFork"
  longBasin <- "YankeeFork"
  dataPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/climate_Analysis/"
  mainPath <- "D:/OneDrive/work/research/CHaMP/"
  gisPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"
  subDir <- "GIS/LST/LST_s1"
  
  yrPath <- "14"
  year <- "2014"
  
######################################################################
# LST processing
######################################################################
  
  setwd(paste0(mainPath, "/", subDir, "_", year, "/"))
  
  LST.in <- read.dbf(paste0(basin, "_", yrPath, "_LST.dbf"))
  GrPolID <- LST.in[,1]
  LST.in <- LST.in[,2:47]
  LST.in[LST.in<0.1] = NA
  
  LST.names<-gsub("X13", "X", colnames(LST.in))
  
  tLST.in <- t(LST.in)
  tLST.out <- na.spline(tLST.in)
  CLST <- tLST.out*0.02-273.12
  tCLST <- t(CLST)
  CLST.out <- as.data.frame(tCLST)
  plot(1:46, CLST.out[10, 1:46])
  CLST.out$GRID_CODE <- GrPolID
  
  colnames(CLST.out)[1:46] <- LST.names
  
  
  setwd(paste0(mainPath, "CHaMP_data/", longBasin))  
  
  out_Preds <- read.dbf(paste0("LST", yrPath, "_", basin, "_interp.dbf")) 
  
  weights <- read.dbf(paste0(basin, "_rca_area_wgts.dbf"))
  weights[weights$area_wgts<0.01, "area_wgt"] = 0.01
  
  rcas <- unique(unlist(weights$rca_id))
  rca_zonal <- matrix(ncol=46, nrow=length(rcas))
  
  l <- 1
  
  for(i in rcas)  
    {
      pixels <- weights[weights$rca_id == i, "GRIDCODE"]
      wgts <- weights[weights$rca_id == i, "area_wgt"]
      
      for (j in 1:46)
        {
          daily <- out_Preds[out_Preds$GRID_CODE %in% pixels, j]
          zonal_mn <- weighted.mean(daily, wgts, na.rm = TRUE)
          rca_zonal[l,j] <- zonal_mn
        }
      l <- l+1
    }
  
  colnames(rca_zonal)[1:46] <- LST.names
  rca_zonal <- as.data.frame(rca_zonal)
  rca_zonal$RCAID <- rcas
  plot(1:46, rca_zonal[1,1:46])
  
  write.dbf(rca_zonal, file = paste0("LST", yrPath, "_", basin, "_RCA.dbf"))
  
  
#################################################################
#Logger data processing part
  
# ################################################################
  Alldata <- data.frame(mup=NULL)
  
  setwd(paste0(mainPath, "CHaMP_data/", longBasin))
  
  ID.in <- read.csv(paste0(basin, "_sites_elev.csv"), stringsAsFactors=FALSE)
  colnames(ID.in)[8] <- "Elev"
  
  elev.in <- read.csv(paste0(basin, "_rca_elev.csv"), stringsAsFactors=FALSE)
  
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))
  
  colnames(LST.in)<-gsub("X", "", colnames(LST.in))
  
  newnamesnum <- as.numeric(colnames(LST.in)[1:46])
  
  LST.in <- merge(LST.in, elev.in, by="RCAID")
  
  Log.in <- read.csv(paste0(basin, "_", year, "_summer_daily_logger.csv"), stringsAsFactors=FALSE)
  
  SiteID <- unique(Log.in$SiteName)
  SiteID <- as.matrix(SiteID)
  model.data <- data.frame(mup=NULL)
  LST.Log.out <- data.frame (mup = NULL)
  
  for (i in SiteID) 
    { 
      Log.site <- Log.in[Log.in$SiteName  == i,]
      Log.site <- Log.site[Log.site$JulianDate < 265 && Log.site$JulianDate > 173,]
      Log.site <- as.data.frame(Log.site)
      Log.site <- merge(Log.site, ID.in, by='SiteName')
      RCAID <- ID.in$RCAID[ID.in$SiteName == i]
      LST.site <- LST.in[LST.in$RCAID == RCAID,]
      
      
        Elev <- ID.in$Elev[ID.in$SiteName == i]
        Max <- max(Log.site$DailyMax)
        Mn <- mean(Log.site$DailyMn)
        MnRng <- mean(Log.site$DailyRange)
        AbRng <- Max - min(Log.site$DailyMin)
        LST.site <- unlist(LST.site[,25:35])
        mnLST <- mean(LST.site)
        mxLST <- max(LST.site)
        rngLST <- max(LST.site) - min(LST.site)
        SiteName <- i
        
        data <- data.frame(RCAID=RCAID, Elev=Elev, Max=Max, Mn=Mn, MnRng=MnRng, AbRng=AbRng, mnLST=mnLST, mxLST=mxLST, rngLST=rngLST, year=year, SiteName=SiteName)
        model.data <- rbind(model.data, data)
       
       
    }
  
  
  Alldata <- rbind(Alldata, model.data)
  
  Alldata$year <- as.character(Alldata$year)
  Alldata$year <- as.numeric(Alldata$year)
  Alldata$SiteName <- as.character(Alldata$SiteName)
  
# ################################################################
# This section reads in the formatted data from the temperature models
# and merges it with the RV data by site.
# ################################################################
  
  setwd(paste0(mainPath, longBasin))
  ID.in <- read.csv(paste0(basin, "_sites_elev.csv"), stringsAsFactors=FALSE)
  
  setwd(dataPath)
  RV.in <- read.csv(paste0(basin, "_site_RV.csv"), stringsAsFactors = FALSE)

  setwd(paste0(mainPath, longBasin, "/", year, "/"))
 
  #data.in <- Alldata
  #data.in <- read.csv(paste0(basin, "_", year, "_8Day_model_data.csv"), header=TRUE, stringsAsFactors=FALSE)
  #data.in <- merge(Alldata, RV.in, by= 'SiteName')


 write.csv(Alldata, file=paste0(basin, "RV_model_data_13_14.csv"), row.names=FALSE)
 
 Data.means <-Alldata 
 
# ################
# Adding in the NBCD, BPS-EVH & EVH data
# ################
 
setwd(dataPath)
 
 Data.means <- read.csv(paste0(basin, "RV_model_data_13_14.csv"), stringsAsFactors = FALSE)
 
 setwd(paste0(dataPath, "NBCD/"))
 baw <- read.csv(paste0(basin, "_RCA_BAW.csv"))
 
 Data <- merge(Data.means, baw, by="RCAID", all.x=TRUE, all.y=FALSE)
 Data.means <- Data
 
 
 

 
# ###############################
# all years
# ##################################

  y <- Data.means$Mn
  x <- Data.means$mnLST
  e <- Data.means$Elev
  
  year <- Data.means$year
  yMnRng <- Data.means$MnRng
  yAbsRng <- Data.means$AbRng
  xRng <- Data.means$rngLST
  yMax <- Data.means$Max
  xMax <- Data.means$mxLST
  baw <- Data.means$BAW
  #evh <- Data.means$EVH_VHm
  bps_evh <- Data.means$bps_evh
  
  plot(baw, y)
  plot(baw, yMax)
  
  
  mod <- lm(y ~ x + e + baw + year)
  sum_mod <- summary(mod)
  sum_mod
  
  
  mod2 <- lm(yMax ~ x + e + baw + year)
  sum_mod2 <- summary(mod2)
  sum_mod2
  
  mod6 <- lm(y ~ x + baw)
  sum_mod6 <- summary(mod6)
  sum_mod6
  

  mod11 <- lm(yMnRng ~ xRng + e + baw + year)
  sum_mod11 <- summary(mod11)
  sum_mod11
  
  
  mod15 <- lm(yMax ~ xMax + baw + year)
  sum_mod15 <- summary(mod15)
  sum_mod15
  
  mod15 <- lm(yMax ~ xMax + baw + e + year)
  sum_mod15 <- summary(mod15)
  sum_mod15
  
  mod20 <- lm(yAbsRng ~ xRng + e + baw + year)
  sum_mod20 <- summary(mod20)
  sum_mod20
  
  mod2 <- lm(yMax ~ x + baw)
  sum_mod2 <- summary(mod2)
  sum_mod2
  
###  
  mod21 <- lm(y ~ x + e + baw + year)
  sum_mod21 <- summary(mod21)
  sum_mod21
  
# ###############################
# pred
# ##################################
  
  y <- Data.means$Mn
  x <- Data.means$mnLST
  e <- Data.means$Elev
  rv <- Data.means$BAW
  year <- Data.means$year
  
  modRv <- lm(y ~ x + rv + e)
  sum_modRv <- summary(modRv)
  sum_modRv
  
  future <- cbind(x,bps_evh, year)
  colnames(future) <- c("x", "rv", "year")
  future <- as.data.frame(future)
  
  pred.future <- predict(lm(y~ x + rv + year), newdata=future)

  plot(y, pred.future)
  abline(0,1)
###############annual parse for looksies##############
  setwd(paste0(dataPath, "NBCD/YF_model_output/"))
  
  coeffs_out <- data.frame(Int=numeric(3), bLST=numeric(3), bBAW=numeric(3), bElev=numeric(3))
  metrics_out <- data.frame(r2=numeric(3), RMSE=numeric(3), p2=numeric(3), RMSEP=numeric(3), N=numeric(3), year=numeric(3))

  
  y2013 <- Data.means[Data.means$year == 2013,]
  y2014 <- Data.means[Data.means$year == 2014,]
  
 
############################
 
  
  mod6 <- lm(y ~ x + baw + e + year)
  
  sum_mod6 <- summary(mod6)
  sum_mod6
  
  mod <- mod6

  pred.y <- predict(mod)
  plot(pred.y, y, main = "Summer mean")
  abline(0,1)
  
  pressstat_sum <- PRESS(mod, verbose = "FALSE")
  RMSEP <- sqrt(mean((pressstat_sum$residuals)^2))
  
  library(pls)
  mod2 <- plsr(y ~ x + baw, validation = "LOO")
  p2 <- R2(mod2)
  detach("package:pls", unload=TRUE)
  
  
  pred.out <- matrix(nrow=length(pred.y), ncol=4)
  pred.out[,1] <- y
  pred.out[,2] <- pred.y
  pred.out[,3] <- "summer_mn"
  pred.out[,4] <- "Allyears"
  colnames(pred.out) <- c("Y", "PredY", "Season", "Year")
  write.table (x=pred.out,append=F,row.names=F,file=paste0("jk_pred_v_y_summer_mn_", basin, "_", "Allyears", ".csv"),sep = ",", col.names=F)  


  fit <- lm(y~ pred.y)
  summary(fit)

  
  coeffs <- as.matrix(coefficients(mod))
  
 
  coeffs_out[i,1] <- coeffs[1,1]
  coeffs_out[i,2] <- coeffs[2,1]
  coeffs_out[i,3] <- coeffs[3,1]
  coeffs_out[i,4] <- coeffs[4,1]
  
  
  metrics_out[i,1] <- sum_mod6$adj.r.squared
  metrics_out[i,2] <- sum_mod6$sigma
  metrics_out[i,3] <- p2$val[5]
  metrics_out[i,4] <- RMSEP
  metrics_out[i,5] <- length(y)
  metrics_out[i,6] <- "Allyears"



write.table(x=coeffs_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_coeffs_summer_Mn.csv"), sep = ",", col.names=T)

write.table(x=metrics_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_metrics_summer_Mn.csv"), sep = ",", col.names=T)

########Max########################


coeffs_out <- data.frame(Int=numeric(3), bLST=numeric(3), bBAW=numeric(3), bElev=numeric(3))
metrics_out <- data.frame(r2=numeric(3), RMSE=numeric(3), p2=numeric(3), RMSEP=numeric(3), N=numeric(3), year=numeric(3))

###############################
 
  mod6 <- lm(yMax ~ xMax + baw + e)
  
  sum_mod6 <- summary(mod6)
  sum_mod6
  
  mod <- mod6
######### model output #######################

  pred.y <- predict(mod)
  plot(pred.y, yMax, main = "Summer Max", ylim=c(10,20), xlim=c(10,20))
  abline(0,1)
  
  pressstat_sum <- PRESS(mod, verbose = "FALSE")
  RMSEP <- sqrt(mean((pressstat_sum$residuals)^2))
  
  library(pls)
  mod2 <- plsr(y ~ x + baw, validation = "LOO")
  p2 <- R2(mod2)
  detach("package:pls", unload=TRUE)


  pred.out <- matrix(nrow=length(pred.y), ncol=4)
  pred.out[,1] <- y
  pred.out[,2] <- pred.y
  pred.out[,3] <- "summer_max"
  pred.out[,4] <- "Allyears"
  colnames(pred.out) <- c("Y", "PredY", "Season", "Year")
  write.table (x=pred.out,append=F,row.names=F,file=paste0("jk_pred_v_y_summer_max_", basin, "_", "Allyears", ".csv"),sep = ",", col.names=F)  


  fit <- lm(y~ pred.y)
  summary(fit)


  coeffs <- as.matrix(coefficients(mod))
  
  coeffs_out[i,1] <- coeffs[1,1]
  coeffs_out[i,2] <- coeffs[2,1]
  coeffs_out[i,3] <- coeffs[3,1]
  coeffs_out[i,4] <- coeffs[4,1]


  metrics_out[i,1] <- sum_mod6$adj.r.squared
  metrics_out[i,2] <- sum_mod6$sigma
  metrics_out[i,3] <- p2$val[5]
  metrics_out[i,4] <- RMSEP
  metrics_out[i,5] <- length(y)
  metrics_out[i,6] <- "Allyears"



write.table(x=coeffs_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_coeffs_summer_Max.csv"), sep = ",", col.names=T)

write.table(x=metrics_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_metrics_summer_Max.csv"), sep = ",", col.names=T)

#####################################

coeffs_out <- data.frame(Int=numeric(3), bLST=numeric(3), bBAW=numeric(3), bElev=numeric(3), year=numeric(3))
metrics_out <- data.frame(r2=numeric(3), RMSE=numeric(3), p2=numeric(3), RMSEP=numeric(3), N=numeric(3), year=numeric(3))

############ year parse ###################
mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"

  i <- 2  
  year <- 2013
  y <- y2013$Mn
  x <- y2013$mnLST
  baw <- y2013$BAW
  e <- y2013$Elev
  
  mod6 <- lm(y ~ x + baw)
  
  sum_mod6 <- summary(mod6)
  sum_mod6
  
  mod <- mod6
################################

  pred.y <- predict(mod)
  plot(pred.y, y, main = "Summer Mean")
  abline(0,1)
  
  pressstat_sum <- PRESS(mod, verbose = "FALSE")
  RMSEP <- sqrt(mean((pressstat_sum$residuals)^2))
  
  library(pls)
  mod2 <- plsr(y ~ x + baw + e, validation = "LOO")
  p2 <- R2(mod2)
  detach("package:pls", unload=TRUE)
  
  
  pred.out <- matrix(nrow=length(pred.y), ncol=4)
  pred.out[,1] <- y
  pred.out[,2] <- pred.y
  pred.out[,3] <- "summer_mean"
  pred.out[,4] <- year
  colnames(pred.out) <- c("Y", "PredY", "Season", "Year")
  write.table (x=pred.out,append=F,row.names=F,file=paste0("jk_pred_v_y_summer_mean_", basin, "_", year, ".csv"),sep = ",", col.names=F)  
  
  
  fit <- lm(y~ pred.y)
  summary(fit)
  
  
  coeffs <- as.matrix(coefficients(mod))
  
  coeffs_out[i,1] <- coeffs[1,1]
  coeffs_out[i,2] <- coeffs[2,1]
  coeffs_out[i,3] <- coeffs[3,1]
  coeffs_out[i,4] <- coeffs[4,1]
  coeffs_out[i,5] <- year
  
  
  metrics_out[i,1] <- sum_mod6$adj.r.squared
  metrics_out[i,2] <- sum_mod6$sigma
  metrics_out[i,3] <- p2$val[5]
  metrics_out[i,4] <- RMSEP
  metrics_out[i,5] <- length(y)
  metrics_out[i,6] <- year



write.table(x=coeffs_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_coeffs_summer_Mn.csv"), sep = ",", col.names=T)

write.table(x=metrics_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_metrics_summer_Mn.csv"), sep = ",", col.names=T)


########################################################################################################
# This part applies the model coefficients to the LST to generate stream temp estimates
# #######################################################################################################
  setwd(paste0(mainPath, longBasin))
  
  ID.in <- read.csv(paste0(basin, "_sites_elev.csv"), stringsAsFactors=FALSE)
  colnames(ID.in)[5] <- "RCAID"
  colnames(ID.in)[4] <- "Elev"
  
  yrPath <- 13
  year <- 2013
  
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))
  
  colnames(LST.in)<-gsub("X", "", colnames(LST.in))
  
  LST.in <- merge(LST.in, elev.in, by="RCAID")
  
  LST.sum <- as.data.frame(apply(LST.in[,25:35], 1, mean))
  colnames(LST.sum)[1] <- "LST.mn"
  LST.sum$RCAID <- LST.in$RCAID
  
setwd(paste0(dataPath, "NBCD/"))
  baw <- read.csv(paste0(basin, "_RCA_BAW.csv"))
  
  LST.baw <- merge(LST.sum, baw, by="RCAID", all.x=TRUE, all.y=FALSE)
  colnames(LST.baw)[3] <- "baw"
  LST.baw$Elev <- LST.in$Elev
  LST.future.baw <- LST.baw[,c(1,2,4,5)]
  
  LST.baw <- LST.baw[,-4]
  
setwd(paste0(dataPath, "NBCD/YF_model_output/"))

  coeffs.in.mn <- read.csv(paste0("BAW_models_", basin, "_mod_coeffs_summer_Mn.csv"), stringsAsFactors=FALSE)
  
  LogPred.out <- LST.baw
  LogPred.out[,2] <- 0
  colnames(LogPred.out)[2] <- "Summ.mn"
  LogPred.out.future <- LST.future.baw
  LogPred.out.future[,2] <- 0
  colnames(LogPred.out.future)[2] <- "Summ.mn"
  

      
    i <- 2 
    LogPred.out[,2] <- (apply(LST.baw, 1, function(x) x[2] * coeffs.in.mn$bLST[i] + x[3] * coeffs.in.mn$bBAW[i] + x[4] * coeffs.in.mn$bElev[i] + coeffs.in.mn$Int[i]))
      
    LogPred.out.future[,2] <- (apply(LST.future.baw, 1, function(x) x[2] * coeffs.in.mn$bLST[i] + x[3] * coeffs.in.mn$bBAW[i] + x[4] * coeffs.in.mn$bElev[i] + coeffs.in.mn$Int[i]))
    
    

write.dbf(LogPred.out, file = paste0("predt", year, "_", basin, "_summer_mn_current.dbf")) 
write.dbf(LogPred.out.future, file = paste0("predt", year, "_", basin, "_summer_mn_RV_potent.dbf")) 



########################################

  yrPath <- 13
  year <- 2013


setwd(paste0(dataPath, "NBCD/", basin, "_model_output/"))
  LogPred.out.mn <- read.dbf(paste0("predt", year, "_", basin, "_summer_mn_current.dbf"))
  colnames(LogPred.out.mn)[2] <- "Current"
  LogPred.out.mn.potent <- read.dbf(paste0("predt", year, "_", basin, "_summer_mn_RV_potent.dbf"))
  colnames(LogPred.out.mn.potent)[2] <- "Potential"
  #LogPred.out.mx <- read.dbf(paste0("predt", year, "_", basin, "_summer_max_current.dbf"))
  #LogPred.out.mx.potent <- read.dbf(paste0("predt", year, "_", basin, "_summer_max_RV_potent.dbf"))
  convType <- read.csv(paste0(basin, "_RCA_CONVTYPE.csv"), stringsAsFactors = FALSE)
  SO <- read.csv(paste0(basin, "_RCA_SO.csv"), stringsAsFactors = FALSE)
  
  RV_plot.mn <- merge(LogPred.out.mn, LogPred.out.mn.potent, by="RCAID")
  
  
#################### max ######################
  RV_plot.mx <- as.data.frame(LogPred.out.mx$Summ_max)
  RV_plot.mx$RCAID <- LogPred.out.mx$RCAID
  colnames(RV_plot.mx)[1] <- "Current"
  RV_plot.mx$Potential <- LogPred.out.mx.potent$Summ_max
##################################################
  
 
  RV_plot.mn <- merge(RV_plot.mn, SO, by="RCAID", all.x=FALSE, all.y=FALSE)
  RV_plot.mn <- RV_plot.mn[,-4]
  
############## mean #################
#### This part takes the weighted mean by stream length of the different conversion types.####
 
  convType$value <- as.factor(convType$RCAID)
  DT <- data.table(convType)
  test <- DT[,list(noCh = weighted.mean(prop_noch,Shape_Length)),by=value]
  test2 <- DT[,list(grSh = weighted.mean(prop_grsh,Shape_Length)),by=value]
  test3 <- DT[,list(deVeg = weighted.mean(prop_deveg,Shape_Length)),by=value]
  test4 <- DT[,list(Con = weighted.mean(prop_con,Shape_Length)),by=value]
  test5 <- DT[,list(Inv = weighted.mean(prop_inv,Shape_Length)),by=value]
  test6 <- DT[,list(Dev = weighted.mean(prop_dev,Shape_Length)),by=value]
  test7 <- DT[,list(Ag = weighted.mean(prop_ag,Shape_Length)),by=value]
  
  test.out <- merge(test, test2,by="value")
  ############# cycle thru test3:7 ###############
  test.out <- merge(test.out, test7,by="value")
  test.out <- as.data.frame(test.out)
  
  maxConv <- apply(test.out[2:8], 1, function(x) colnames(test.out)[which.max(x)+1])
  
  test.out$maxConv <- maxConv
  sConv <- apply(test.out[3:8], 1, function(x) colnames(test.out)[which.max(x)+2])
  test.out$someConv2 <- ifelse(test.out$noCh < 0.7, sConv, "noCh") 
  
  test.out$RCAID <- as.numeric(test.out$value)
  test.out <- test.out[,-1]
  Data.mn <- merge(RV_plot.mn, test.out, by="RCAID", all.x=FALSE, all.y=FALSE)
  Data.mn$delta <- Data.mn$Current - Data.mn$Potential
  
########### max Conversion type #############  
  
  Data.mn$Color="white"

  
  Data.mn$Color[Data.mn$maxConv =="Con"]="blue"
  #Data.mn$Color[Data.mn$conv_type =="Minor Conversion to Grass/Shrubland"]="lightgreen"
 # Data.mn$Color[Data.mn$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Data.mn$Color[Data.mn$maxConv =="grSh"]="chartreuse4"
  #Data.mn$Color[Data.mn$conv_type =="Multiple Dominant Conversion Types"]="darkviolet"
  plot(Data.mn$Current, Data.mn$Potential, pch=21, col="black", bg=Data.mn$Color, cex=1.5, main = paste0(longBasin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(8,13.5), ylim=c(8,13.5))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=.7, xpd=NA)

  ###################### "some" conversion type ############
  
  Data.mn$Color="white"
  
  
  Data.mn$Color[Data.mn$someConv =="Con"]="blue"
  #Data.mn$Color[Data.mn$conv_type =="Minor Conversion to Grass/Shrubland"]="lightgreen"
  # Data.mn$Color[Data.mn$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Data.mn$Color[Data.mn$someConv =="grSh"]="chartreuse4"
  #Data.mn$Color[Data.mn$conv_type =="Multiple Dominant Conversion Types"]="darkviolet"
  plot(Data.mn$Current, Data.mn$Potential, pch=21, col="black", bg=Data.mn$Color, cex=1.5, main = paste0(longBasin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(8,13.5), ylim=c(8,13.5))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=.7, xpd=NA)
  
###################### orig conversion type ############
  Data.orig <- merge(RV_plot.mn, convType, by="RCAID")
  Data.orig$Color="white"
  
  Data.orig$Color[Data.orig$conv_type =="Minor Conifer Encroachment"]="cadetblue1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conifer Encroachment"]="dodgerblue"
  Data.orig$Color[Data.orig$conv_type =="Significant Conifer Encroachment"]="blue"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Grass/Shrubland"]="darkolivegreen1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Grass/Shrubland"]="chartreuse4"
  Data.orig$Color[Data.orig$conv_type =="Multiple Dominant Conversion Types"]="darkviolet"
  Data.orig$Color[Data.orig$conv_type =="Minor Devegetation"]="darkred"
  Data.orig$Color[Data.orig$conv_type =="Multiple Dominant Conversion Types"]="darkred"
  Data.orig$Color[Data.orig$conv_type =="Significant Devegetation"]="azure4"
  plot(Data.orig$Current, Data.orig$Potential, pch=21, col="black", bg=Data.orig$Color, cex=1.5, main = paste0(longBasin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(8,13.5), ylim=c(8,13.5))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=.7, xpd=NA)
  
 ############### Box plot max Conversion type#######################

  
  
  Data.mn$ConT_lab="NoC"
  Data.mn$ConT_lab[Data.mn$maxConv =="Con"]="ConEn"
  Data.mn$ConT_lab[Data.mn$maxConv =="grSh"]="grSH"

  boxplot(delta ~ SO, data = Data.mn, main = paste0(basin," ", year, " change in summer mn temp by stream order"), xlab = "Stream order", ylab = "Current - Potential", ylim=c(-3,3))
  boxplot(delta ~ ConT_lab, data = Data.mn, col=c("blue", "chartreuse4", "white", ), main = paste0(basin, " ", year," change in summer mn temp by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-3,3))
  abline(0,0, lty = 2)
 
   ############### Box plot some Conversion type#######################
  
  
  
  Data.mn$ConT_lab="NoC"
  Data.mn$ConT_lab[Data.mn$someConv =="Con"]="ConEn"
  Data.mn$ConT_lab[Data.mn$someConv =="grSh"]="grSH"
  boxplot(delta ~ ConT_lab, data = Data.mn, col=c("blue", "chartreuse4", "white", "blueviolet", "white", "lightgreen"), main = paste0(basin, " ", year," change in summer mn temp by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-3,3))
  abline(0,0, lty = 2)
  
setwd(paste0(dataPath, "NBCD/", basin, "_model_output/"))
  write.csv(Data.mn, file = paste0("predt", year, "_", basin, "_summer_mean_change_by_type.csv"), row.names = FALSE)

############### subset by SO &/or reach ##############
  
  mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"
  setwd(paste0(mainPath, longBasin)) 
  
  stNames <- read.csv(paste0(basin, "_reach_names.csv"), stringsAsFactors = FALSE)
  hiOrder <- Data.mn[ which(Data.mn$SO > 2), ]
  hiOrder$ptSz <- 1.5
  hiOrder$ptSz[hiOrder$someConv =="noCh"]= 0.8
  
  plot(hiOrder$Current, hiOrder$Potential, pch=21, col="black", bg=hiOrder$Color, cex=hiOrder$ptSz, main = paste0(basin, " summer mn stream temp SO 3+ ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(8,13.5), ylim=c(8,13.5))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=0.7, xpd=NA)
  
  boxplot(delta ~ ConT_lab, data = hiOrder, col=c("blue", "chartreuse4", "white", "blueviolet", "white", "lightgreen"), main = paste0(basin, " ", year," 3+ SO delta (summer mn) by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-2,2))
  abline(0,0, lty = 2)
  
###  upper main stem YF ####
  
  Data.mn <- merge(Data.mn, stNames, by="RCAID")
  
  mainStem <- Data.mn[ which(Data.mn$GNIS_NAME == "Yankee Fork" | Data.mn$GNIS_NAME == "Adair Creek" | Data.mn$GNIS_NAME == "Blind Creek" | Data.mn$GNIS_NAME == "Cearley Creek" | Data.mn$GNIS_NAME == "Eightmile Creek" | Data.mn$GNIS_NAME == "Elevenmile Creek" | Data.mn$GNIS_NAME == "Estes Creek" | Data.mn$GNIS_NAME == "Fivemile Creek" | Data.mn$GNIS_NAME == "Fourth of July Creek" | Data.mn$GNIS_NAME == "Greylock Creek" | Data.mn$GNIS_NAME == "Jerrys Creek" | Data.mn$GNIS_NAME == "Jordan Creek" | Data.mn$GNIS_NAME == "Ninemile Creek" | Data.mn$GNIS_NAME == "Park Creek" | Data.mn$GNIS_NAME == "Polecamp Creek" | Data.mn$GNIS_NAME == "Ramney Creek" | Data.mn$GNIS_NAME == "Rankin Creek" | Data.mn$GNIS_NAME == "Sevenmile Creek" | Data.mn$GNIS_NAME == "Silver Creek" | Data.mn$GNIS_NAME == "Sixmile Creek" | Data.mn$GNIS_NAME == "Tenmile Creek" | Data.mn$GNIS_NAME == "Twelvemile Creek"), ]
  mainStem <- mainStem[mainStem$RCAID != 56,]
  mainStem <- mainStem[mainStem$GNIS_NAME == "Yankee Fork",]
  
  mainStem$ptSz <- 1.5
  mainStem$ptSz[mainStem$someConv =="noCh"]= 0.8
  
  plot(mainStem$Current, mainStem$Potential, pch=21, col="black", bg=mainStem$Color, cex=mainStem$ptSz, main = paste0(basin, " upper main stem summer mn stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(8,13.5), ylim=c(8,13.5))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=.7, xpd=NA)
  
  mainStem$ConT_lab="NoC"
  mainStem$ConT_lab[mainStem$someConv =="Con"]="ConEn"
  mainStem$ConT_lab[mainStem$someConv =="grSh"]="grSH"
  
  boxplot(delta ~ ConT_lab, data = mainStem, col=c("blue", "chartreuse4", "white"), main = paste0(basin, " ", year," upper main stem delta (summer mn) by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-2,2))
  abline(0,0, lty = 2)

###  lower  main stem YF ####
   LFmainStem <- subset(Data.mn, RCAID==1 | RCAID==2 | RCAID==3 | RCAID==4 | RCAID==5 | RCAID==53 | RCAID==54 | RCAID==55 | RCAID==56)
  
  LFmainStem$ptSz <- 1.5
  LFmainStem$ptSz[LFmainStem$someConv =="noCh"]= 0.8
  
  LFmainStem$Color="white"
  LFmainStem$Color[LFmainStem$someConv =="Con"]="blue"
  LFmainStem$Color[LFmainStem$someConv =="grSh"]="chartreuse3"
  plot(LFmainStem$Current, LFmainStem$Potential, pch=21, col="black", bg=LFmainStem$Color, cex=LFmainStem$ptSz, main = paste0(basin, " LF mainstem summer mn stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(8,13.5), ylim=c(8,13.5))
  abline(0,1)
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=.7, xpd=NA)
  
  LFmainStem$ConT_lab="NoC"
  LFmainStem$ConT_lab[LFmainStem$someConv =="Con"]="ConEn"
  LFmainStem$ConT_lab[LFmainStem$someConv =="grSh"]="grSH"
  
  boxplot(delta ~ ConT_lab, data = LFmainStem, col=c("blue", "chartreuse4", "white"), main = paste0("LF ", basin, " ", year," delta (summer mn) by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-2,2))
  abline(0,0, lty = 2)
###  West Fork YF #####
  
  Wfork <- Data.mn[ which(Data.mn$GNIS_NAME == "West Fork Yankee Fork" | Data.mn$GNIS_NAME == "Cabin Creek" | Data.mn$GNIS_NAME == "Deadwood Creek" | Data.mn$GNIS_NAME == "Lightning Creek" | Data.mn$GNIS_NAME == "Sawmill Creek"), ]
  Wfork$ptSz <- 1.5
  Wfork$ptSz[Wfork$someConv =="noCh"]= 0.8
  
  plot(Wfork$Current, Wfork$Potential, pch=21, col="black", bg=Wfork$Color, cex=Wfork$ptSz, main = paste0("West Fork ", basin, " summer mn stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(9,13), ylim=c(9,13))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "blue", "chartreuse4"), cex=.7, xpd=NA)
  
  boxplot(delta ~ ConT_lab, data = Wfork, col=c("blue", "chartreuse4", "white"), main = paste0("West Fork", basin, " ", year," delta (summer mn) by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-2,2))
  abline(0,0, lty = 2)
  
  boxplot(delta ~ SO, data = Wfork, main = paste0("WF ", basin," ", year, " change in summer mn temp by stream order"), xlab = "Stream order", ylab = "Current - Potential", ylim=c(-3,3))
  abline(0,0, lty = 2)
  
  Wfork.origCT <- merge(Wfork, convType, by="RCAID")
  
  Wfork.origCT$ptSz <- 1.5
  Wfork.origCT$ptSz[Wfork.origCT$conv_type =="No Change"]= 0.8
  
  Wfork.origCT$Color="white"
  
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Minor Conifer Encroachment"]="cadetblue1"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Moderate Conifer Encroachment"]="dodgerblue"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Significant Conifer Encroachment"]="blue"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Minor Conversion to Grass/Shrubland"]="darkolivegreen1"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Significant Conversion to Grass/Shrubland"]="chartreuse4"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Multiple Dominant Conversion Types"]="darkred"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Minor Devegetation"]="azure"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Moderate Devegetation"]="azure3"
  Wfork.origCT$Color[Wfork.origCT$conv_type =="Significant Devegetation"]="azure4"
  
  plot(Wfork.origCT$Current, Wfork.origCT$Potential, pch=21, col="black", bg=Wfork.origCT$Color, cex=Wfork.origCT$ptSz, main = paste0("WF ", basin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(9,13), ylim=c(9,13))
  abline(0,1)
  
  legend("topleft", pch=c(21,16,16,16,16), bty="n", legend = c("No Change", "Conifer Encroachment", "Devegetation", "Grass/shrub", "Multiple Dominant Types"), col=c("black", "dodgerblue", "azure4", "chartreuse3", "darkred"), cex=.75, xpd=NA)
  
### West Fork YF 3rd & 4th order streams only #############
  
  Wfork34 <- Wfork.origCT[Wfork.origCT$SO > 2,]
  
  plot(Wfork34$Current, Wfork34$Potential, pch=21, col="black", bg=Wfork34$Color, cex=Wfork34$ptSz, main = paste0("West Fork ", basin, " 3rd+ summer mn stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(9,13), ylim=c(9,13))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub"), col="black", pt.bg=c("white", "dodgerblue", "chartreuse3"), cex=.7, xpd=NA)
  
  boxplot(delta ~ ConT_lab, data = Wfork, col=c("blue", "chartreuse4", "white"), main = paste0("West Fork", basin, " ", year," delta (summer mn) by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-2,2))
  abline(0,0, lty = 2)
  
###
  
  fullConT <- merge(test.out, )
write.csv(Data.mx, file = paste0("predt", year, "_", basin, "_summer_max_change_by_type.csv"))

