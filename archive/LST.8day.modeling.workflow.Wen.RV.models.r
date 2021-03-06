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

  basin <- "Wen"
  midBasin <- "Wenatchee"
  longBasin <- "Wenatchee"
  dataPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/climate_Analysis/"
  mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"
  gisPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"
  
#################################################################
#Logger data processing part
  
# ################################################################
  Alldata <- data.frame(mup=NULL)
  
  setwd(paste0(mainPath, longBasin))
  
  ID.in <- read.csv(paste0(basin, "_sites_elev.csv"), stringsAsFactors=FALSE)
  colnames(ID.in)[5] <- "RCAID"
  colnames(ID.in)[4] <- "Elev"
  
  yrPath <- "14"
  year <- "2014"
  
  LST.in <- read.csv(paste0("LST", yrPath, "_", basin, "_RCA.csv"), stringsAsFactors = F)
                            ########### OR ###########
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))
  
  colnames(LST.in)<-gsub("X", "", colnames(LST.in))
  
  newnamesnum <- as.numeric(colnames(LST.in)[1:46])
  
  
  Log.in <- read.csv(paste0(basin, "_", year, "_summer_daily_logger.csv"), stringsAsFactors=FALSE)
  
  SiteID <- unique(Log.in$SiteName)
  SiteID <- as.matrix(SiteID)
  model.data <- data.frame(mup=NULL)
  LST.Log.out <- data.frame (mup = NULL)
  
  for (i in SiteID) 
    { 
      Log.site <- Log.in[Log.in$SiteName  == i,]
      Log.site <- as.data.frame(Log.site)
      
      if (dim(Log.site)[1] > 90)
      {
        RCAID <- ID.in$RCAID[ID.in$SiteID == i]
        Elev <- ID.in$Elev[ID.in$SiteID == i]
        Max <- max(Log.site$DailyMax)
        Mn <- mean(Log.site$DailyMn)
        MnRng <- mean(Log.site$DailyRange)
        AbRng <- Max - min(Log.site$DailyMin)
        LST.site <- unlist(LST.in[LST.in$RCAID == RCAID,23:33])
        mnLST <- mean(LST.site)
        mxLST <- max(LST.site)
        rngLST <- max(LST.site) - min(LST.site)
        precip <- y01_15_ppt[14]
        
        data <- data.frame(RCAID=RCAID, Elev=Elev, Max=Max, Mn=Mn, MnRng=MnRng, AbRng=AbRng, mnLST=mnLST, mxLST=mxLST, rngLST=rngLST, precip=precip, year=year)
        model.data <- rbind(model.data, data)
       
      } 
    }
  
  
  Alldata <- rbind(Alldata, model.data)
  
# ################################################################
# This section reads in the formatted data from the temperature models
# and merges it with the RV data by site.
# ################################################################
  
  setwd(paste0(mainPath, longBasin))
  ID.in <- read.csv(paste0(basin, "_sites_elev.csv"), stringsAsFactors=FALSE)
  
  setwd(dataPath)
  RV.in <- read.dbf(paste0(basin, "_RV_RCA.dbf"))

  setwd(paste0(mainPath, longBasin, "/", year, "/"))
 
  data.in <- Alldata
  #data.in <- read.csv(paste0(basin, "_", year, "_8Day_model_data.csv"), header=TRUE, stringsAsFactors=FALSE)
  

  RVID <- unique(RV.in$RV_reachID)
  LST.RV.out <- data.frame (mup = NULL)
  rcas <- unique(data.in$RCAID)
  RVID <- unique(RV.in$rca_id)
  
    for (i in RVID) 
      { 
        
        RCAID <- i
        
        if(RCAID %in% rcas)
          
          {
              data <- data.in[data.in$RCAID == RCAID,]
              data$EVT <- RV.in$EVT_MEAN[RV.in$RV_reachID == i]
              data$BPS <- RV.in$BPS_MEAN[RV.in$RV_reachID == i]
              data$DepR <- RV.in$DEP_RATIO[RV.in$RV_reachID == i]
              data$ConvC <- RV.in$CONV_CODE[RV.in$RV_reachID == i]
              data$ConvT <- RV.in$CONV_TYPE[RV.in$RV_reachID == i]
              
              LST.RV.out <- rbind(LST.RV.out, data)
          }
          
      }

    
 Data.means <- LST.RV.out   
 write.csv(Data.means, file=paste0(basin, "RV_model_data_12_14.csv"))
# ################
# Adding in the NBCD, BPS-EVH & EVH data
# ################
 
setwd(dataPath)
 
 Data.means <- read.csv(paste0(basin, "RV_model_data_12_14.csv"), stringsAsFactors = FALSE)
 
 setwd(paste0(dataPath, "NBCD/"))
 baw <- read.csv(paste0(basin, "_RCA_BAW.csv"))
 
 Data <- merge(Data.means, baw, by="RCAID", all.x=TRUE, all.y=FALSE)
 Data.means <- Data
 
 setwd(paste0(dataPath, "EVH/"))
 evh <- read.csv(paste0(basin, "_RCA_EVH.csv"))
 
 Data <- merge(Data.means, evh, by="RCAID", all.x=TRUE, all.y=FALSE)
 Data.means <- Data
 
 setwd(paste0(dataPath, "BPS/"))
 bps_evh <- read.csv(paste0(basin, "_BpS_EVH.csv"))
 
 Data <- merge(Data.means, bps_evh, by="RCAID", all.x=TRUE, all.y=FALSE)
 
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
  year <- Data.means$precip
  yMnRng <- Data.means$MnRng
  yAbsRng <- Data.means$AbRng
  xRng <- Data.means$rngLST
  yMax <- Data.means$Max
  xMax <- Data.means$mxLST
  baw <- Data.means$NBCD_BAW
  evh <- Data.means$EVH_VHm
  bps_evh <- Data.means$BpS_evh
  plot(evt, y)
  plot(baw, y)
  plot(evh, y)
  plot(bps_evh, y)
  
  mod <- lm(y ~ x + e + evt + depr + bps + baw + year)
  sum_mod <- summary(mod)
  sum_mod
  
  
  mod2 <- lm(yMax ~ xMax + e + evt + depr + bps + baw + year)
  sum_mod2 <- summary(mod2)
  sum_mod2
  
  mod3 <- lm(y ~ x + evt + year)
  sum_mod3 <- summary(mod3)
  sum_mod3
  
  mod4 <- lm(y ~ x + depr + year)
  sum_mod4 <- summary(mod4)
  sum_mod4
  
  mod5 <- lm(y ~ x + bps + year)
  sum_mod5 <- summary(mod5)
  sum_mod5
  
  mod6 <- lm(y ~ x + baw)
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
###  
  mod21 <- lm(y ~ x + e + baw + year)
  sum_mod21 <- summary(mod21)
  sum_mod21
  
  mod22 <- lm(yMax ~ xMax + evh + year)
  sum_mod22 <- summary(mod22)
  sum_mod22
  
  mod23 <- lm(y ~ x + e + bps_evh + year)
  sum_mod23 <- summary(mod23)
  sum_mod23
  
  mod22 <- lm(yMax ~ xMax + bps_evh + year)
  sum_mod22 <- summary(mod22)
  sum_mod22
# ###############################
# pred
# ##################################
  
  y <- Data.means$Mn
  x <- Data.means$mnLST
  e <- Data.means$Elev
  rv <- Data.means$BAW
  year <- Data.means$year
  
  modRv <- lm(y ~ x + rv)
  sum_modRv <- summary(modRv)
  sum_modRv
  
  future <- cbind(x,bps_evh, year)
  colnames(future) <- c("x", "rv", "year")
  future <- as.data.frame(future)
  
  pred.future <- predict(lm(y~ x + rv + year), newdata=future)



###############annual parse for looksies##############
  setwd(paste0(dataPath, "NBCD/Wen_model_output/"))
  
  coeffs_out <- data.frame(Int=numeric(3), bLST=numeric(3), BAW=numeric(3), year=numeric(3))
  metrics_out <- data.frame(r2=numeric(3), RMSE=numeric(3), p2=numeric(3), RMSEP=numeric(3), N=numeric(3), year=numeric(3))

  y2012 <- Data.means[Data.means$year == 2012,]
  y2013 <- Data.means[Data.means$year == 2013,]
  y2014 <- Data.means[Data.means$year == 2014,]
  
 
############################
  i <- 3
  year <- 2014
  y <- y2014$Mn
  
  x <- y2014$mnLST
  baw <- y2014$NBCD_BAW
  mod6 <- lm(y ~ x + baw)
  
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
  pred.out[,4] <- year
  colnames(pred.out) <- c("Y", "PredY", "Season", "Year")
  write.table (x=pred.out,append=F,row.names=F,file=paste0("jk_pred_v_y_summer_mn_", basin, "_", year, ".csv"),sep = ",", col.names=F)  


  fit <- lm(y~ pred.y)
  summary(fit)

  
  coeffs <- as.matrix(coefficients(mod))
  
 
  coeffs_out[i,1] <- coeffs[1,1]
  coeffs_out[i,2] <- coeffs[2,1]
  coeffs_out[i,3] <- coeffs[3,1]
  coeffs_out[i,4] <- year
  
  
  metrics_out[i,1] <- sum_mod6$adj.r.squared
  metrics_out[i,2] <- sum_mod6$sigma
  metrics_out[i,3] <- p2$val[5]
  metrics_out[i,4] <- RMSEP
  metrics_out[i,5] <- length(y)
  metrics_out[i,6] <- year



write.table(x=coeffs_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_coeffs_summer_Mn.csv"), sep = ",", col.names=T)

write.table(x=metrics_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_metrics_summer_Mn.csv"), sep = ",", col.names=T)

########Max########################


coeffs_out <- data.frame(Int=numeric(3), bLST=numeric(3), BAW=numeric(3), year=numeric(3))
metrics_out <- data.frame(r2=numeric(3), RMSE=numeric(3), p2=numeric(3), RMSEP=numeric(3), N=numeric(3), year=numeric(3))

###############################
  i <- 3  
  year <- 2014
  y <- y2014$Max
  x <- y2014$mxLST
  baw <- y2014$NBCD_baw
  e <- y2014$Elev
  
  mod6 <- lm(y ~ x + baw + e)
  
  sum_mod6 <- summary(mod6)
  sum_mod6
  
  mod <- mod6
########### model output #####################

  pred.y <- predict(mod)
  plot(pred.y, y, main = "Summer Max")
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
  pred.out[,4] <- year
  colnames(pred.out) <- c("Y", "PredY", "Season", "Year")
  write.table (x=pred.out,append=F,row.names=F,file=paste0("jk_pred_v_y_summer_max_", basin, "_", year, ".csv"),sep = ",", col.names=F)  


  fit <- lm(y~ pred.y)
  summary(fit)


  coeffs <- as.matrix(coefficients(mod))
  
  coeffs_out[i,1] <- coeffs[1,1]
  coeffs_out[i,2] <- coeffs[2,1]
  coeffs_out[i,3] <- coeffs[3,1]
  coeffs_out[i,4] <- year


  metrics_out[i,1] <- sum_mod6$adj.r.squared
  metrics_out[i,2] <- sum_mod6$sigma
  metrics_out[i,3] <- p2$val[5]
  metrics_out[i,4] <- RMSEP
  metrics_out[i,5] <- length(y)
  metrics_out[i,6] <- year



write.table(x=coeffs_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_coeffs_summer_Max.csv"), sep = ",", col.names=T)

write.table(x=metrics_out, append=F,row.names=T, file = paste0("BAW_models_", basin, "_mod_metrics_summer_Max.csv"), sep = ",", col.names=T)


########################################################################################################
# This part applies the model coefficients to the LST to generate 8-day temp estimates for 1 July - 30 Sept
# #######################################################################################################
  setwd(paste0(mainPath, longBasin))
  
  ID.in <- read.csv(paste0(basin, "_sites_elev.csv"), stringsAsFactors=FALSE)
  colnames(ID.in)[5] <- "RCAID"
  colnames(ID.in)[4] <- "Elev"
  
  yrPath <- 14
  year <- 2014
  
  LST.in <- read.csv(paste0("LST", yrPath, "_", basin, "_RCA.csv"), stringsAsFactors = F)
  ########### OR ###########
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))
  
  colnames(LST.in)<-gsub("X", "", colnames(LST.in))
  
  newnamesnum <- as.numeric(colnames(LST.in)[1:46])
  
  LST.sum <- as.data.frame(apply(LST.in[,22:33], 1, mean))
  colnames(LST.sum)[1] <- "LST.mn"
  LST.sum$RCAID <- LST.in$RCAID
  
setwd(paste0(dataPath, "NBCD/"))
  baw <- read.csv(paste0(basin, "_RCA_BAW.csv"))
  
  LST.baw <- merge(LST.sum, baw, by="RCAID", all.x=TRUE, all.y=FALSE)
  colnames(LST.baw)[3] <- "baw"

setwd(paste0(dataPath, "BPS/"))
  bps_evh <- read.csv(paste0(basin, "_BpS_EVH.csv"))
  
  LST.future.baw <- merge(LST.sum, bps_evh, by="RCAID", all.x=TRUE, all.y=FALSE)
  
  
setwd(paste0(dataPath, "NBCD/Wen_model_output/"))

  coeffs.in.mn <- read.csv(paste0("BAW_models_", basin, "_mod_coeffs_summer_Mn.csv"), stringsAsFactors=FALSE)
  
  LogPred.out <- LST.baw
  LogPred.out[,2] <- 0
  colnames(LogPred.out)[2] <- "Summ.mn"
  LogPred.out.future <- LST.future.baw
  LogPred.out.future[,2] <- 0
  colnames(LogPred.out.future)[2] <- "Summ.mn"
  

      
    i <- 3 
    LogPred.out[,2] <- (apply(LST.baw, 1, function(x) x[2] * coeffs.in.mn$bLST[i] + x[3] * coeffs.in.mn$BAW[i] + coeffs.in.mn$Int[i]))
      
    LogPred.out.future[2]  <- (apply(LST.future.baw, 1, function(x) x[2] * coeffs.in.mn$bLST[i] + x[3] * coeffs.in.mn$BAW[i] + coeffs.in.mn$Int[i])) 
    
    

write.dbf(LogPred.out, file = paste0("predt", year, "_", basin, "_summer_mn_current.dbf")) 
write.dbf(LogPred.out.future, file = paste0("predt", year, "_", basin, "_summer_mn_RV_potent.dbf")) 


##################### Max models ########################################################
  LST.sum <- as.data.frame(apply(LST.in[,22:33], 1, mean))
  colnames(LST.sum)[1] <- "LST.mn"
  LST.sum$RCAID <- LST.in$RCAID
  
  setwd(paste0(dataPath, "NBCD/"))
  baw <- read.csv(paste0(basin, "_RCA_BAW.csv"))
  
  LST.baw <- merge(LST.sum, baw, by="RCAID", all.x=TRUE, all.y=FALSE)
  colnames(LST.baw)[3] <- "baw"
  
  setwd(paste0(dataPath, "BPS/"))
  bps_evh <- read.csv(paste0(basin, "_BpS_EVH.csv"))
  
  LST.future.baw <- merge(LST.sum, bps_evh, by="RCAID", all.x=TRUE, all.y=FALSE)
  
  
setwd(paste0(dataPath, "NBCD/Wen_model_output/"))

 coeffs.in.mx <- read.csv(paste0("BAW_models_", basin, "_mod_coeffs_summer_Max.csv"), stringsAsFactors=FALSE)
  
  LogPred.out <- LST.baw
  LogPred.out[,2] <- 0
  colnames(LogPred.out)[2] <- "Summ.max"
  LogPred.out.future <- LST.future.baw
  LogPred.out.future[,2] <- 0
  colnames(LogPred.out.future)[2] <- "Summ.max"
  
  
  
    i <- 3 
    LogPred.out[,2] <- (apply(LST.baw, 1, function(x) x[2] * coeffs.in.mx$bLST[i] + x[3] * coeffs.in.mx$BAW[i] + coeffs.in.mx$Int[i]))
    
    LogPred.out.future[2]  <- (apply(LST.future.baw, 1, function(x) x[2] * coeffs.in.mx$bLST[i] + x[3] * coeffs.in.mx$BAW[i] + coeffs.in.mx$Int[i])) 
    

write.dbf(LogPred.out, file = paste0("predt", year, "_", basin, "_summer_max_current.dbf")) 
write.dbf(LogPred.out.future, file = paste0("predt", year, "_", basin, "_summer_max_RV_potent.dbf")) 


########################################

  yrPath <- 12
  year <- 2012


setwd(paste0(dataPath, "NBCD/Wen_model_output/"))
  LogPred.out.mn <- read.dbf(paste0("predt", year, "_", basin, "_summer_mn_current.dbf"))
  LogPred.out.mn.potent <- read.dbf(paste0("predt", year, "_", basin, "_summer_mn_RV_potent.dbf"))
  LogPred.out.mx <- read.dbf(paste0("predt", year, "_", basin, "_summer_max_current.dbf"))
  LogPred.out.mx.potent <- read.dbf(paste0("predt", year, "_", basin, "_summer_max_RV_potent.dbf"))
  convType <- read.csv("Wen_RCA_CONVTYPE.csv", stringsAsFactors = FALSE)

  RV_plot.mn <- as.data.frame(LogPred.out.mn$Summ_mn)
  RV_plot.mn$RCAID <- LogPred.out.mn$RCAID
  colnames(RV_plot.mn)[1] <- "Current"
  RV_plot.mn$Potential <- LogPred.out.mn.potent$Summ_mn
  
##########################################
  RV_plot.mx <- as.data.frame(LogPred.out.mx$Summ_max)
  RV_plot.mx$RCAID <- LogPred.out.mx$RCAID
  colnames(RV_plot.mx)[1] <- "Current"
  RV_plot.mx$Potential <- LogPred.out.mx.potent$Summ_max
##################################################
  setwd(dataPath)
  SO <- read.csv("Wen_RCA_SO.csv", stringsAsFactors = FALSE) #this only includes RCAs in teh STHD network
  colnames(SO)[1] <- "RCAID"

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
  test.out$someConv <- ifelse(test.out$noCh < 0.7, sConv, "noCh") 
  
  test.out$RCAID <- as.numeric(test.out$value)
  test.out <- test.out[,-1]
  Data.mn <- merge(RV_plot.mn, test.out, by="RCAID", all.x=FALSE, all.y=FALSE)
  Data.mn$delta <- Data.mn$Current - Data.mn$Potential
  
  ########### max Conversion type #############  
  
  Data.mn$Color="white"
  
  
  Data.mn$Color[Data.mn$maxConv =="Con"]="dodgerblue"
  Data.mn$Color[Data.mn$maxConv =="Dev"]="darkgrey"
  Data.mn$Color[Data.mn$maxConv =="Ag"]="gold"
  Data.mn$Color[Data.mn$maxConv =="Inv"]="darkorchid1"
  
  Data.mn$ptSz <- 1.5
  Data.mn$ptSz[Data.mn$maxConv =="noCh"]= 0.8
  
  plot(Data.mn$Current, Data.mn$Potential, pch=21, col="black", bg=Data.mn$Color, cex=Data.mn$ptSz, , main = paste0(longBasin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(6.5,14), ylim=c(6.5,14))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Development", "Agriculture", "Invasives"), col="black", pt.bg=c("white", "dodgerblue", "darkgrey", "gold", "darkorchid1"), cex=.7, xpd=NA)
  
  ###################### "some" conversion type ############
  
  Data.mn$Color="white"
  
  
  Data.mn$Color[Data.mn$someConv =="Con"]="dodgerblue"
  Data.mn$Color[Data.mn$someConv =="Dev"]="darkgrey"
  Data.mn$Color[Data.mn$someConv =="Ag"]="gold"
  Data.mn$Color[Data.mn$someConv =="Inv"]="darkorchid1"
  
  Data.mn$ptSz <- 1.5
  Data.mn$ptSz[Data.mn$someConv =="noCh"]= 0.8
  plot(Data.mn$Current, Data.mn$Potential, pch=21, col="black", bg=Data.mn$Color, cex=Data.mn$ptSz, main = paste0(longBasin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(6.5,14), ylim=c(6.5,14))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Development", "Agriculture", "Invasive"), col="black", pt.bg=c("white", "dodgerblue", "darkgrey", "gold", "darkorchid1"), cex=.7, xpd=NA)
  
  ###################### orig conversion type ############
  Data.orig <- merge(RV_plot.mn, convType, by="RCAID")
  Data.orig <- merge(Data.orig, SO, by="RCAID")
  Data.orig$Color="white"
  
  Data.orig$ptSz <- 1.5
  Data.orig$ptSz[Data.orig$conv_type =="No Change"]= 0.8
  
  Data.orig$Color[Data.orig$conv_type =="Minor Conifer Encroachment"]="cadetblue1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conifer Encroachment"]="dodgerblue"
  Data.orig$Color[Data.orig$conv_type =="Significant Conifer Encroachment"]="blue"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Grass/Shrubland"]="darkolivegreen1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Grass/Shrubland"]="chartreuse4"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Agriculture"]="lightgoldenrod1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Agriculture"]="gold"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Agriculture"]="gold3"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Invasive"]="plum1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Invasive"]="mediumorchid1"
  Data.orig$Color[Data.orig$conv_type =="significant Conversion to Invasive"]="darkorchid1"
  Data.orig$Color[Data.orig$conv_type =="Minor Development"]="gainsboro"
  Data.orig$Color[Data.orig$conv_type =="Moderate Development"]="gray84"
  Data.orig$Color[Data.orig$conv_type =="Multiple Dominant Conversion Types"]="darkred"
  Data.orig$Color[Data.orig$conv_type =="Significant Development"]="azure4"
  Data.orig$Color[Data.orig$conv_type =="Minor Devegetation"]="orangered"
  
  plot(Data.orig$Current, Data.orig$Potential, pch=21, col="black", bg=Data.orig$Color, cex=Data.orig$ptSz, main = paste0(longBasin, " summer mean stream temp ", year), xlab="Summer mn stream temp under current riparian veg", ylab="Summer mn stream temp under potential riparian veg", xlim=c(6.5,14), ylim=c(6.5,14))
  abline(0,1)
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub", "Agriculture", "Invasives", "Devegetation", "Development", "Multiple"), col="black", pt.bg=c("white", "dodgerblue", "limegreen", "gold", "darkorchid1", "orangered", "grey", "darkred"), cex=.75, xpd=NA)
  
  ############### Box plot max Conversion type#######################
  
  
  
  Data.mn$ConT_lab="NoC"
  Data.mn$ConT_lab[Data.mn$maxConv =="Con"]="Con"
  Data.mn$ConT_lab[Data.mn$maxConv =="Ag"]="Ag"
  Data.mn$ConT_lab[Data.mn$maxConv =="grSh"]="grSH"
  Data.mn$ConT_lab[Data.mn$maxConv =="Ag"]="Ag"
  Data.mn$ConT_lab[Data.mn$maxConv =="Inv"]="Inv"
  Data.mn$ConT_lab[Data.mn$maxConv =="Dev"]="Dev"
  Data.mn$ConT_lab[Data.mn$maxConv =="deVeg"]="deVeg"
  
  boxplot(delta ~ SO, data = Data.mn, main = paste0(basin," ", year, " change in summer mn temp by stream order"), xlab = "Stream order", ylab = "Current - Potential")
  abline(0,0, lty = 2)
  
  boxplot(delta ~ ConT_lab, data = Data.mn, col=c("gold", "blue", "grey", "darkorchid1", "white"), main = paste0(basin, " ", year," change in summer mn temp by conversion type"), xlab = "Conversion type", ylab = "Current - Potential")
 
  abline(0,0, lty = 2)
  
  ############### Box plot some Conversion type#######################
  
  
  
  Data.mn$ConT_lab="NoC"
  Data.mn$ConT_lab[Data.mn$someConv =="Con"]="ConEn"
  Data.mn$ConT_lab[Data.mn$someConv =="grSh"]="grSH"
  Data.mn$ConT_lab[Data.mn$someConv =="Ag"]="Ag"
  Data.mn$ConT_lab[Data.mn$someConv =="Inv"]="Inv"
  Data.mn$ConT_lab[Data.mn$someConv =="Dev"]="Dev"
  Data.mn$ConT_lab[Data.mn$someConv =="deVeg"]="deVeg"
  
  boxplot(delta ~ ConT_lab, data = Data.mn, col=c("gold", "blue", "grey", "orangered", "chartreuse4", "darkorchid1", "white"), main = paste0(basin, " ", year," change in summer mn temp by conversion type"), xlab = "Conversion type", ylab = "Current - Potential")
  abline(0,0, lty = 2)
  
  setwd(paste0(dataPath, "NBCD/", basin, "_model_output/"))
  write.csv(Data.mn, file = paste0("predt", year, "_", basin, "_summer_mean_change_by_type.csv"), row.names = FALSE)
  
 ############### Box plot #######################

  Data.mn <- merge(Data.mn, SO, by="RCAID")
  
  Data.mn$ConT_lab="NoC"
  Data.mn$ConT_lab[Data.mn$CONV_TYPE =="Conversion to Developed"]="Dev"
  Data.mn$ConT_lab[Data.mn$CONV_TYPE =="Conifer Encroachment"]="ConEn"
  Data.mn$ConT_lab[Data.mn$CONV_TYPE =="Upland Encroachment"]="UpEn"
  Data.mn$ConT_lab[Data.mn$CONV_TYPE =="Conversion to Agriculture"]="Ag"
  Data.mn$ConT_lab[Data.mn$CONV_TYPE =="Conversion to Invasive Vegetation"]="IV"
  boxplot(delta ~ SO, data = Data.mn, main = paste0("Wen ", year, " change in summer mean temp by stream order"), xlab = "Stream order", ylab = "Current - Potential", ylim=c(-5,9))
  boxplot(delta ~ ConT_lab, data = Data.mn, col=c("darkgoldenrod1", "green4", "gray34", "blueviolet", "white", "lightgreen"), main = paste0("Wen ", year," change in summer mean temp by conversion type"), xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-5,9))
  abline(0,0, lty = 2)

setwd(paste0(dataPath, "NBCD/Wen_model_output/"))
  write.csv(Data.mn, file = paste0("predt", year, "_", basin, "_summer_mean_change_by_type.csv"), row.names = FALSE)

############### max ##############
  Data.mx$Color="white"
  
  Data.mx$Color[Data.mx$someConv =="Con"]="dodgerblue"
  Data.mx$Color[Data.mx$someConv =="Dev"]="darkgrey"
  Data.mx$Color[Data.mx$someConv =="Ag"]="gold"
  Data.mx$Color[Data.mx$someConv =="Inv"]="darkorchid1"
  Data.mx$Color[Data.mx$someConv =="deVeg"]="orangered"
  Data.mx$Color[Data.mx$someConv =="grSh"]="chartreuse4"
  
  Data.mx$ptSz <- 1.5
  Data.mx$ptSz[Data.mx$someConv =="noCh"]= 0.8
  
  plot(Data.mx$Current, Data.mx$Potential, pch=21, col="black", bg=Data.mx$Color, cex=Data.mx$ptSz, main = paste0("Wen summer max stream temp ", year), ylim=c(8,21), xlim=c(8,21), xlab="Summer max stream temp under current riparian veg", ylab="Summer max stream temp under potential riparian veg")
  abline(0,1)
  
  
  legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub", "Agriculture", "Invasives", "Devegetation", "Development"), col="black", pt.bg=c("white", "dodgerblue", "chartreuse4", "gold", "darkorchid1", "orangered", "darkgrey"), cex=.7, xpd=NA)
  
   ###############Box plot#######################

Data.mx <- merge(Data.mx, convType, by="RCAID")
Data.mx <- merge(Data.mx, SO, by="RCAID")
Data.mx$delta <- Data.mx$Current - Data.mx$Potential
Data.mx$ConT_lab="NoC"
Data.mx$ConT_lab[Data.mx$someConv =="Dev"]="Dev"
Data.mx$ConT_lab[Data.mx$someConv  =="Con"]="ConEn"
Data.mx$ConT_lab[Data.mx$someConv  =="Ag"]="Ag"
Data.mx$ConT_lab[Data.mx$someConv  =="Inv"]="Inv"
Data.mx$ConT_lab[Data.mx$someConv  =="deVeg"]="deVeg"
Data.mx$ConT_lab[Data.mx$someConv  =="grSh"]="grSh"

boxplot(delta ~ SO, data = Data.mx, main = "Wen 2012 change in summer max temp by stream order", xlab = "Stream order", ylab = "Current - Potential", ylim=c(-5,9))
boxplot(delta ~ ConT_lab, data = Data.mx, col=c("darkgoldenrod1", "blue", "darkgrey", "orangered", "chartreuse4", "darkorchid1", "white"), main = "Wen 2012 change in summer max temp by conversion type", xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-5,9))
abline(0,0, lty = 2)

  ###################### orig conversion type ############
  Data.orig <- Data.mx
  
  Data.orig$Color="white"
  
  Data.orig$ptSz <- 1.5
  Data.orig$ptSz[Data.orig$conv_type =="No Change"]= 0.8
  
  Data.orig$Color[Data.orig$conv_type =="Minor Conifer Encroachment"]="cadetblue1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conifer Encroachment"]="dodgerblue"
  Data.orig$Color[Data.orig$conv_type =="Significant Conifer Encroachment"]="blue"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Grass/Shrubland"]="darkolivegreen1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Grass/Shrubland"]="chartreuse4"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Agriculture"]="lightgoldenrod1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Agriculture"]="gold"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Agriculture"]="gold3"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Invasive"]="plum1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Invasive"]="mediumorchid1"
  Data.orig$Color[Data.orig$conv_type =="significant Conversion to Invasive"]="darkorchid1"
  Data.orig$Color[Data.orig$conv_type =="Minor Development"]="gainsboro"
  Data.orig$Color[Data.orig$conv_type =="Moderate Development"]="gray84"
  Data.orig$Color[Data.orig$conv_type =="Multiple Dominant Conversion Types"]="darkred"
  Data.orig$Color[Data.orig$conv_type =="Significant Development"]="azure4"
  Data.orig$Color[Data.orig$conv_type =="Minor Devegetation"]="orangered"
  
  plot(Data.orig$Current, Data.orig$Potential, pch=21, col="black", bg=Data.orig$Color, cex=Data.orig$ptSz, main = paste0(longBasin, " summer max stream temp ", year), xlab="Summer max stream temp under current riparian veg", ylab="Summer max stream temp under potential riparian veg", xlim=c(8,21), ylim=c(8,21))
  abline(0,1)

legend("topleft", pch=21, bty="n", legend = c("No Change", "Conifer Encroachment", "Grass/shrub", "Agriculture", "Invasives", "Devegetation", "Development", "Multiple"), col="black", pt.bg=c("white", "dodgerblue", "limegreen", "gold", "darkorchid1", "orangered", "grey", "darkred"), cex=.75, xpd=NA)

  Data.orig$Color[Data.orig$conv_type =="Minor Conifer Encroachment"]="cadetblue1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conifer Encroachment"]="dodgerblue"
  Data.orig$Color[Data.orig$conv_type =="Significant Conifer Encroachment"]="blue"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Grass/Shrubland"]="darkolivegreen1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Grass/Shrubland"]="chartreuse3"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Grass/Shrubland"]="chartreuse4"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Agriculture"]="lightgoldenrod1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Agriculture"]="gold"
  Data.orig$Color[Data.orig$conv_type =="Significant Conversion to Agriculture"]="gold3"
  Data.orig$Color[Data.orig$conv_type =="Minor Conversion to Invasive"]="plum1"
  Data.orig$Color[Data.orig$conv_type =="Moderate Conversion to Invasive"]="mediumorchid1"
  Data.orig$Color[Data.orig$conv_type =="significant Conversion to Invasive"]="darkorchid1"
  Data.orig$Color[Data.orig$conv_type =="Minor Development"]="gainsboro"
  Data.orig$Color[Data.orig$conv_type =="Moderate Development"]="gray84"
  Data.orig$Color[Data.orig$conv_type =="Multiple Dominant Conversion Types"]="darkred"
  Data.orig$Color[Data.orig$conv_type =="Significant Development"]="azure4"
  Data.orig$Color[Data.orig$conv_type =="Minor Devegetation"]="orangered"
boxplot(delta ~ SO.x, data = Data.orig, main = "Wen 2012 change in summer max temp by stream order", xlab = "Stream order", ylab = "Current - Potential", ylim=c(-5,9))
boxplot(delta ~ ConT_lab, data = Data.orig, col=c("darkgoldenrod1", "green4", "gray34", "blueviolet", "white", "lightgreen"), main = "Wen 2012 change in summer max temp by conversion type", xlab = "Conversion type", ylab = "Current - Potential", ylim=c(-5,9))
abline(0,0, lty = 2)

write.csv(Data.mx, file = paste0("predt", year, "_", basin, "_summer_max_change_by_type.csv"))

############### misc plots ########################
plot(Data.mn$delta, Data.mx$delta)
plot(RV_plot.mn$RCAID, RV_plot.mn$Current, xlab="RCAID", ylab="temp", main="Wen 2012 summer temp v RCAID", ylim=c(-4,20))
points(RV_plot.mn$RCAID, RV_plot.mn$Potential, pch=16, col="blue", cex=.75)
points(RV_plot.mx$RCAID, RV_plot.mx$Current, col="red")
points(RV_plot.mx$RCAID, RV_plot.mx$Potential, pch=16, col="red", cex=.75)
points(RV_plot.mn$RCAID, (RV_plot.mn$Current - RV_plot.mn$Potential), pch=16, col="black", cex=.75)
points(RV_plot.mx$RCAID, (RV_plot.mx$Current - RV_plot.mx$Potential), pch=16, col="red", cex=.75)

plot((RV_plot.mn$Current - RV_plot.mn$Potential), (RV_plot.mx$Current - RV_plot.mx$Potential), pch=16, col="blue", cex=.75)


##########################
#This parts reads in the RV output for further analysis
###########################

setwd(paste0(dataPath, "NBCD/Wen_model_output/"))
Data.mn <- read.csv(paste0("predt", year, "_", basin, "_summer_mean_change_by_type.csv"), stringsAsFactors = FALSE)

###############################################
#
# Edited 5 may 2016 to add animation output
# Edited 25 May 2016 to add HUC display
################################################

library(rgdal)
library(RColorBrewer)
library(classInt)


  modelPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/", yearPath, "_temp_CHaMP/")
  ptsPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/", midBasin, "/", "Mean_models")
  shpPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/", midBasin, "/", yearPath, "/", "graphic_shapes")

  netname <- paste0(basin, "_", yearPath, "_8D_mn")
  ptsname <- paste0(basin, "_Error_", yearPath, "_8D_mn")
  
setwd(ptsPath)

  error_pts <- readOGR(dsn=".", "Wen_Error_2014_8D_Mn")
  
setwd(shpPath) 
  
  network <- readOGR(dsn=".", layer = netname)
  network@data <- network@data[,-3]
  
  HUCs <- readOGR(dsn=".", "Wen_HUC5")
  
  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)

  names.out <- colnames(network@data[4:49])
  namesnum <- as.numeric(gsub("Tmn_14_", "", colnames(network@data[4:49])))
  means <- colMeans(network@data[4:49])
  SDs <- colStdevs(network@data[4:49])
  yplus <- means + SDs
  yminus <- means - SDs
  df <- data.frame(means=means, SDs=SDs, names=namesnum)
  sequ <- c(1:46)
  namer <- sprintf('%03d', sequ)
  fix4 <- classIntervals(means, n = 10, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18))
  fix4.colors <- findColours(fix4,pal=seis)
  
  for (i in 4:49)
    {
      
      namey <- gsub("Tmn_14_", "", colnames(network@data)[i])
      
      filename <- paste0(mainPath, longBasin, "/", yearPath, "/graphics3/", namer[i-3], ".png", sep="")
      png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="black")
      
      fix3 <- classIntervals(network@data[,i], n = 11, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18,22))
      fix3.colors <- findColours(fix3,pal=seis)
      
      cexEr <- ifelse(abs(error_pts@data[,i]) <= 1, 0.5,
                      ifelse(abs(error_pts@data[,i])>1, 0.75,
                             ifelse(abs(error_pts@data[,i])>2, 1.0,
                                    ifelse(abs(error_pts@data[,i])>3, 1.25, NA))))
      
      plot(network, col=fix3.colors, bg="black", fg="white")
      points(error_pts, pch=16, col="gray40", cex=cexEr)
      
      legend("right", fill = attr(fix3.colors, "palette"), legend = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18+"), bty = "n", cex=.5, inset=c(.1,0), text.col="white");
      
      
      title("Wenatchee 8-day mean 2014 ('C)", sub = paste0("Julian Day ", namey), line=-0.9, adj=.80, col.main="white", col.sub="white", outer=FALSE, cex.main=0.5, cex.sub=0.5)
      tmp2 <- subplot(
        plot(namesnum[1:(i-2)], means[1:(i-2)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,18), cex.main=.8, main="Basin mean (+/-SD)", adj=0, xlab='',ylab='', col.lab="white", cex.axis=0.5, cex.lab = 0.25, col.axis="white", col.main = "white", bg="black"), 
        x=grconvertX(c(0.1,0.45), from='npc'), 
        y=grconvertY(c(0.05, 0.20), from='npc'),
        size=c(1,1.5), vadj=0.5, hadj=0.5, 
        pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
      
      
      dev.off()
    }


setwd(paste0(mainPath, longBasin, "/", yearPath, "/graphics3/"))

system('"C:/Program Files/ImageMagick-7.0.1-Q16/convert.exe" -delay 20 -morph 3 *.png example2.mpeg')

###############################################
#
# Edited 5 may 2016 to add animation output
# Edited 25 May 2016 to add HUC display
# Edited 13 June 2016 to add multiple frame display
################################################

library(rgdal)
library(RColorBrewer)
library(classInt)


  modelPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/", yearPath, "_temp_CHaMP/")
  ptsPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/", midBasin, "/", "Mean_models")
  shpPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/", midBasin, "/", yearPath, "/", "graphic_shapes")

  netname <- paste0(basin, "_", yearPath, "_8D_mn")
  netname2 <-paste0(basin, "_2013_8D_mn") 
  ptsname <- paste0(basin, "_Error_", yearPath, "_8D_mn")

setwd(ptsPath)

  error_pts <- readOGR(dsn=".", "Wen_Error_2014_8D_Mn")

setwd(shpPath) 

  network <- readOGR(dsn=".", layer = netname)

  network2 <- readOGR(dsn=".", layer = netname2)
  network2@data <- network2@data[,-4]
  
  HUCs <- readOGR(dsn=".", "Wen_HUC5")
  
  means2 <- colMeans(network2@data[4:49])
  SDs2 <- colStdevs(network2@data[4:49])
  yplus2 <- means2 + SDs2
  yminus2 <- means2 - SDs2
  df2 <- data.frame(means=means, SDs=SDs2, names=namesnum)
  sequ <- c(1:46)
  namer <- sprintf('%03d', sequ)
  fix4 <- classIntervals(means, n = 10, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18))
  fix4.colors <- findColours(fix4,pal=seis)
  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)
  
  names.out <- colnames(network@data[4:49])
  namesnum <- as.numeric(gsub("Tmn_14_", "", colnames(network@data[4:49])))

  op <- par(mfrow = c(1,3),
            mar = c(0,0,1,1) + 0.1)

  for (i in 4:49)
    {
      
      namey <- gsub("Tmn_14_", "", colnames(network@data)[i])
      
      filename <- paste0(mainPath, longBasin, "/", yearPath, "/graphics4/", namer[i-3], ".png", sep="")
      png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="black")
      
      fix3 <- classIntervals(network@data[,i], n = 11, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18,22))
      fix3.colors <- findColours(fix3,pal=seis)
      
      op <- par(mfrow = c(1,2),
                oma = c(0,2,0,0),
                mar = c(1,1,1,1.5) + 0.1,
                mgp = c(2,0,0),
                xpd=NA)
      plot(network, col=fix3.colors, bg="black", fg="white")
      text(x = (usr[1] + usr[2])/2, y=usr[3], yearPath, col="white", cex=.8)
      
      plot(network2, col=fix3.colors, bg="black", fg="white")
      text(x = (usr[1] + usr[2])/2, y=usr[3], "2013", col="white", cex=.8)
      
      legend("right", title="8-day Mean ('C)", fill = attr(fix3.colors, "palette"), legend = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18+"), bty = "n", cex=.5, inset=c(-.2,0), text.col="white");
      mtext("Wenatchee 8-day mean ('C)",side =3, outer=TRUE, line=-3, col="white", cex=.9)
      mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-3, col="white", cex=.7)
      
      
      dev.off()
    }


setwd(paste0(mainPath, longBasin, "/", yearPath, "/graphics4/"))

system('"C:/Program Files/ImageMagick-7.0.1-Q16/convert.exe" -delay 20 -morph 3 *.png example.mpeg')


###########################
pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
nf <- layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), respect = TRUE)
layout.show(nf)

