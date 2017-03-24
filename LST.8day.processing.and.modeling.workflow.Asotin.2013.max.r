############################################################################################################
# This set of R scripts combines a the series of scripts that make up the work flow to predict and validate 
# stream temperature for a summer max temps MODIS 1km LST data
# The initial input is an LST_YY.dbf which is a table (columns = days, rows = sites) with the grid value for each grid cell in the spatial extent
# 
# Edited Aug 2014 to add the PRESS stastic output
# Should look at PRESS output to screen to chose model - only that model will output resids and preds.
# Edited Aug 2014 to add the PRESS stastic output

# Edited Oct 2015 for EP watershed summer output
# Edited Jan 2016 to update the gap-filling interpolation functions



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

  basin <- "Aso"
  midBasin <- "Asotin"
  longBasin <- "Asotin"
  yrPath <- "13"
  yearPath <- "2013"
  subDir <- "EP_temp/"
  dataPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"
  mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"
  
  
           
# ################################################################
#Logger prediction modeling part
# ################################################################
  
  
  setwd(paste0(mainPath, midBasin))
  
  
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))

  colnames(LST.in)<-gsub("X13", "", colnames(LST.in))

  newnamesnum <- as.numeric(colnames(LST.in)[1:46])

  setwd(paste0(mainPath, longBasin))
        
  ID.in <- read.csv(paste0(basin, "_sites_elev_rca.csv"), stringsAsFactors=FALSE)
  
  Log.in <- read.csv(paste0(basin, "_temp_data_", yearPath, ".csv"), stringsAsFactors=FALSE)
  
  setwd(paste0(mainPath, subDir, longBasin, "/", yearPath, "/"))
  
  SiteID <- unique(Log.in$SiteName)
  SiteID <- as.matrix(SiteID)
  
  LST.Log.out <- data.frame (mup = NULL)
  
    for (i in SiteID) 
      { 
        Log.site <- Log.in[Log.in$SiteName  == i,]
        Log.site <- as.data.frame(Log.site)
        
        
        RCAID <- ID.in$rca_id[ID.in$SiteName == i]
        Elev <- ID.in$Elev_M[ID.in$SiteName == i]
        
        LST.site <- matrix(ncol=3, nrow=46)
        LST.site[,1] <- newnamesnum
        LST.site[,2] <- unlist(LST.in[LST.in$RCAID == RCAID,1:46])
        LST.site <- data.frame(LST.site)
        colnames(LST.site) <- c("JulDay", "LST", "Elev")
        LST.site[3] <- Elev
        LST.Log.site <- merge(LST.site, Log.site, by.x = "JulDay", by.y = "JulDay", all.x=TRUE, all.y = FALSE)
        
        LST.Log.out <- rbind(LST.Log.out, LST.Log.site)
      }
  
  
  ind <- apply(LST.Log.out, 1, function(x) !any(is.na(x)))
  NoNA.xyz <- LST.Log.out[ind,]
  
  NoNA.xyz <- NoNA.xyz[,c(12, 2, 1, 3, 5)]
  colnames(NoNA.xyz) <- c("y", "x", "z", "e", "SiteName")
  
  plot(NoNA.xyz$z, NoNA.xyz$y)
  plot(NoNA.xyz$x, NoNA.xyz$y)

  write.csv(x=NoNA.xyz, file=paste0(basin, "_", yearPath, "_8Day_model_data.csv"), row.names = FALSE)
  
  NoNA.xyz <- orderBy(~z, NoNA.xyz)
  
  maxrow <- which.max(NoNA.xyz$y)
  data.sp <- NoNA.xyz[1:(maxrow-1),]
  data.fall <- NoNA.xyz[maxrow:nrow(NoNA.xyz),]

  points(data.sp$x, data.sp$y, pch=16, col="cadetblue3")
  points(data.fall$x, data.fall$y, pch=16, col="chocolate")

    


# ###############################
# full year
# ##################################

  setwd(paste0(mainPath, subDir, longBasin, "/", yearPath))

  y <- NoNA.xyz$y
  x <- NoNA.xyz$x
  z <- NoNA.xyz$z
  e <- NoNA.xyz$e
  plot(x, y)
  
  maxrow <- which.max(NoNA.xyz$y)
  data.sp <- NoNA.xyz[1:maxrow,]
  data.fall <- NoNA.xyz[maxrow:nrow(NoNA.xyz),]


  mod <- lm(y ~ x + I(x^2) + z + e)
  sum_mod <- summary(mod)
  pred.y <- predict(mod)
  plot(pred.y, y, main = "8-day Max Full Year")
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
  write.table (x=pred.out,append=T,row.names=F,file=paste0("jk_pred_v_y_Max_", basin, "_", yearPath, "_full_year.csv"),sep = ",", col.names=F)  
  
  plot(pred.out[,1], pred.out[,2])
  summer_pred <- subset(pred.out, z > 181 & z < 258)
  points(summer_pred[, 1], summer_pred[,2], pch = 16, col = "green")
  abline(0,1)

  fit <- lm(y~ pred.y)
  plot(fit)
  summary(fit)
# ####################################
# spring/fall
# #####################################
  setwd(paste0(mainPath, subDir, longBasin, "/", yearPath))

  coeffs_out <- data.frame(Int=numeric(2), bLST=numeric(2), bLST2=numeric(2), bJul=numeric(2), bElev=numeric(2))
  metrics_out <- data.frame(r2=numeric(2), RMSE=numeric(2), p2=numeric(2), RMSEP=numeric(2), N_Sites=numeric(2), N=numeric(2))
  rownames(metrics_out) <- c("Spring", "Fall")
  rownames(coeffs_out) <- c("Spring", "Fall")

# 
#   data.sp <- data.sp[-131,]
#   data.sp <- data.sp[-125,]
#   data.sp <- data.sp[-119,]
#   

  y <- data.sp$y
  x <- data.sp$x
  z <- data.sp$z
  e <- data.sp$e
  plot(z, y)  
  plot(x, y)
  
  mod <- lm(y ~ x + I(x^2) + z + e)
  sum_mod <- summary(mod)
  coeffs <- as.matrix(coefficients(mod))
  pred.y <- predict(mod)
  
  pred.y[pred.y < -0.5] = -0.5
  
  plot(pred.y, y, main = "8-day Max Spring Leg")
  abline(0,1)
  post_mod <- summary(lm(y ~ pred.y))
  gvmodel <- gvlma(mod)
  summary(gvmodel)
  plot(mod, which= 1:5)
  outlierTest(mod)
  qqPlot(mod, main="QQ Plot Spring Leg")
  spreadLevelPlot(mod)
  plot(pred.y, mod$residuals, main="Model diagnostics Spring Leg", xlab="Predicted", ylab="Residuals")
  
  pressstat_sum <- PRESS(sum_mod, verbose = "FALSE")
  RMSEP <- sqrt(mean((pressstat_sum$residuals)^2))
  
  library(pls)
  mod2 <- plsr(y ~ x + I(x^2) + z + e, validation = "LOO")
  p2 <- R2(mod2)
  detach("package:pls", unload=TRUE)
  
  
  pred.out <- matrix(nrow=length(pred.y), ncol=5)
  pred.out[,1] <- y
  pred.out[,2] <- pred.y
  pred.out[,3] <- z
  pred.out[,4] <- "Spring"
  pred.out[,5] <- yearPath
  colnames(pred.out) <- c("Y", "PredY", "JulDay", "Season", "Year")
  write.table (x=pred.out,append=F,row.names=F,file=paste0("jk_pred_v_y_Max_", basin, "_", yearPath, "_sp_fall.csv"),sep = ",", col.names=F)  
  
  coeffs_out[1,1] <- coeffs[1,1]
  coeffs_out[1,2] <- coeffs[2,1]
  coeffs_out[1,3] <- coeffs[3,1]
  coeffs_out[1,4] <- coeffs[4,1]
  coeffs_out[1,5] <- coeffs[5,1]
  
  metrics_out[1,1] <- sum_mod$adj.r.squared
  metrics_out[1,2] <- sum_mod$sigma
  metrics_out[1,3] <- p2$val[5]
  metrics_out[1,4] <- RMSEP
  metrics_out[1,5] <- length(unique(data.sp$SiteName))
  metrics_out[1,6] <- length(y)



  y <- data.fall$y
  x <- data.fall$x
  z <- data.fall$z
  e <- data.fall$e

  plot(z, y)  
  plot(x, y)

  mod <- lm(y ~ x + I(x^2) + z + e)
  sum_mod <- summary(mod)
  coeffs <- as.matrix(coefficients(mod))
  pred.y <- predict(mod)
  pred.y[pred.y < -0.5] = -0.5
  plot(pred.y, y, main = "8-day Min Fall Leg")
  abline(0,1)
  post_mod <- summary(lm(y ~ pred.y))
  
  plot(mod, which = 1:5)
  gvmodel <- gvlma(mod)
  summary(gvmodel)
  outlierTest(mod)
  qqPlot(mod, main="QQ Plot Fall Leg")
  spreadLevelPlot(mod)
  plot(pred.y, mod$residuals, main="Model diagnostics Fall Leg", xlab="Predicted", ylab="Residuals")
  
  pressstat_sum <- PRESS(sum_mod, verbose = "FALSE")
  RMSEP <- sqrt(mean((pressstat_sum$residuals)^2))
  
  library(pls)
  mod2 <- plsr(y ~ x + I(x^2) + z + e, validation = "LOO")
  p2 <- R2(mod2)
  detach("package:pls", unload=TRUE)

  pred.out <- matrix(nrow=length(pred.y), ncol=5)
  pred.out[,1] <- y
  pred.out[,2] <- pred.y
  pred.out[,3] <- z
  pred.out[,4] <- "fall"
  pred.out[,5] <- yearPath
  plot(pred.out[,1], pred.out[,2])
  write.table (x=pred.out,append=T,row.names=F,file=paste0("jk_pred_v_y_Max_", basin, "_", yearPath, "_sp_fall.csv"),sep = ",", col.names=F)  

  fall_pred <- subset(pred.out, z > 181 & z < 258)
  origY <- fall_pred[,1]
  predY <- fall_pred[,2]
  points(fall_pred[, 1], fall_pred[,2], pch = 16, col = "purple")


  coeffs_out[2,1] <- coeffs[1,1]
  coeffs_out[2,2] <- coeffs[2,1]
  coeffs_out[2,3] <- coeffs[3,1]
  coeffs_out[2,4] <- coeffs[4,1]
  coeffs_out[2,5] <- coeffs[5,1]

  metrics_out[2,1] <- sum_mod$adj.r.squared
  metrics_out[2,2] <- post_mod$sigma
  metrics_out[2,3] <- p2$val[5]
  metrics_out[2,4] <- RMSEP
  metrics_out[2,5] <- length(unique(data.fall$SiteName))
  metrics_out[2,6] <- length(y)

write.table(x=coeffs_out, append=F,row.names=T, file = paste0("All_data_", basin, "_", yearPath, "_mod_coeffs_Mx.csv"), sep = ",", col.names=T)

write.table(x=metrics_out, append=F,row.names=T, file = paste0("All_data_", basin, "_", yearPath, "_mod_metrics_Mx.csv"), sep = ",", col.names=T)

  pred.y <- read.csv(paste0("jk_pred_v_y_Max_", basin, "_", yearPath, "_sp_fall.csv"), stringsAsFactors = FALSE)
  colnames(pred.y) <- c("Y", "PredY", "JulDay", "Season", "Year")
  
  plot(pred.y$PredY, pred.y$Y, pch=16, col="blue", main=paste0("Max 8-day stream temp ", yearPath), xlab="Predicted", ylab="Observed")
  abline(0,1)
  abline(lm(pred.y$Y~ pred.y$PredY), col="blue")
  fit <- lm(pred.y$Y~ pred.y$PredY)
  plot(fit)
  summary(fit)

  ########################################################################################################
  # This part applies the model coefficients to the LST to generate 8-day temp estimates 
  ########################################################################################################
  
  
  setwd(paste0(mainPath, longBasin, "/"))
  
  elev.in <- read.csv(paste0(basin, "_rca_elev.csv"), stringsAsFactors=F)
  
  
  LST.in <- read.dbf(paste0("LST", yrPath, "_", basin, "_RCA.dbf"))
  
  colnames(LST.in)<-gsub("X13", "", colnames(LST.in))
  colnames(LST.in)[47] <- "RCAID"
  newnamesnum <- as.numeric(colnames(LST.in)[1:46])
  
  LST.elev <- merge(LST.in, elev.in, by.x = "RCAID", by.y = "RCAID")
  
  setwd(paste0(mainPath, subDir, longBasin, "/", yearPath))
  
  coeffs.in <- read.csv(paste0("All_data_", basin, "_", yearPath, "_mod_coeffs_Mx.csv"), stringsAsFactors=FALSE)
  LogPred.out <- LST.elev[,c(2:47,49,1)]
  LogPred.out[,1:46] <- 0
  rcas <- unique(LST.elev$RCAID)
  LST.sum <- LST.elev[,c(2:47, 49,1)]
  
  for (i in 1:length(rcas))  
    {
      x <- unlist(LST.sum[i,])
      maxrow <- as.numeric(which.max(x[1:46])) #either specify or let be dynamic
      midrow <- maxrow - 1
      day <- as.numeric(colnames(LST.sum)[maxrow])
      
      j <- 1
      for (l in 1:midrow)
        {x[l] <- x[l] * coeffs.in$bLST[1] + j * coeffs.in$bJul[1] +  x[47] * coeffs.in$bElev[1] + x[l]^2 * coeffs.in$bLST2[1] + coeffs.in$Int[1]
        j <- j + 8}
      k <- day
      for (l in maxrow:46)     
        {x[l] <- x[l] * coeffs.in$bLST[2] + k * coeffs.in$bJul[2] +  x[47] * coeffs.in$bElev[2] + x[l]^2 * coeffs.in$bLST2[2] + coeffs.in$Int[2]
        k <- k + 8}
      if (maxrow > 3)
        {
          x[(midrow)] <- NA
          fill <- na.spline(x[1:46],)
          x[(maxrow-3):(maxrow+3)] <- fill[(maxrow-3):(maxrow+3)]
        }
      LogPred.out[i,1:46] <- x [1:46] 
    }
  
  LogPred.out <- as.data.frame(LogPred.out)
  
  LogPred.out$Basin_RCA <- paste0(basin, "_", LogPred.out$RCAID)
  namesnum <- as.numeric(colnames(LogPred.out[1:46]))
  varName <- paste0("Tmx_", yrPath)
  names.out <- sprintf("%s_%03d", varName, namesnum)
  colnames(LogPred.out)[1:46] <- names.out[1:46]
  plot(namesnum, LogPred.out[1,1:46])
  
  setwd(paste0(mainPath, longBasin, "/Max_models/"))
  
  write.dbf(LogPred.out, file = paste0("predt", yearPath, "_", basin, "_8D_Mx.dbf")) 
  
  
  
# #########################
# This parts formats the error by day/site info
# ##########################
  
  setwd(paste0(mainPath, subDir, longBasin, "/", yearPath))
  
  NoNA.xyz <- read.csv(paste0(basin, "_", yearPath, "_8Day_model_data.csv"), stringsAsFactors = TRUE)
  NoNA.xyz <- orderBy(~z, NoNA.xyz)
  maxrow <- which.max(NoNA.xyz$y)
  data.sp <- NoNA.xyz[1:(maxrow-1),]
  data.fall <- NoNA.xyz[maxrow:nrow(NoNA.xyz),]
  
  y <- data.sp$y
  x <- data.sp$x
  z <- data.sp$z
  e <- data.sp$e
  mod <- lm(y ~ x + I(x^2) + z + e)
  pred.new <- predict(mod, data = data.sp)
  pred.new[pred.new < -0.5] = -0.5
  data.sp$pred <- unlist(pred.new)
  
  y <- data.fall$y
  x <- data.fall$x
  z <- data.fall$z
  e <- data.fall$e
  mod <- lm(y ~ x + I(x^2) + z + e)
  pred.new <- predict(mod, data=data.fall)
  pred.new[pred.new < -0.5] = -0.5
  data.fall$pred <- unlist(pred.new)
  
  error.pts <- rbind(data.sp, data.fall)
  SiteID <- unique(error.pts$SiteName)
  SiteID <- as.matrix(SiteID)
  Error.pts.out <- matrix(nrow = length(SiteID), ncol = 47)
  
  errorName <- paste0("JulDay_", namesnum)
  colnames(Error.pts.out)[2:47] <- errorName
  colnames(Error.pts.out)[1] <- "SiteName"
  
  Error.pts.out[1:length(SiteID), 1] <- unlist(SiteID)[1:length(SiteID)]
  Error.pts.out <- as.data.frame(Error.pts.out, stringsAsFactors = FALSE)
  
  
    for (i in SiteID) 
      { 
        error.site <- error.pts[error.pts$SiteName  == i,]
        error.site$error <- error.site[,'y']-error.site[,'pred']
        
        
        error <- matrix(ncol=1, nrow=46)
        error[,1] <- namesnum
        error <- data.frame(error)
        colnames(error) <- c("JulDay")
        
        error.site.fill <- merge(error, error.site, by.x = "JulDay", by.y = "z", all.x=TRUE, all.y = FALSE)
        Error.pts.out[Error.pts.out$SiteName==i,2:47] <- as.numeric(unlist(error.site.fill$error))
      }
    
  Error.pts.out[,2:47] <- sapply(Error.pts.out[,2:47], as.numeric)
  
  Error.pts.out[,2:47] <- round(Error.pts.out[,2:47], digits=3)
  
  setwd(paste0(mainPath, longBasin, "/Max_models/"))
  
  write.dbf(Error.pts.out, file = paste0("Error", yearPath, "_", basin, "_8D_Mx.dbf")) 
  write.csv(Error.pts.out, file = paste0("Error", yearPath, "_", basin, "_8D_Mx.csv"))
 
# ##############################################
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
  yrPath <- "13"
  yearPath <- "2013"
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
  
  setwd(paste0(mainPath, longBasin, "/Max_models/"))
  
  network <- spTransform(network, proj4string(pts))
  preds <- read.dbf(paste0("predt", yearPath, "_", basin, "_8D_Mx.dbf"))
  netmerge <- merge(network, preds, by.x='rca_id', by.y = 'RCAID')
  netmerge@data$ReachCode <- as.character(netmerge@data$ReachCode)
  
  error <- read.dbf(paste0("Error", yearPath, "_", basin, "_8D_Mx.dbf"))
  error_pts <- merge(pts, error, by.x='SiteName', by.y='SiteName', all.x=FALSE, all.y=TRUE)
  # #### plot it to make sure it looks right ######### 
  
  
  fix3 <- classIntervals(netmerge@data[,10], n = 11, style = "fixed",fixedBreaks=c(-1,0,1,2,4,6,8,10,12,14,20))
  fix3.colors <- findColours(fix3,pal=seis)
  plot(netmerge, col=fix3.colors, bg="black", fg="white")
  points(error_pts, pch=16, col="gray40")
  network <- netmerge
  
  # #################################################  
  
  setwd(paste0(mainPath, longBasin, "/Max_models/", yearPath, "/shapes/"))
  
  layername <- paste0(basin, "_", yearPath, "_8D_mx")
  ptslayername <- paste0(basin, "_", yearPath, "_8D_mx_error")
  
  writeOGR(netmerge, dsn=".", layer=layername, driver="ESRI Shapefile")
  writeOGR(error_pts, dsn=".", layer=ptslayername, driver="ESRI Shapefile")
  
# ##############################################
# animation output
# ###############################################
  
  library(rgdal)
  library(RColorBrewer)
  library(classInt)
  
  netname <- paste0(basin, "_", yearPath, "_8D_mx")
  ptsname <- paste0(basin, "_", yearPath, "_8D_mx_error")
  
  setwd(paste0(mainPath, longBasin, "/Max_models/", yearPath, "/shapes/"))
  
  error_pts <- readOGR(dsn=".", layer = ptsname)
  
  network <- readOGR(dsn=".", layer = netname)
  error_pts@data[error_pts@data == 0] <- NA
  
  data <- network@data[,3:48]
  data[data < -0.5] = -0.5
  network@data[,3:48] <- data
  
  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)
  fix4 <- classIntervals(means, n = 11, style = "fixed",fixedBreaks=c(-1,6,8,10,12,14,16,18,20,22,24))
  fix4.colors <- findColours(fix4,pal=seis)
  
  names.out <- colnames(network@data[3:48])
  namesnum <- as.numeric(gsub("Tmx_13_", "", colnames(network@data[3:48])))
  means <- colMeans(network@data[3:48])
  SDs <- colStdevs(network@data[3:48])
  yplus <- means + SDs
  yminus <- means - SDs
  df <- data.frame(means=means, SDs=SDs, names=namesnum)
  sequ <- c(1:46)
  namer <- sprintf('%03d', sequ)
  
    for (i in 3:48)
      {
        
        namey <- gsub("Tmx_13_", "", colnames(network@data)[i])
        
        filename <- paste0(mainPath, longBasin, "/Max_models/", yearPath, "/graphics/", namer[i-2], ".png", sep="")
        png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="black")
        
        fix3 <- classIntervals(network@data[,i], n = 11, style = "fixed",fixedBreaks=c(-1,6,8,10,12,14,16,18,20,22,24))
        fix3.colors <- findColours(fix3,pal=seis)
        
        cexEr <- ifelse(abs(error_pts@data[,i]) <= 1, 0.5,
                        ifelse(abs(error_pts@data[,i])>1, 0.75,
                               ifelse(abs(error_pts@data[,i])>2, 1.0,
                                      ifelse(abs(error_pts@data[,i])>3, 1.25, NA))))
        
        plot(network, col=fix3.colors, bg="black", fg="white")
        points(error_pts, pch=16, col="gray40", cex=cexEr)
        
        legend("topright", fill = attr(fix3.colors, "palette"), title="8-day Max (°C)",legend = c("0-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", "20-22", "22+"), bty = "n", cex=.5, inset=c(0,0), text.col="white");
        legend("bottomright", pch=16, col="gray40", pt.cex=c(0.5, 0.75, 1.0, 1.25), title="Model error (°C)", legend = c("0-1","1-2","2-3","3+"), bty = "n", cex=.5, inset=c(0,0), text.col="white");
        
        mtext("Asotin 2013 8-day max (°C)",side =3, outer=TRUE, line=-3, col="white", cex=.7)
        mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-3, col="white", cex=.7)
        
        tmp2 <- subplot(
          plot(namesnum[1:(i-2)], means[1:(i-2)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,22), cex.main=.8, main="Basin mean", adj=0, xlab='',ylab='', col.lab="white", cex.axis=0.5, cex.lab = 0.25, col.axis="white", col.main = "white", bg="black"), 
          x=grconvertX(c(0.0,0.25), from='npc'), 
          y=grconvertY(c(0.80,0.95), from='npc'),
          size=c(1,1.5), vadj=0.5, hadj=0.5, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
        
        
        dev.off()
      }
  
  
  setwd(paste0(mainPath, longBasin, "/Max_models/", yearPath, "/graphics/"))
  
  system('"C:/Program Files/ImageMagick-7.0.1-Q16/convert.exe" -delay 20 -morph 3 *.png Asotin_2013_8D_Mx.mpeg')
  
# ##############
# image example output for wiki
# ##############
  
  seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
  seis <- rev(seis)
  fix4 <- classIntervals(means, n = 11, style = "fixed",fixedBreaks=c(-1,6,8,10,12,14,16,18,20,22,24))
  fix4.colors <- findColours(fix4,pal=seis)
  
  names.out <- colnames(network@data[3:48])
  namesnum <- as.numeric(gsub("Tmx_13_", "", colnames(network@data[3:48])))
  means <- colMeans(network@data[3:48])
  SDs <- colStdevs(network@data[3:48])
  yplus <- means + SDs
  yminus <- means - SDs
  df <- data.frame(means=means, SDs=SDs, names=namesnum)
  sequ <- c(1:46)
  namer <- sprintf('%03d', sequ)
  
    for (i in 28:35)
      {
        
        namey <- gsub("Tmx_13_", "", colnames(network@data)[i])
        
        filename <- paste0(mainPath, longBasin, "/Max_models/", yearPath, "/graphics2/", namer[i-2], ".png", sep="")
        png(filename=filename, res = 300, width = 1500, height = 1500, units = "px", bg="white")
        
        fix3 <- classIntervals(network@data[,i], n = 11, style = "fixed",fixedBreaks=c(-1,6,8,10,12,14,16,18,20,22,24))
        fix3.colors <- findColours(fix3,pal=seis)
        
        cexEr <- ifelse(abs(error_pts@data[,i]) <= 1, 0.5,
                        ifelse(abs(error_pts@data[,i])>1, 0.75,
                               ifelse(abs(error_pts@data[,i])>2, 1.0,
                                      ifelse(abs(error_pts@data[,i])>3, 1.25, NA))))
        
        plot(network, col=fix3.colors, bg="white", fg="black")
        points(error_pts, pch=16, col="gray40", cex=cexEr)
        
        legend("topright", fill = attr(fix3.colors, "palette"), title="8-day Max (°C)",legend = c("0-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", "20-22", "22+"), bty = "n", cex=.5, inset=c(0,0), text.col="black");
        legend("bottomright", pch=16, col="gray40", pt.cex=c(0.5, 0.75, 1.0, 1.25), title="Model error (°C)", legend = c("0-1","1-2","2-3","3+"), bty = "n", cex=.5, inset=c(0,0), text.col="black");
        
        mtext("Asotin 2013 8-day max (°C)",side =3, outer=TRUE, line=-3, col="black", cex=.7)
        mtext(paste0("Julian Day ", namey),side =1, outer=TRUE, line=-3, col="black", cex=.7)
        
        tmp2 <- subplot(
          plot(namesnum[1:(i-2)], means[1:(i-2)], col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,22), cex.main=.8, main="Basin mean", adj=0, xlab='',ylab='', col.lab="black", cex.axis=0.5, cex.lab = 0.25, col.axis="black", col.main = "black", bg="white"), 
          x=grconvertX(c(0.0,0.25), from='npc'), 
          y=grconvertY(c(0.80,0.95), from='npc'),
          size=c(1,1.5), vadj=0.5, hadj=0.5, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.5))
        
        
        dev.off()
      }
    
  
  
  