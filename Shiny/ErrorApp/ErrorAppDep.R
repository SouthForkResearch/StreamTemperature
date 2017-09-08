#
# This is a Shiny web application that asks for inputs as:
# 1. a file of temperature logger data with days in rows, and columns including SiteName, JulianDay, and DailyTemp
# 2. a file of logger site locality data with days in rows, and columns including SiteName, POINT_X, POINT_Y
# 3. a stream temp file with RCAID in the 1st column,then a series of 8D means as temp_1, temp_2, ... etc.
# 4. Update the "mainpath" variable to the path to the network shapefiles
# !diagnostics off

library(shiny)
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
library(readxl)
library(data.table)
library(rdrop2)
library(searchable)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
var <- "Mn"
varName <- "TMn"
tVar <- "Mean"
mainPath <- "https://www.dropbox.com/sh/juesdmyc9df8i60/"
seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
seis <- rev(seis)
namesnum <- c(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153,161,169,177,185,193,201,209,217,225,233,241,249,257,265,273,281,289,297,305,313,321,329,337,345,353,361)
namey <- sprintf('%03d', namesnum)

# Define UI for application that draws a map and calcs some stats
ui <- fluidPage(
  
  # Application title
  titlePanel("Model prediction error for new sites"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h5("This tool reads in your logger data and compares it to modeled mean stream temperature estimates."),
      selectizeInput(inputId = "year",
                  label = "Choose Year",
                  choices = c("", "2011", "2012", "2013", "2014", "2015"),
                  selected = NULL,
                  options = list(placeholder = 'Year')),
      selectizeInput(inputId = "basin",
                  label = "Choose Basin",
                  choices = c("", "Asotin", "Entiat", "JohnDay", "Lemhi", "Methow", "Minam-Wallowa", "MFSalmon-PantherCreek", "Pahsimeroi", "Potlatch", "Secesh", "Tucannon", "Wenatchee", "YankeeFork"),
                  selected = NULL,
                  options = list(placeholder = 'Basin')),
      
      helpText("Logger data file should be a .csv or .txt file with",
               "a minimum of 3 columns with headers: 'SiteName',",
               "'JulianDay', & 'DailyTemp'. Other columns will be ignored."),
      
      fileInput(inputId = "loggerData",
                label = "Logger Data file",
                accept =  c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      helpText("Site XY file should be a .csv or .txt file with",
               "a minimum of 3 columns with headers: 'SiteName',",
               "'Point_X' & 'Point_Y'. Other columns will be ignored.",
               "Coordinates should be in decimal degrees"),
      
      fileInput(inputId = "Sites",
                label = "Site XY file",
                accept =  c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      h5("The small inset graph shows the basin-wide summer mean temperature."),
      h5("Data, code, and descriptions of the models can be found ",
         a("here.", 
           href = "https://github.com/SouthForkResearch/StreamTemperature/wiki")),
      h6("Contact: Kris McNyset at kris -at- southforkresearch.org")
      
               
    ),
    
    # Show a plot of the generated error
    
    mainPanel(
      h5("(May take a few moments to load...)"),
      plotOutput(outputId = "meanGraph", width = 650, height = 750),
      h4("Root Mean Squared Error for each site (°C)"),
      tableOutput(outputId = "tableRMSE")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  loggers <- reactive({
    inFile <- input$loggerData
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
    
  })  
  
  sites <- reactive({
    inFile <- input$Sites
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
    
  })  
  

  output$meanGraph <- renderPlot({
    
    validate(
      need(input$Sites != "", "Please upload your logger/sites data")
    )
    
    Sites <- sites()
    Sites <- Sites[,c("SiteName", "POINT_X", "POINT_Y")]
    coordinates(Sites) <- ~POINT_X+POINT_Y
    proj4string(Sites) = CRS("+proj=longlat +datum=NAD83 +ellps=WGS84 +towgs84=0,0,0")
    
    Log.in <- loggers()
    Log.in <- Log.in[,c("SiteName", "JulianDay", "DailyTemp")]
    SiteID <- unique(Log.in$SiteName)
    SiteID <- as.matrix(SiteID)
    Log.8Day.out <- data.frame (mup = NULL)
    
    longBasin <- input$basin
    yearPath <- input$year
    yrPath <- substr(yearPath, start=3, stop=4)
    
    if(longBasin == "Pahsimeroi"){
      basinName <- "Pahs"
    } else if (longBasin == "Lochsa"){
      basinName <- "Lochsa"
    } else if (longBasin == "Lolo"){
      basinName <- "Lolo"
    } else if (longBasin == "LowerClearwater"){
      basinName <- "CW"
    } else if (longBasin == "MFSalmon - Panther Creek"){
      basinName <- "Panth"
    } else if (longBasin == "Minam-Wallowa"){
      basinName <- "MinW"
    } else if (longBasin == "UpperGrandRonde"){
      basinName <- "UGR"
    } else if (longBasin == "UpperSalmon"){
      basinName <- "USal"
    } else if (longBasin == "YankeeFork"){
      basinName <- "YF"
    } else {
      basinName <- substr(longBasin, start=1, stop=3)
    }
    
    #function for reading shapefiles from Dropbox
    drop_getShp <- function(my.file, dest=tempdir()){
      localfile = paste0(dest, "/", basename(my.file))
      drop_get(my.file, local_file = localfile, overwrite = TRUE)
    }
    
    netname <- paste(basinName, "_", yearPath, "_8D_", var, sep = "")
    modelPathNet <- paste0("StreamTemperatureModels/", longBasin, "/", yearPath, "/", tVar, sep = "")
    filesInfoNet <- drop_dir(paste0(modelPathNet, sep = ""))
    filesInfoNet <- data.table(filesInfoNet)
    
    #error check for whether model data exists
    validate(
      need(try(!is.null(filesInfoNet$path) ), "No model data for that basin/year/metric exists, please choose another.")
    )
    
    filesInfoNet <- filesInfoNet[path %like% netname]
    dest=tempdir()
    for(i in 1:dim(filesInfoNet)[1]){
      drop_getShp(filesInfoNet$path[i])
    }
    
    network <- readOGR(dsn=dest, layer=netname)
    
    names <- paste0(varName, yrPath, "_", namey)
    network@data <- network@data[,c('RCAID', names)]
    network@data$Mn <- NULL
    network@data$Mn <- rowMeans(network@data[,21:32])
    
   
    rcaName <- paste0(basinName, "_", "RCAID")
    
    modelPathRCA <- paste0("StreamTemperatureModels/", longBasin, sep = "")
    filesInfoRCA <- drop_dir(paste0(modelPathRCA, sep = ""))
    filesInfoRCA <- data.table(filesInfoRCA)
    filesInfoRCA <- filesInfoRCA[path %like% rcaName]
    
    dest=tempdir()
    for(i in 1:dim(filesInfoRCA)[1]){
      drop_getShp(filesInfoRCA$path[i])
    }
    
    rca <- readOGR(dsn=dest, layer = rcaName)
    rca <- spTransform(rca, proj4string(network))
    
    error_pts <- spTransform(Sites, proj4string(network))
    errorData <- over(error_pts, rca)
    error_pts$RCAID <- errorData$RCAID
    
    dates <- matrix(nrow=365, ncol=1)
    dates[,1] <- 1:365
    colnames(dates) <- c("Days")
    
    for (i in SiteID)
      {
        Log.site <- Log.in[Log.in$SiteName == i,]
        full.year <- merge(dates, Log.site, by.x = "Days", by.y = "JulianDay", all.x = TRUE)
        eightday <- rollapply(full.year$DailyTemp, width = 8, FUN = mean, na.rm = T, fill = NA, align = c("left"))
        eightday <- as.matrix(eightday)
        full.year$Mn8D<- as.vector(eightday)
        full.year$Mn8D[361] <- mean(full.year$DailyTemp[361:365])
        Log.8Day.out <- rbind(Log.8Day.out, full.year)
        
      }
    
    ind <- apply(Log.8Day.out, 1, function(x) !any(is.na(x)))
    Log.8Day.out <- Log.8Day.out[ind,]
    
    
    Pred.in <- network@data[,c('RCAID', names)]
    colnames(Pred.in)<-gsub(varName, "", colnames(Pred.in))
    colnames(Pred.in)<-gsub(paste0(yrPath, "_"), "", colnames(Pred.in))
    namesnum <- as.numeric(colnames(Pred.in[2:47]))
    
    Log.8D <- merge(Log.8Day.out, error_pts@data, by="SiteName")
    
    Pred.Log.out <- data.frame (mup = NULL)
    
    SiteID <- unique(Log.8D$SiteName)
    
    for (i in SiteID)
      {
        Log.site <- Log.8D[Log.8D$SiteName  == i,]
        Log.site <- as.data.frame(Log.site)
        
        RCAID <- unique(Log.8D$RCAID[Log.8D$SiteName == i])
        
        Pred.site <- matrix(ncol=2, nrow=46)
        Pred.site[,1] <- as.numeric(unlist(colnames(Pred.in)[2:47]))
        Pred.site[,2] <- unlist(Pred.in[RCAID,2:47])
        Pred.site <- data.frame(Pred.site)
        colnames(Pred.site) <- c("JulDay", "Pred")
        
        Pred.Log.site <- merge(Pred.site, Log.site, by.x = "JulDay",by.y = "Days", all.x=TRUE, all.y = FALSE)
        
        Pred.Log.out <- rbind(Pred.Log.out, Pred.Log.site)
      }
    
    ind <- apply(Pred.Log.out, 1, function(x) !any(is.na(x)))
    NoNA.pred <- Pred.Log.out[ind,]
    NoNA.pred <- NoNA.pred[,c('Mn8D', 'Pred', 'JulDay', 'SiteName')]
    
    SiteID <- unique(NoNA.pred$SiteName)
    SiteID <- as.matrix(SiteID)
    Error.pts.out <- matrix(nrow = length(SiteID), ncol = 47)
    
    errorName <- paste0("JulDay_", namesnum)
    colnames(Error.pts.out)[2:47] <- errorName
    colnames(Error.pts.out)[1] <- "SiteName"
    
    Error.pts.out[1:length(SiteID), 1] <- unlist(SiteID)[1:length(SiteID)]
    Error.pts.out <- as.data.frame(Error.pts.out, stringsAsFactors = FALSE)
  
    
    for (i in SiteID) 
      { 
        error.site <- NoNA.pred[NoNA.pred$SiteName  == i,]
        error.site$error <- error.site[,'Mn8D']-error.site[,'Pred']
        
        
        error <- matrix(ncol=1, nrow=46)
        error[,1] <- namesnum
        error <- data.frame(error)
        colnames(error) <- c("JulDay")
        
        error.site.fill <- merge(error, error.site, by = "JulDay", all.x=TRUE, all.y = FALSE)
        Error.pts.out[Error.pts.out$SiteName==i,2:47] <- as.numeric(unlist(error.site.fill$error))
      }
    
    Error.pts.out[,2:47] <- sapply(Error.pts.out[,2:47], as.numeric)
    
    Error.pts.out[,2:47] <- round(Error.pts.out[,2:47], digits=3)
    
    Error.sites.out <- merge(Error.pts.out, Sites, by = "SiteName") 
    coordinates(Error.sites.out) <- ~POINT_X+POINT_Y
    proj4string(Error.sites.out) = CRS("+proj=longlat +datum=NAD83 +ellps=WGS84 +towgs84=0,0,0")
    Error.sites.out <- spTransform(Error.sites.out, proj4string(network))
    Error.sites.out@data$Mn <- NULL
    Error.sites.out$Mn <- abs(rowMeans(Error.sites.out@data[2:47]))
    
    namesnum <- as.numeric(gsub(paste0(varName, yrPath, "_"), "", colnames(network@data[2:47])))
    means <- colMeans(network@data[2:47], na.rm = TRUE)
    SDs <- colStdevs(network@data[2:47], na.rm = TRUE)
    yplus <- means + SDs
    yminus <- means - SDs
    df <- data.frame(means=means, SDs=SDs, names=namesnum)
    sequ <- c(1:46)
    namer <- sprintf('%03d', sequ)
    fix4 <- classIntervals(means, n = 10, style = "fixed",fixedBreaks=c(-1,2,4,6,8,10,12,14,16,18))
    fix4.colors <- findColours(fix4,pal=seis)
    
      
    fix3 <- classIntervals(network$Mn, n = 10, style = "fixed",fixedBreaks=c(2,4,6,8,10,12,14,16,18,20))
    fix3.colors <- findColours(fix3,pal=seis)
    
    cexEr <-ifelse(abs(Error.sites.out$Mn) == 0, 0,
                   ifelse(abs(Error.sites.out$Mn)>0 & abs(Error.sites.out$Mn)<1, 1.0,
                          ifelse(abs(Error.sites.out$Mn)>1 & abs(Error.sites.out$Mn)<2, 1.5,
                                 ifelse(abs(Error.sites.out$Mn)>2 & abs(Error.sites.out$Mn)<3, 2.0,
                                        ifelse(abs(Error.sites.out$Mn)>3, 2.5,
                                               ifelse(abs(Error.sites.out$Mn)== NA, 0, NA))))))
    
    plot(network, col=fix3.colors, bg="white", fg="black",lwd=2)
    points(Error.sites.out, pch=16, col="gray40", cex=cexEr)
    
    legend("right", fill = attr(fix3.colors, "palette"), title="°C", legend = c("2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18+"), bty = "n", cex=1.0, inset=c(0,0), text.col="black");
    legend("bottomright", pch=16, col="gray40", pt.cex=c(1.0, 1.5, 2.0, 2.5), title="Model error (°C)", legend = c("0-1","1-2","2-3","3+"), bty = "n", cex=1.0, inset=c(0,0), text.col="black");
    
    mtext(paste0(longBasin, " ",  yearPath, " 8-day Summer ", tVar, " (°C)"),side =3, outer=TRUE, line=-3, col="black", cex=1.0)
    
    tmp2 <- subplot(
      plot(namesnum, means, col=fix4.colors, pch=16, bty="n", xlim=c(0,360), ylim=c(0,22), cex.main=1.0, main="Basin mean", adj=0, xlab='',ylab='', col.lab="black", cex.axis=0.8, cex.lab = 0.75, col.axis="black", col.main = "black", bg="white"), 
      x=grconvertX(c(0.1,0.45), from='npc'), 
      y=grconvertY(c(0.05, 0.20), from='npc'),
      size=c(1,1.5), vadj=0.7, hadj=0.7, 
      pars=list( mar=c(0,0,0,0)+0.1, cex=0.9))
      
    output$tableRMSE <- renderTable({
      RMSE_all <- Error.sites.out@data[,c("SiteName", "Mn")]
      colnames(RMSE_all)[2] <- "RMSE"
      RMSE_all
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
  