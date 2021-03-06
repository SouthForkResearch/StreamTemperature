#
# This is a Shiny web application that asks for inputs as:
# 1. a file of temperature logger data with days in rows, and columns including SiteID, 
# 
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

var <- "Mn"
tvar <- "mn"
varName <- "Tmn"
mainPath <- "D:/OneDrive/work/research/Shiny/ErrorApp/shapes"
seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
seis <- rev(seis)



# Define UI for application that draws a map and calcs some stats
ui <- fluidPage(
  
  # Application title
  titlePanel("Error for sites"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year",
                  label = "Choose Year",
                  choices = c("2011", "2012", "2013", "2014"),
                  selected = "2013"),
      selectInput(inputId = "basin",
                  label = "Choose Basin",
                  choices = c("Entiat", "Lemhi", "Pahsimeroi", "Wenatchee"),
                  selected = "Wenatchee"),
      
      helpText("Logger data file should be a .csv or .txt file with",
               "a minimum of 3 columns with headers: 'SiteName',",
               "'JulianDay', & 'DailyTemp'. Other columns will be ignored."),
      
      fileInput(inputId = "loggerData",
                label = "Logger data file",
                accept =  c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      helpText("Site XY file should be a .csv or .txt file with",
               "a minimum of 3 columns with headers: 'SiteName',",
               "'Point_X' & 'Point_Y'. Other columns will be ignored."),
      
      fileInput(inputId = "Sites",
                label = "Site XY file",
                accept =  c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      actionButton("button", "Update")
      
               
    ),
    
    # Show a plot of the generated error
    
    mainPanel(
      plotOutput(outputId = "meanGraph", width = 650, height = 750),
      h4("RMSE for each site"),
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
    

    shortBasin <- input$basin
    yearPath <- input$year
    yrPath <- substr(yearPath, start=3, stop=4)
    basinName <- substr(shortBasin, start=1, stop=3)
    netname <- paste(basinName, "_", yearPath, "_8D_", var, sep = "")
    network <- readOGR(dsn=mainPath, layer = netname)
    network@data$Mn <- NULL
    network@data$Mn <- rowMeans(network@data[,22:33])
    
    Sites <- sites()
    Sites <- Sites[,c("SiteName", "POINT_X", "POINT_Y")]
    coordinates(Sites) <- ~POINT_X+POINT_Y
    proj4string(Sites) = CRS("+proj=longlat +datum=NAD83 +ellps=WGS84 +towgs84=0,0,0")
    rcaName <- paste0(basinName, "_", "RCAID")
    rca <- readOGR(dsn=mainPath, layer = rcaName)
    rca <- spTransform(rca, proj4string(network))
    
    error_pts <- spTransform(Sites, proj4string(network))
    errorData <- over(error_pts, rca)
    error_pts$RCAID <- errorData$rca_id
    
    dates <- matrix(nrow=365, ncol=1)
    dates[,1] <- 1:365
    colnames(dates) <- c("Days")
    
    Log.in <- loggers()
    Log.in <- Log.in[,c("SiteName", "JulianDate", "DailyTemp")]
    SiteID <- unique(Log.in$SiteName)
    SiteID <- as.matrix(SiteID)
    Log.8Day.out <- data.frame (mup = NULL)
    
    for (i in SiteID)
      {
        Log.site <- Log.in[Log.in$SiteName == i,]
        full.year <- merge(dates, Log.site, by.x = "Days", by.y = "JulianDate", all.x = TRUE)
        eightday <- rollapply(full.year$DailyTemp, width = 8, FUN = mean, na.rm = T, fill = NA, align = c("left"))
        eightday <- as.matrix(eightday)
        full.year$Mn8D<- as.vector(eightday)
        full.year$Mn8D[361] <- mean(full.year$DailyTemp[361:365])
        Log.8Day.out <- rbind(Log.8Day.out, full.year)
        
      }
    
    ind <- apply(Log.8Day.out, 1, function(x) !any(is.na(x)))
    Log.8Day.out <- Log.8Day.out[ind,]
    
    Pred.in <- network@data[,c(1,3:48)]
    colnames(Pred.in)<-gsub(varName, "", colnames(Pred.in))
    colnames(Pred.in)<-gsub(paste0("_", yrPath, "_"), "", colnames(Pred.in))
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
    
    namesnum <- as.numeric(gsub(paste0(varName, "_", yrPath, "_"), "", colnames(network@data[3:48])))
    means <- colMeans(network@data[3:48], na.rm = TRUE)
    SDs <- colStdevs(network@data[3:48], na.rm = TRUE)
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
    
    mtext(paste0(shortBasin, " ",  yearPath, " 8-day summer ", var, " (°C)"),side =3, outer=TRUE, line=-3, col="black", cex=1.0)
    
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
  