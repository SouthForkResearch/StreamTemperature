#
# This is a Shiny web application that asks for inputs as:
# 1. a file of stream reach fish population designation by RCAID
# 2. a stream temp file with RCAID in the 1st column,then a series of 8D means as temp_1, temp_2, ... etc.
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
library(ggvis)
library(magrittr)
library(data.table)
library(rdrop2)
library(searchable)


#########This part must be done once to authorize the dropbox acct and save the token:
#token <- drop_auth(new_user=TRUE)
#saveRDS(token, "droptoken.rds")
#####################################################

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
#mainPath <- "https://www.dropbox.com/sh/juesdmyc9df8i60/"
seis = c("#AA0000", "#D00000", "#F70000", "#FF1D00", "#FF4400", "#FF6A00", "#FF9000", "#FFB700", "#FFDD00", "#FFE200", "#BDFF0C", "#73FF1A", "#3FFA36", "#16F45A", "#00D08B", "#0087CD", "#0048FA", "#0024E3")
seis <- rev(seis)
purp6 = c("#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#6e016b")
YlGnBu = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494") 
namesnum <- c(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153,161,169,177,185,193,201,209,217,225,233,241,249,257,265,273,281,289,297,305,313,321,329,337,345,353,361)
namey <- sprintf('%03d', namesnum)
tVar <- "Mn"

# options(shiny.error = function() {
#   stop("Please select basin/year/metric")
# })

# Define UI for application that draws a map and calcs some stats
ui <- fluidPage(

  titlePanel("Stream Temperature Exceedence"),
  
  sidebarLayout(
    sidebarPanel(
      h5("This tool displays the total number of days the stream temperature is above the chosen threshold for Mean or Max temp, or below for Min."),
      selectizeInput(inputId = "year",
                  label = "Choose Year",
                  choices = c("", "2011","2012", "2013", "2014", "2015"),
                  selected = "2015",
                  options = list(placeholder = 'Year')),
      selectizeInput(inputId = "basin",
                  label = "Choose Basin",
                  choices = c("", "Asotin", "Entiat", "JohnDay", "Lemhi", "Methow", "Minam-Wallowa", "MFSalmon-PantherCreek", "Pahsimeroi", "Potlatch", "Secesh", "SForkSalmon", "Tucannon", "UpperGrandeRonde", "Wenatchee", "YankeeFork"),
                  selected = "Entiat",
                  options = list(placeholder = 'Basin')),
      selectInput(inputId = "metric",
                  label = "Choose temperature metric",
                  choices = c("Min", "Mean", "Max"),
                  selected = "Mean"),
      sliderInput(inputId = "dateRange", "Date Range", 1, 361, value = c(153, 249), step = 8),
      helpText("Date range in Julian Days, 8-day time-steps"),
      
      numericInput(inputId = "thresh",
                   label = "Temperature threshold",
                   value = 16),
      helpText("Temperature threshold in degrees C"),
      
      submitButton("Update View"),
      
      h5("The small inset graph shows the basin-wide mean of the selected metric across the year."),
      h5("Data, code, and descriptions of the models can be found ",
         a("here.", 
           href = "https://github.com/SouthForkResearch/StreamTemperature/wiki")),
      h6("Contact: Kris McNyset at kris -at- southforkresearch.org")
      
    ),
    
    mainPanel(
      h4("Total days meeting criteria over selected time period"),
      h5("(May take a few moments to load...)"),
      plotOutput(outputId = "meanGraph", width = 650, height = 750),
      h4("Percent of total stream length meeting criteria"),
      tableOutput(outputId = "tableMean")
    )
  )
)

# Define server logic required to draw the stream network and summary table
server <- function(input, output) {
  
  
 
  
  
  output$meanGraph <- renderPlot({

    # This part reads in all the input & sets naming conventions
    
    longBasin <- input$basin
    yearPath <- input$year
    minDate <- input$dateRange[1]
    maxDate <- input$dateRange[2]
    maxTemp <- input$thresh
    var <- input$metric
    
    if (var == "Min") {
      tVar <- "Min"
    } else if (var == "Mean"){
      tVar <- "Mn"
    } else {
      tVar <- "Mx"
    }
    
    if(longBasin == "Pahsimeroi"){
      basinName <- "Pahs"
    } else if (longBasin == "Lochsa"){
      basinName <- "Lochsa"
    } else if (longBasin == "Lolo"){
      basinName <- "Lolo"
    } else if (longBasin == "LowerClearwater"){
      basinName <- "CW"
    } else if (longBasin == "MFSalmon-PantherCreek"){
      basinName <- "Panth"
    } else if (longBasin == "JohnDay"){
      basinName <- "JD"
    } else if (longBasin == "Minam-Wallowa"){
      basinName <- "MinW"
    } else if (longBasin == "Sec"){
      basinName <- "Sec"
    } else if (longBasin == "SForkSalmon"){
      basinName <- "SFS"
    } else if (longBasin == "UpperGrandeRonde"){
      basinName <- "UGR"
    } else if (longBasin == "UpperSalmon"){
      basinName <- "USal"
    } else if (longBasin == "Minam-Wallowa"){
      basinName <- "MinW"
    } else if (longBasin == "YankeeFork"){
      basinName <- "YF"
    } else if (longBasin == "UpperGrandeRonde"){
      basinName <- "UGR"
    } else {
      basinName <- substr(longBasin, start=1, stop=3)
    }
    varName <- paste0("T",tVar)
    yrPath <- substr(yearPath, start=3, stop=4)
    
    netname <- paste(basinName, "_", yearPath, "_8D_", tVar, sep = "")
    
    # This part gets the directory structure and reads in the spatial data
    
    modelPath <- paste0("streamtemperaturemodels/", longBasin, "/", yearPath, "/", var, sep = "")
    
    filesInfo <- drop_dir(paste0(modelPath, sep = ""))
    filesInfo <- data.table(filesInfo)
    validate(
      need(try(!is.null(filesInfo$path_display) ), "No model data for that basin/year/metric exists, please choose another.")
    )
    filesInfo <- filesInfo[path_display %like% netname]
    dest=tempdir()
    drop_getShp <- function(my.file, dest=tempdir()){
      localfile = paste0(dest, "/", basename(as.character(my.file)))
      drop_download(my.file, local_path = localfile, overwrite = TRUE, dtoken=token)
    }
    
    for(i in 1:dim(filesInfo)[1]){
      drop_getShp(paste0(filesInfo[,"path_display"][i]))
    }
    
    network <- readOGR(dsn=dest, layer=netname)
    
    names <- paste0(varName, yrPath, "_", namey)
    network@data <- network@data[,c('RCAID', names)]
    colnames(network@data)[2:47] <- namesnum
    start <- which(colnames(network@data) == minDate)
    end <- which(colnames(network@data) == maxDate)
    
    rcaName <- paste0(basinName, "_", "RCAID")
    rcas <- drop_read_csv(paste0("StreamTemperatureModels/", longBasin, "/", rcaName, ".csv"),)
    
    rcaList <- rcas$RCAID
    net = network
             
    net@data$Mn <- NULL
    net@data$Mn <- rowMeans(net@data[,start:end])
    
    # calcs basin means for color ramp
    means <- colMeans(net@data[2:47], na.rm = TRUE)
    
    # counts days in time range
    days <- net@data[, start:end]
    
    # counts days above or below threshold (depending on if the metric is the min temp)
    if (tVar == "Min"){
      count <- (rowSums(days < maxTemp))*8
    } else{
      count <- (rowSums(days > maxTemp))*8
    }
    
    validate(
      need(sum(count, na.rm=TRUE) > 0, "No days meet the criteria.")
    )
    
    # add the count to the network data table
    net@data$count <- count
    summary_all <- quantile(count, na.rm = TRUE)
    nclr <- length(unique(count))
    
    # sets display parameters for specific basins, followed by a general set
    
    fix3 <- classIntervals(means, n = 10, style = "fixed",fixedBreaks=c(2,4,6,8,10,12,14,16,18,20))
    fix3.colors <- findColours(fix3,pal=seis)
    
    
      fix4 <- classIntervals(net@data$count, n = nclr, style = "equal")
      fix4.colors <- findColours(fix4,pal=purp6)
     
      if(basinName == "Pot"){
        plot(net, col=fix4.colors, bg="white", fg="black",lwd=2)
        legend("bottomright", 
               fill = attr(fix4.colors, "palette"), 
               title="Day above threshold", 
               legend = names(attr(fix4.colors, "table")), 
               bty = "n", 
               cex=1.0, 
               inset=c(0,0), 
               text.col="black")
        
        tmp2 <- subplot(
          plot(namesnum, 
               means, 
               col=fix3.colors, 
               pch=16, 
               bty="n", 
               xlim=c(0,362), 
               ylim=c(0,22), 
               cex.main=1.0, 
               main="Basin mean temp", 
               adj=0, xlab='',ylab='', 
               col.lab="black", 
               cex.axis=0.8, 
               cex.lab = 0.75, col.axis="black", col.main = "black", bg="white", abline(h=maxTemp, v=c(minDate,maxDate))), 
          x=grconvertX(c(0.0,0.43), from='npc'), 
          y=grconvertY(c(0.80, 0.95), from='npc'),
          size=c(1,1.5), vadj=0.7, hadj=0.7, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.9))
      } else if(basinName == "Tuc"){
        plot(net, col=fix4.colors, bg="white", fg="black",lwd=2)
        legend(x=grconvertX(c(0.0,0.20), from='npc'), 
               y=grconvertY(c(0.28, 0.58), from='npc'), 
               fill = attr(fix4.colors, "palette"), 
               title="Number of days", 
               legend = names(attr(fix4.colors, "table")), 
               bty = "n", 
               cex=1.0, 
               inset=c(0,0), 
               text.col="black")
        
        tmp2 <- subplot(
          plot(namesnum, 
               means, 
               col=fix3.colors, 
               pch=16, 
               bty="n", 
               xlim=c(0,362), 
               ylim=c(0,22), 
               cex.main=1.0, 
               main="Basin mean temp", 
               adj=0, xlab='',ylab='', 
               col.lab="black", 
               cex.axis=0.8, 
               cex.lab = 0.75, col.axis="black", col.main = "black", bg="white", abline(h=maxTemp, v=c(minDate,maxDate))), 
          x=grconvertX(c(0.0,0.43), from='npc'), 
          y=grconvertY(c(0.00, 0.15), from='npc'),
          size=c(1,1.5), vadj=0.7, hadj=0.7, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.9))
      } else if(basinName == "YF"){
        plot(net, col=fix4.colors, bg="white", fg="black",lwd=2)
        legend("topleft", 
               fill = attr(fix4.colors, "palette"), 
               title="Number of days", 
               legend = names(attr(fix4.colors, "table")), 
               bty = "n", 
               cex=1.0, 
               inset=c(0,0), 
               text.col="black")
        
        tmp2 <- subplot(
          plot(namesnum, 
               means, 
               col=fix3.colors, 
               pch=16, 
               bty="n", 
               xlim=c(0,362), 
               ylim=c(0,22), 
               cex.main=1.0, 
               main="Basin mean temp", 
               adj=0, xlab='',ylab='', 
               col.lab="black", 
               cex.axis=0.8, 
               cex.lab = 0.75, col.axis="black", col.main = "black", bg="white", abline(h=maxTemp, v=c(minDate,maxDate))), 
          x=grconvertX(c(0.0,0.43), from='npc'), 
          y=grconvertY(c(0.00, 0.15), from='npc'),
          size=c(1,1.5), vadj=0.7, hadj=0.7, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.9))
        } else {
        plot(net, col=fix4.colors, bg="white", fg="black",lwd=2)
        legend("topright", 
               fill = attr(fix4.colors, "palette"), 
               title="Number of days", 
               legend = names(attr(fix4.colors, "table")), 
               bty = "n", 
               cex=1.0, 
               inset=c(0,0), 
               text.col="black")
          
        tmp2 <- subplot(
          plot(namesnum, 
               means, 
               col=fix3.colors, 
               pch=16, 
               bty="n", 
               xlim=c(0,362), 
               ylim=c(0,22), 
               cex.main=1.0, 
               main="Basin mean temp", 
               adj=0, xlab='',ylab='', 
               col.lab="black", 
               cex.axis=0.8, 
               cex.lab = 0.75, col.axis="black", col.main = "black", bg="white", abline(h=maxTemp, v=c(minDate,maxDate))), 
          x=grconvertX(c(0.0,0.43), from='npc'), 
          y=grconvertY(c(0.00, 0.15), from='npc'),
          size=c(1,1.5), vadj=0.7, hadj=0.7, 
          pars=list( mar=c(0,0,0,0)+0.1, cex=0.9))
      }
        
      
    
    output$tableMean <- renderTable({
      popData <- rcas[rcas$RCAID %in% rcaList,]
      popPerc <- merge(popData, net@data, by='RCAID')
      popSum <- sum(popPerc$Shape_Leng)
      dat <- data.table(popPerc)
      totes <- dat[,list(sum=sum(Shape_Leng)), by=count]
      totes$count <- as.integer(totes$count)
      totes$sum <- round((totes$sum/popSum)*100, 1)
      totes <- totes[order(totes$count),]
      #totes$cumtot <- cumsum(totes$sum)
      colnames(totes) <- c("# days", "% stream length")
      head(totes, n = dim(totes)[1], digits=0)
     })
  })
}

shinyApp(ui = ui, server = server)
  
