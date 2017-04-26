
gisPath <- paste0("D:/OneDrive/work/research/CHaMP/CHaMP_data/2012_temp_CHaMP")

var <- "Mn"
yearPath <- "2012"
basin <- "Wen"
tVar <- "mn"

netname <- paste0(basin, "_", yearPath, "_8D_", tVar)
outnetname <- paste0(basin, "_", yearPath, "_8D_", var)

network <- readOGR(dsn=gisPath, layer = netname)

colnames(network@data) <-gsub(paste0("T", tVar, "_"), paste0("T", var), colnames(network@data))


writeOGR(obj=network, dsn=gisPath, layer = paste0(outnetname), driver="ESRI Shapefile", overwrite_layer = TRUE)

