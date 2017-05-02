library(uuid)

################################################################
# Generate project.rs.xml file starting here!!!!
################################################################

basin <- "Wen"
midBasin <- "Wenatchee"
longBasin <- "Wenatchee"
yrPath <- "12"
yearPath <- "2012"
dataPath <- "D:/OneDrive/work/research/CHaMP/GIS/LST/LST_s1_"
mainPath <- "D:/OneDrive/work/research/CHaMP/CHaMP_data/"
metricPath <- "D:/Dropbox/StreamTemperatureModels/"
var <- "Min"
longVar <- "Min"
nameBasin <- "Wenatchee"
modelPath <- paste0(mainPath, longBasin, "/", yearPath, "/Min/")
gisPath <- "D:/OneDrive/work/research/CHaMP/GIS/coverages/"

Realizguid = UUIDgenerate(F)

now <- Sys.Date()

#########################################################
# Start wriging project.rs.xml file here
#########################################################

cat("", file=paste(modelPath,"project.rs.xml",sep=""))

cat(paste("","<?xml version=\"1.0\" encoding=\"utf-8\"?>
<Project xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:noNamespaceSchemaLocation=\"XSD/V1/Project.xsd\">

  <Name>stream_temperature_project</Name>
  <ProjectType>STREAMTEMP</ProjectType>

  <MetaData>
    <!--This first metadata exists only to place this project in the riverscapes project-->
    <Meta name=\"HUCID\">17060201</Meta>
		<Meta name=\"Region\">CRB</Meta>
    <Meta name=\"Watershed\">",longBasin,"</Meta>
  </MetaData>

  
  <Inputs>
  </Inputs>

  <Realizations>
    <StreamTemperature guid=",Realizguid," id=\"streamtemp\" dateCreated=\"",now,"\" productVersion=\"1.0\">
      <Name>Stream Temperature for ",nameBasin, " ",yearPath,"</Name>

      <MetaData>
        <!--This next metadata relates to this particular realization-->
        <Meta name=\"MODIS_dataset\">MOD11A2v005</Meta>
        <Meta name=\"EOSDIS\">https://reverb.echo.nasa.gov/reverb/</Meta>
        <Meta name=\"Logger_data_source\">www.champmonitoring.org</Meta>
        <Meta name=\"Code_wiki\">https://github.com/SouthForkResearch/StreamTemperature/wiki</Meta>
      </MetaData>

      <Analyses>
        <Analysis>
          <Name>Stream Temp Analysis</Name>
          <Outputs>

            <CSV id=\"coeffs\">
              <Name>Model_coefficients</Name>
              <Path>All_data_",basin,"_",yearPath,"_mod_coeffs_",var,".csv</Path>
            </CSV>

            <CSV id=\"metrics\">
              <Name>Model_metrics</Name>
              <Path>All_data_",basin,"_",yearPath,"_mod_metrics_",var,".csv</Path>
            </CSV>

            <CSV id=\"error\">
              <Name>Model_error</Name>
              <Path>Error_",basin,"_",yearPath,"_8D_",var,".csv</Path>
            </CSV>

            <CSV id=\"estimates\">
              <Name>Model_data_prediction</Name>
              <Path>jk_pred_v_y_",var, "_", basin,"_",yearPath,"_sp_fall.csv</Path>
            </CSV>

            <CSV id=\"modelData\">
              <Name>Model_input_data</Name>
              <Path>", basin,"_",yearPath,"_8D_",var,"_model_data.csv</Path>
            </CSV>
          
            <Image id=\"still\">
              <Name>Network_estimate_snapshot</Name>
              <Path>graphics/Stills/",basin,"_",yearPath,"_8D_",var,".png</Path>
            </Image>

            <Video id=\"movie\">
              <Name>Network_annual_estimate_animation</Name>
              <Path>graphics/",basin,"_",yearPath,"_8D_",var,".mpeg</Path>
            </Video>

            <Vector id=\"network\">
              <Name>Network_estimate_shapefile</Name>
              <Path>",basin,"_",yearPath,"_8D_",var,".shp</Path>
            </Vector>

            <Vector id=\"errorPts\">
              <Name>Error_by_site_points</Name>
              <Path>",basin,"_Error_",yearPath,"_8D_",var,".shp</Path>
            </Vector>

          </Outputs>
        </Analysis>
      </Analyses>
    </StreamTemperature>
  </Realizations>
</Project>
",sep=""), file=paste(modelPath,"project.rs.xml",sep=""), append=T)

