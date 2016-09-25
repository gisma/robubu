demCorrection<- function(demFn ,df,p,altFilter,followSurface,followSurfaceRes,logger,projectDir){
  pb2<- pb2 <- txtProgressBar(max = 7, style = 3)
  if (is.null(demFn)){
    
    
    setTxtProgressBar(pb2, 1)
    levellog(logger, 'WARN', "CAUTION!!! no dem file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    cat("CAUTION!!! no dem file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    # download corresponding srtm data
    dem<-robubu::getGeoData(name="SRTM",xtent = extent(p$lon1,p$lon3,p$lat1,p$lat3), zone = 1.0,merge = TRUE)
    if(!is.null(dem)){
      file.copy(overwrite=TRUE,dem@file@name, paste0(file.path(projectDir,"data"),"/",basename(dem@file@name)))
    }
    retdem<-dem
    dem<- raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
    # extract the altitudes
    df$Altitude<- raster::extract(dem,df)
  } else {
    # read local dem file
    if (class(demFn)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
      dem<-demFn
      retdem<-dem
      raster::writeRaster(dem,"tmpdem.tif",overwrite=TRUE)
    } else{
      dem<-raster::raster(demFn)
      retdem<-dem
      raster::writeRaster(dem,"tmpdem.tif",overwrite=TRUE)
    }
    setTxtProgressBar(pb2, 2)
    # brute force projection check    
    if (is.null(dem@crs)) {stop("the DSM is not georeferencend")}
    else {
      demll<-gdalwarp(srcfile = "tmpdem.tif", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )  
    }
    setTxtProgressBar(pb2, 3)
    # fill gaps and extrapolate 
    #system(paste0("gdal_fillnodata.py   -md 500 -of GTiff ",demFn," filldem.tif"))
    
    if (as.numeric(p$flightAltitude)<as.numeric(50)){
      #cat("\n manipulating the DSM for low altitude flights...\n")
      # resample dem to followTerrainRes and UTM  
      tmpdem<-gdalwarp(srcfile = "demll.tif", dstfile = "tmpdem.tif", overwrite=TRUE,  t_srs=paste0("+proj=utm +zone=",long2UTMzone(p$lon1)," +datum=WGS84"),output_Raster = TRUE ,tr=c(as.numeric(followSurfaceRes),as.numeric(followSurfaceRes)))
      # deproject it again to latlon
      demll<-gdalwarp(srcfile = "tmpdem.tif", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )
      # export it to SAGA
      gdalwarp("demll.tif","demll.sdat", overwrite=TRUE,  of='SAGA')
      setTxtProgressBar(pb2, 4)
      # fill sinks (clearings) that are 0-30 meters deep
      ret<-system2("saga_cmd", c("ta_preprocessor 2", "-DEM=demll.sgrd", "-SINKROUTE=NULL", "-DEM_PREPROC='flightdem.sdat'", "-METHOD=1", "-THRESHOLD=1", "-THRSHEIGHT=30.000000"),stdout=TRUE, stderr=TRUE)
      if (grep("%okay",ret)){ setTxtProgressBar(pb2, 5)} #cat("filling clearings performs okay\n")}
      else {stop("Crucial Error in filling flight surface")}
      # smooth the result
      ret<-system2("saga_cmd", c("grid_filter 0","-INPUT='flightdem.sgrd'", "-RESULT='flightsurface.sdat'" ,"-METHOD=0", "-MODE=0" ,paste0("-RADIUS=",followSurfaceRes)),stdout=TRUE, stderr=TRUE)
      
      if (grep("%okay",ret)){ setTxtProgressBar(pb2, 6)}#cat("filtering flight surface performs okay\n")} 
      
      else {stop("Crucial Error in filtering flight surface")}
      
      #demll<-gdalwarp(srcfile = "flightsurface.sdat", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )
      # calculate the min max values to correct elevation errors from filtering
      
      demll<-raster("flightsurface.sdat",setMinMax=TRUE)
      demll<-setMinMax(demll)
      setTxtProgressBar(pb2, 7)
      tmpdem<-setMinMax(tmpdem)
      dem<-setMinMax(dem)
      altCor<-ceiling(maxValue(dem)-maxValue(demll))
      demll=demll+altCor
      
      levellog(logger, 'INFO', paste("altitude shift              : ",altCor,      "  (meter)")) 
    } 
    
    close(pb2)
    # find local minima/maxima
    #system2("saga_cmd shapes_grid 9 -GRID='dem.sgrd' -MINIMA=NULL -MAXIMA='max'")
    #max<-readOGR(".","max")
    # crop it for speeding up
    #dem<-raster::crop(tmpdem,extent(min(p$lon1,p$lon3,p$lon2)-0.009,max(p$lon1,p$lon2,p$lon3)+0.009,min(p$lat1,p$lat2,p$lat3)-0.007,max(p$lat1,p$lat2,p$lat3)+0.007))
  }
  
  # extract all waypoint altitudes
  altitude<-raster::extract(demll,df)
  # get maximum altitude of the task area
  maxAlt<-max(altitude,na.rm = TRUE)

  levellog(logger, 'INFO', paste("maximum DEM Altitude : ", maxAlt," m"))
  # if no manually provided launch altitude exist get it from DEM
  pos<-as.data.frame(cbind(p$launchLat,p$launchLon))
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  if (p$launchAltitude==-9999){
    tmpalt<-raster::extract(demll,pos)  
    p$launchAltitude<-as.numeric(tmpalt)
    # otherwise take it from the parameter set
  } else 
  {
    p$launchAltitude<-as.numeric(p$launchAltitude)
  }
  levellog(logger, 'INFO', paste("launching Altitude : ", p$launchAltitude," m"))
  launchAlt<-p$launchAltitude
  # calculate the flight altitude shift due to launching and max altitude
  p$flightAltitude=as.numeric(p$flightAltitude)+(maxAlt-as.numeric(launchAlt))
  rthFlightAlt<-p$flightAltitude
  p$rthAltitude=rthFlightAlt
  levellog(logger, 'INFO', paste("rthFlightAlt : ", rthFlightAlt," m"))
  
  if (followSurface) {
    altitude<-altitude+as.numeric(p$flightAltitude)-maxAlt
    df$altitude<-altitude
    
    if ( as.character(p$flightPlanMode) == "terrainTrack") {
      sDF<-as.data.frame(df@data)
      dif<-abs(as.data.frame(diff(as.matrix(sDF$altitude))))
      sDF<- sDF[-c(1), ]
      sDF$dif<-dif[,1]
      sDF[is.na(sDF)] <- 0
      fDF<-sDF[sDF$id=="99" | sDF$dif > altFilter , ]
      sDF<- sDF[-c(ncol(sDF),ncol(sDF)-1) ]
      fDF$lon<-fDF$longitude
      fDF$lat<-fDF$latitude
      fDF[complete.cases(fDF),]
      sp::coordinates(fDF) <- ~lon+lat
      sp::proj4string(fDF) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
      df<-fDF
    }
  }
  return(c(pos,df,demll,rthFlightAlt,launchAlt,maxAlt,p,retdem))
}

# export data to xternal format deals with the splitting of the mission files
writeDjiCSV <-function(df,mission,nofiles,maxPoints,p,logger,rth,trackSwitch=FALSE,dem,maxAlt){
  minPoints<-1
  row1<-df@data[1,1:(ncol(df@data))]
  launchLat<-df@data[1,1]
  launchLon<-df@data[1,2]
  
  for (i in 1:nofiles) {
    startLat<-df@data[minPoints,1]
    startLon<-df@data[minPoints,2]
    endLat<-df@data[maxPoints,1]
    endLon<-df@data[maxPoints,2]
    yhome <- c(launchLat,endLat)
    xhome <- c(launchLon,endLon)
    ystart <- c(launchLat,startLat)
    xstart <- c(launchLon,startLon)
    start<-SpatialLines(list(Lines(Line(cbind(xstart,ystart)), ID="start")))
    home<-SpatialLines(list(Lines(Line(cbind(xhome,yhome)), ID="home")))
    sp::proj4string(home) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    sp::proj4string(start) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # calculate minimum rth altitude
    homeRth<-max(unlist(raster::extract(dem,home)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    startRth<-max(unlist(raster::extract(dem,start)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    
    # calculate rth heading 
    homeheading<-geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    startheading<-geosphere::bearing(c(startLon,startLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    # claculate rt home ascent
    pos<-calcNextPos(endLon,endLat,homeheading,10)
    # generate rth waypoint
    heading<-homeheading
    altitude<-homeRth
    latitude<-pos[2]
    longitude<-pos[1]
    ascentrow<-cbind(latitude,longitude,altitude,heading,row1[5:12])
    homerow<-cbind(row1[1:2],altitude,heading,row1[5:12])
    heading<-startheading
    altitude<-startRth
    startrow<-cbind(row1[1:2],altitude,heading,row1[5:12])
    
    DF<-df@data[minPoints:maxPoints,]
    DF = rbind(startrow,DF)
    DF = rbind(DF,ascentrow)
    DF = rbind(DF,homerow)

    #if (maxPoints>nrow(DF)){maxPoints<-nrow(DF)}
    write.csv(DF[,1:(ncol(DF)-2)],file = paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,i,".csv"),quote = FALSE,row.names = FALSE)
    
    
    levellog(logger, 'INFO', paste("created : ", paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,"-",i,".csv")))
    minPoints<-maxPoints
    maxPoints<-maxPoints+94
    
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
}



# create the full argument list for one waypoint
makeUavPoint<- function(pos,uavViewDir,group,p,header=FALSE,sep=","){
  # create the value lines
  if (!header){
    # create camera action arguments
    action<-""
    for (i in seq(1:length(p$task[,1]))){ 
      action<-paste0(action,p$task[i,]$x[1],sep)
    }
    # create waypoint plus camera options
    tmp <-    paste0(pos[1],sep,pos[2],sep,pos[2],sep,pos[1],
                     sep,as.character(p$flightAltitude),
                     sep,as.character(uavViewDir),
                     sep,as.character(p$curvesize),
                     sep,as.character(p$rotationdir),
                     sep,as.character(p$gimbalmode),
                     sep,as.character(p$gimbalpitchangle),
                     sep,action,
                     group)
  }
  # create the header
  else {
    action<-""
    for (i in seq(1:length(p$task[,1]))){ 
      action<-paste0(action,p$task[i,]$actionNames[1],sep)
    }
    tmp <-    paste0("lon",sep,"lat",sep,"latitude",sep,"longitude",sep,
                     "altitude",sep,
                     "heading",sep,
                     "curvesize",sep,
                     "rotationdir",sep,
                     "gimbalmode",sep,
                     "gimbalpitchangle",sep,
                     action,"id")    
  }
}



# write autonoumous flight track to MAV format 
# deals with the splitting of the mission files

writeMavCSV <-function(df,mission,rawTime,flightPlanMode,trackDistance,batteryTime,logger,p,len,multiply,tracks,param,speed=180,uavType){
  nofiles<-1
  minPoints<-1
  nofiles<- ceiling(rawTime/batteryTime)
  maxPoints<-ceiling(nrow(df@data)/(rawTime/batteryTime))
  mp<-maxPoints
  
  for (i in 1:nofiles) {
    
    # if maxpoints is greater than the existing number of points reset it
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
    
    # write and re-read waypoints
    sep<-"\t"
    keeps <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","j")
    df@data<-df@data[keeps]
    write.table(df@data[minPoints:maxPoints,1:(ncol(df@data))],file = "tmp.csv",quote = FALSE,row.names = FALSE,sep = "\t",)
    lns <- data.table::fread("tmp.csv", skip=1L, header = FALSE,sep = "\n", data.table = FALSE)
    lnsnew<-data.frame()
    
    # create default header line  
    lnsnew[1,1] <- "QGC WPL 110"
    # create homepoint 
    lnsnew[2,1] <-       paste0("0",sep,"1",sep,"0",sep,"16",sep,"0",sep,"0",sep,"0",sep,"0",sep,p$launchLat,sep,p$launchLon,sep,as.character(param$launchAltitude),sep,"1")
    # CREATE takeoff
    lnsnew[3,1] <-       paste0("1",sep,"0",sep,"3",sep,"22",sep,"200.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,as.character(param$flightAltitude),sep,"1")
    #set mission speed
    lnsnew[4,1] <-       paste0("2",sep,"0",sep,"3",sep,"178",sep,"0.0",sep,speed,sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    # create "normal" waypoints
    for (j in 1:length(lns[,1])) {
      lnsnew[j+4,1]<-paste0(as.character(j+2),"\t",lns[j,])
    }
    #set rth altitude
    lnsnew[length(lnsnew[,1])+1,1]<-  paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"30",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,param$rthAltitude,sep,"1")
    #set max return speed
    lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"178",sep,"0.0",sep,"250",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    # trigger rth event
    lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"20",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    # write the control file
    write.table(lnsnew, paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,i,"_solo.waypoints"), sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE,na = "")
    # log event 
    levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
    minPoints<-maxPoints
    maxPoints<-maxPoints+mp
    if (maxPoints>nrow(df@data)){
      maxPoints<-nrow(df@data)
    }
  }
  
}


# create the full argument list for one waypoint in MAV format
makeUavPointMAV<- function(pos,uavViewDir,group,p,header=FALSE,sep="\t",speed="180"){
  # create the value lines
  if (!header){
    # CREATE NORMAL WAYPOINT
    tmp <-    paste0("0",sep,"3",sep,"16",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,pos[2],sep,pos[1],sep,pos[2],sep,pos[1],sep,as.character(p$flightAltitude),sep,group,sep,"1\n")
    
  }
  # create the header
}

getSurveyExtent<- function(surveyArea,projectDir,logger){

  # check and read mission area coordinates
  if (is.null(surveyArea)) {
    levellog(logger, 'FATAL', '### external flight area file or coordinates missing - dont know what to to')
    stop("### external flight area file or coordinates missing - don't know what to to")
  }
  else {
    # import flight area if provided by an external vector file
    if (class(surveyArea)=="numeric" & length(surveyArea)>= 8){
      surveyArea<-surveyArea
    }
    else if (class(surveyArea)=="numeric" & length(surveyArea)< 8){
      levellog(logger, 'FATAL', "### you did not provide a launching coordinate")
      stop("### you did not provide a launching coordinate")
    }
    else {
      file.copy( from = surveyArea, to = file.path(projectDir,"data"),)
      test<-try(flightBound<-readExternalFlightBoundary(surveyArea))
      if (class(test)!="try-error"){
        surveyArea<-flightBound 
      }else{
        levellog(logger, 'FATAL', "### can not find/read input file")        
        stop("### could not read surveyArea file")
      }
    }
  }
  return(surveyArea)
}

# imports the task are either from a json or kml file
importsurveyArea<- function(fN){
  # read shapefile
  if (path.expand(extension(fN)) == ".json") 
    flightBound<-rgdal::readOGR(dsn = path.expand(fN), layer = "OGRGeoJSON",verbose = FALSE)
  else if (path.expand(extension(fN)) != ".kml" ) 
    flightBound<- rgdal::readOGR(dsn = path.expand(dirname(fN)), layer = tools::file_path_sans_ext(basename(fN)),pointDropZ=TRUE,verbose = FALSE)
  else if (path.expand(extension(fN)) == ".kml" ) {
    flightBound<- rgdal::readOGR(dsn = path.expand(fN), layer = tools::file_path_sans_ext(basename(fN)),pointDropZ=TRUE,verbose = FALSE)    
  }
  return(flightBound)
  
}

#  or as a list of for coordinates
readExternalFlightBoundary<- function(fN,extend=FALSE){
  flightBound<-importsurveyArea(fN)
  sp::spTransform(flightBound, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  if (extend){
    x<-raster::extent(flightBound)
    # first flightline used for length and angle of the parallels
    
    lon1<-x@xmin # startpoint
    lat1<-x@ymin # startpoint
    lon2<-x@xmin # endpoint
    lat2<-x@ymax # endpoint
    lon3<-x@xmax # crosswaypoint
    lat3<-x@ymax # crosswaypoint
    if (class(flightBound)=="SpatialPolygonesDataFrame") {
      lauchLon<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,1] 
      launchLat<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,2]  
    } else if(class(flightBound)=="SpatialLinesDataFrame") {
      launchLon<-flightBound@lines[[1]]@Lines[[1]]@coords[7,1] 
      launchLat<-flightBound@lines[[1]]@Lines[[1]]@coords[7,2]
    }
  } else{
    if (class(flightBound)=="SpatialPolygonesDataFrame") {
      
      lon1<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,1] 
      lat1<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,2] 
      
      lon2<-flightBound@polygons[[1]]@Polygons[[1]]@coords[2,1] 
      lat2<-flightBound@polygons[[1]]@Polygons[[1]]@coords[2,2] 
      
      lon3<-flightBound@polygons[[1]]@Polygons[[1]]@coords[3,1] 
      lat3<-flightBound@polygons[[1]]@Polygons[[1]]@coords[3,2] 
      
      lauchLon<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,1] 
      launchLat<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,2]       
    }
    if (class(flightBound)=="SpatialLinesDataFrame") {
      
      lon1<-flightBound@lines[[1]]@Lines[[1]]@coords[1,1] 
      lat1<-flightBound@lines[[1]]@Lines[[1]]@coords[1,2] 
      
      lon2<-flightBound@lines[[1]]@Lines[[1]]@coords[3,1] 
      lat2<-flightBound@lines[[1]]@Lines[[1]]@coords[3,2] 
      
      lon3<-flightBound@lines[[1]]@Lines[[1]]@coords[5,1] 
      lat3<-flightBound@lines[[1]]@Lines[[1]]@coords[5,2]
      
      launchLon<-flightBound@lines[[1]]@Lines[[1]]@coords[7,1] 
      launchLat<-flightBound@lines[[1]]@Lines[[1]]@coords[7,2]
      
    }
    
    
  }
  return(c(lat1,lon1,lat2,lon2,lat3,lon3,launchLat,launchLon))
}


#  function to start litchi as a local instance
openLitchi<- function(){
  tempDir <- tempfile()
  dir.create(tempDir)
  currentfiles<-list.files(paste0(.libPaths()[1],"/robubu/htmlwidgets/lib/litchi"))
  dir.create(file.path(tempDir, currentfiles[1]))
  currentfiles<-list.files(paste0(.libPaths()[1],"/robubu/htmlwidgets/lib/litchi/"))
  
  file.copy(from=paste0(.libPaths()[1],"/robubu/htmlwidgets/lib/litchi"), to=file.path(tempDir), 
            overwrite = TRUE, recursive = TRUE, 
            copy.mode = TRUE)
  
  htmlFile <- file.path(tempDir, "litchi","index.html")
  # (code to write some content to the file)
  utils::browseURL(htmlFile)
  
}


# calculate the overlap factor of the camera footprints returning an heatmap
fovHeatmap<- function(footprint,dem){
  p<-split(footprint,footprint@plotOrder)
  t <- raster::raster(nrow=nrow(dem)*2,ncol=ncol(dem)*2)
  t@crs <-dem@crs
  t@extent<-dem@extent
  
  t<-resample(dem,t)
  t[]<-0
  s<-t
  for (i in seq(1:length(footprint))) {
    tmp<-raster::rasterize(p[[i]],t)
    s <- raster::stack(tmp, s)
  }
  fovhm <- raster::stackApply(s, indices= nlayers(s), fun=sum)
  fovhm[fovhm<1]=NaN
  return(fovhm)
}

# create a sppolygon to estimate the pictures footprint 
taskarea<- function(p,csvFn){
  # construct the 4th corner
  crossdir<-geosphere::bearing(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  crosslen<-geosphere::distGeo(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  p4<-geosphere::destPoint(c(p$lon1,p$lat1), crossdir,crosslen)
  # create SPDF
  ID = paste0("FlightTask_",basename(csvFn))
  rawPolygon <- sp::Polygon(cbind(c(p$lon1,p$lon2,p$lon3,p4[[1]],p$lon1),c(p$lat1,p$lat2,p$lat3,p4[[2]],p$lat1)))
  areaExtent <- sp::Polygons(list(rawPolygon), ID = ID)
  areaExtent <- sp::SpatialPolygons(list(areaExtent))
  df <- data.frame( ID=1:length(rawPolygon), row.names = ID)
  area <- sp::SpatialPolygonsDataFrame(areaExtent, df)
  sp::proj4string(area) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(area)
} 

# calculates the camera footprint 
cameraExtent<- function(lon,lat,heading,distance,flightaltitude,i,j){
  
  t1<-calcNextPos(lon,lat,abs(heading),1.71*flightaltitude/2)
  t2<-calcNextPos(lon,lat,abs(heading),-1*(1.71*flightaltitude/2))
  
  
  yllc<-calcNextPos(t1[1],t1[2],-90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xllc<-calcNextPos(t1[1],t1[2],-90+ abs(heading),1.71*flightaltitude*0.75/2)[1]
  ylrc<-calcNextPos(t1[1],t1[2],90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xlrc<-calcNextPos(t1[1],t1[2],90+abs(heading),1.71*flightaltitude*0.75/2)[1]
  
  yulc<-calcNextPos(t2[1],t2[2],-90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xulc<-calcNextPos(t2[1],t2[2],-90+abs(heading),1.71*flightaltitude*0.75/2)[1]
  yurc<-calcNextPos(t2[1],t2[2],90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xurc<-calcNextPos(t2[1],t2[2],90+abs(heading),1.71*flightaltitude*0.75/2)[1]
  
  ID = paste0("CameraExtend_",flightaltitude,"_",lon,lat)
  rawPolygon <- sp::Polygon(cbind(c(xulc,xurc,xlrc,xllc,xulc),c(yulc,yurc,ylrc,yllc,yulc)))
  tileExtend <- sp::Polygons(list(rawPolygon), ID = ID)
  tileExtend <- sp::SpatialPolygons(list(tileExtend))
  df <- data.frame( ID=1:length(rawPolygon), row.names = ID)
  frame <- sp::SpatialPolygonsDataFrame(tileExtend, df)
  sp::proj4string(frame) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(frame)
} 

getPresetTask<- function (param="remote"){
  #' shows existing camera action presets 
  #' @description 
  #' NOTE: only for flightPlanMode = "waypoint")
  # preset waypoints & orthophoto
  
  if  (param == "multi_ortho") {
    flightParams=actiontype=c(1,0,4,0,5,-60,1,0,4,90,1,0,4,180,1,0,4,270,1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  # preset waypoints  take vertical picture at wp
  else if (param == "simple_ortho") { 
    flightParams=actiontype=c(5,-90,1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "simple_pano") { 
    flightParams=actiontype=c(4,-180,1,0,4,-128,1,0,4,-76,1,0,4,-24,1,0,4,28,1,0,4,80,1,0,4,132,1,0,-1,0) 
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }  # preset waypoints  take vertical picture at wp
  else if (param == "remote") { 
    flightParams=actiontype=c(-1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "treetop") { 
    flightParams=actiontype=c(0,1000,5,-90,1,0,1,0,5,-70,1,0,4,-90,1,0,4,90,5,-30,-1,0,-1,0,-1,0,-1,0,-1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "nothing") { 
    flightParams=actiontype=c(-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  return(task)
}

# create and recalculates all arguments for a drone waypoint
makeFlightParam<- function(surveyArea,flightParams,followSurface){
  # retrieve and recalculate the arguments to provide the flight paramaer for litchi
  validPreset<-c("multi_ortho","simple_ortho","simple_pano","remote","treetop","nothing")
  validFlightPlan<-c("waypoints","track","manual")
  stopifnot(flightParams["presetFlightTask"] %in% validPreset)
  stopifnot(flightParams["flightPlanMode"] %in% validFlightPlan)
  
  if (followSurface == TRUE){
    flightParams["flightPlanMode"] = "terrainTrack"
  }
  
  p<-list()
  
  # preset camera action at waypoints 
  task<-getPresetTask(flightParams["presetFlightTask"])  
  
  # flight area coordinates either from external file or from argument list
  p$lat1<-surveyArea[1]
  p$lon1<-surveyArea[2]
  p$lat2<-surveyArea[3]
  p$lon2<-surveyArea[4]
  p$lat3<-surveyArea[5]
  p$lon3<-surveyArea[6]
  p$launchLat<- surveyArea[7]
  p$launchLon<- surveyArea[8]
  p$launchAltitude<-flightParams["launchAltitude"]
  # rest of the arguments  
  p$flightPlanMode<- flightParams["flightPlanMode"] # waypoints, terrainTrack track
  p$flightAltitude<-flightParams["flightAltitude"]  # planned static altitude above ground (note from starting point)
  p$curvesize<-flightParams["curvesize"]      # default may be set t0 zero
  p$rotationdir<-flightParams["rotationdir"]      # default nothing
  p$gimbalmode<-flightParams["gimbalmode"]       # default nothing 
  p$gimbalpitchangle<-flightParams["gimbalpitchangle"] # default nothing
  p$overlap<-overlap<-flightParams["overlap"]    # overlapping factor 0-1 default 0.6
  p$task<-task  # camera task
  return(p)
}

# creates task paramter list
makeTaskParamList<- function(x) {
  actionNames<-list()
  j<-1
  for (i in seq(1:(length(x)/2)) ){
    actionNames[j]<-paste0("actiontype",i)
    actionNames[j+1]<-paste0("actionparam",i) 
    j=j+2
  }
  return(cbind(actionNames,x))
  
}

# calculate a new position from given lat lon
calcNextPos<- function(lon,lat,heading,distance){
  p<-geosphere::destPoint(c(lon,lat), heading, distance)
  return(c(p[1],p[2]))
}




dumpFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

# calculates the overall flight distance
calcTrackDistance<- function (fliAltRatio,flightAltitude,factor=1.71){
  
  trackDistance<-(fliAltRatio*(factor*flightAltitude))
  
}



calculateFlightTime<- function(maxFlightTime,windCondition,maxSpeed,uavOptimumspeed,flightLength,totalTrackdistance,picRate,logger) {
  # wind speed adaption for reducing the lifetime of the battery Roughly the Beaufort scale is used
  
  if (windCondition==1){
    windConditionFactor<-1
  } else if (windCondition==2){
    windConditionFactor<-0.8
  } else if (windCondition==3){
    windConditionFactor<-0.6
  } else if (windCondition==4){
    windConditionFactor<-0.4
  } else if (windCondition < 4){
    windConditionFactor<-0.0
    levellog(logger, 'INFO', "come on, it is a uav not the falcon...")  
    stop("come on, it is a uav not the falcon...")
  }
  
  # log preset picture rate sec/pic
  levellog(logger, 'INFO', paste("original picture rate: ", picRate,"  (sec/pic) "))    
  
  #   # calculate speed & time parameters  
  if (maxSpeed>uavOptimumspeed) {
    maxSpeed<-uavOptimumspeed
    levellog(logger, 'INFO',paste( "MaxSpeed forced to ", uavOptimumspeed," km/h \n"))
    cat("\n MaxSpeed forced to ", uavOptimumspeed," km/h \n")
  }
  # calculate time need to fly the task
  rawTime<-round(((flightLength/1000)/maxSpeed)*60,digit=1)
  
  # calculate the corresponding (raW)  timeintevall for each picture
  picIntervall<-round(rawTime*60/(flightLength/totalTrackdistance),digits = 1)
  levellog(logger, 'INFO', paste("initial speed estimation  : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  while (picIntervall< picRate){
    maxSpeed<-maxSpeed-1
    rawTime<-round(((flightLength/1000)/maxSpeed)*60,digit=1)
    rawTime<-rawTime*windConditionFactor
    picIntervall<-round(rawTime*60/(flightLength/totalTrackdistance),digits = 1)
    levellog(logger, 'INFO', paste("decrease speed to  : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  }
  
  # APPLY battery lifetime loss by windspeed
  maxFlightTime<-maxFlightTime*windConditionFactor
  return(c(rawTime,maxFlightTime,maxSpeed,picIntervall))
}


# assign launching point 
launch2flightalt<- function(p,lns,uavViewDir,launch2startHeading,uavType) {
  launchPos<-c(p$launchLon,p$launchLat)
  if (uavType=="djip3"){lns[length(lns)+1]<-makeUavPoint(launchPos,uavViewDir,group=99,p)}
  if (uavType=="solo"){lns[length(lns)+1]<-makeUavPointMAV(launchPos,uavViewDir,group=99,p)}
  pOld<-launchPos
  pos<-calcNextPos(pOld[1],pOld[2],launch2startHeading,10)
  if (uavType=="djip3"){lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)}
  if (uavType=="solo"){lns[length(lns)+1]<-makeUavPointMAV(pos,uavViewDir,group=99,p)}
  return(lns)
}

################################

long2UTMzone <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}
rad2deg <- function(rad) {(rad * 180) / (pi)}

deg2rad <- function(deg) {(deg * pi) / (180)}

# inserts a row in a dataframe
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}