#' toptree 
#'
#' @description  toptree helps to take close up top down pictures from point objects
#'
#' @note basic idea is to fly in a serie to object positions with respect to the surface model for taking high resolution pics
#' @param projectDir path to the main folder where several projects can be hosted
#' It will overwrite the DEM based estimation if any other value than -9999
#' @param demFn  filname of the corresponding DEM data file
#' @param missionName base string for mission filenames
#' @param followSurface  \code{boolean}  TRUE performs an altitude correction 
#' of the missions flight altitude using additional DEM data. 
#' If no DEM data is provided and \code{followSurface} is TRUE, 
#' SRTM data will be downloaded and used
#' Further explanation at \link{seealso}
#' @param presetFlightTask (DJI only) strongly recommended to use "remote" 
#'        \cr
#'  Options are: 
#' \code{"simple_ortho"} takes one picture/waypoint, 
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and 
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude of the mission. It is 
#'   assumed that the UAV is started at the highest point of the surveyArea 
#'   otherwise you have to defined the position of launching.
#' By default it is set to (\code{= 0.0}). If set to \code{-99} it will be 
#' calculated from the swath width of the pictures. NOTE This makes only sense for 
#' \code{followingTerrain = TRUE} to smooth curves.
#' For \code{flightPlanMode = "waypoint"} camera actions are DISABLED during curve flights.
#' @param rotationdir (DJI only) camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' @param gimbalmode (DJI only) camera control parameter
#' \code{0} deactivates the gimbal control
#' \code{1} activates the gimbale for focussing POIs
#' \code{2} activates the gimbale for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
#' @param gimbalpitchangle (DJI only) vertical angle of camera  \code{+30 deg..-90 deg} 
#' @param actiontype (DJI only) individual actionype settings of the camera c(1,1,...)
#' @param actionparam (DJI only) corresponding parameter for the above individual actiontype c(0,0,...)
#' @param maxSpeed  cruising speed
#' @param followSurfaceRes horizontal step distance for analysing the DEM altitudes
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h
#' @param rcRange range of estimated range of remote control 
#' @param uavType type of uav. currently "djip3" and "solo" are supported
#' @param maxFl maximum duration of a flight in minutes

#' @author
#' Chris Reudenbach
#'
#' @examples
#' fpdata<-toptree(projectDir ="/home/creu/uav/bayerwald",
#' missionName = "filzmoosTree",
#' missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
#' demFn = "~/uav/grossfilz/grosserfilz.tif",
#' windCondition = 2,
#' uavType = "djip3",
#' followSurfaceRes=5,
#' launchPos = c(13.409114897133804,48.92039612988935))
#' 
#' @export toptree 
#'               

toptree<- function(projectDir="~",
                              missionName="autoflightcontrol",
                              missionTrackList=NULL,
                              launchPos=NULL,
                              demFn=NULL,
                              flightAltitude=100,
                              aboveTreeAlt=20,
                              presetFlightTask="remote",
                              maxSpeed=20.0,
                              followSurfaceRes=10,
                              altFilter=1.0,
                              maxFL=10,
                              windCondition=1,
                              rcRange=-9999,
                              launchAltitude=-9999,
                              uavType="djip3") {
  ###  setup environ and params
  cat("setup environ and params...\n")
  # assign flight mission name 
  mission<-paste(missionName, sep=.Platform$file.sep)
  
  workingDir<-missionName
  # create directories if needed
  if(!file.exists(file.path(projectDir, workingDir))){dir.create(file.path(projectDir, workingDir),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"tmp"))){  dir.create(file.path(projectDir, workingDir,"/tmp"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"control"))) { dir.create(file.path(projectDir, workingDir,"control"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir,"data"))){dir.create(file.path(projectDir,"data"),recursive = TRUE)}
  # setting R environ temp folder to the current working directory
  Sys.setenv(TMPDIR=file.path(projectDir, workingDir,"tmp"))
  
  # set R working directory
  setwd(file.path(projectDir, workingDir,"tmp"))
  
  Sys.chmod(list.dirs("../.."), "777")
  
  # create log file
  logger <- log4r::create.logger(logfile = paste0(file.path(projectDir, workingDir,"control/"),strsplit(basename(mission), "\\.")[[1]][1],'.log'))
  log4r::level(logger) <- "INFO"
  log4r::levellog(logger, 'INFO',"                                                           ")
  log4r::levellog(logger, 'INFO',"                                                           ")
  log4r::levellog(logger, 'INFO',"--------------------- START RUN ---------------------------")
  log4r::levellog(logger, 'INFO',paste("Working folder: ",file.path(projectDir, workingDir)))
  

  
  # create misson filename
  csvFn<- paste(file.path(projectDir, workingDir,"control"), paste0(mission,".csv"), sep=.Platform$file.sep)
  
    # import flight area if provided by an external vector file
      file.copy(overwrite = TRUE, from = missionTrackList, to = file.path(projectDir,"data"))
      test<-try(flightList<-readTreeTrack(missionTrackList))
      if (class(test)!="try-error"){
        treeList<-flightList
      }
      else{
         log4r::levellog(logger, 'FATAL', "### can not find/read flight list")        
        stop("### could not read flight list")
      }
      test<-try(readLaunchPos(launchPos))
      if (class(test)!="try-error"){
        launchPos<-test
      }
      else{
        log4r::levellog(logger, 'FATAL', "### can not find/read launchPosition")        
        stop("### could not read launchPosition")
      }
      
  
  #
  p<-list()
  p$launchPos<-launchPos
  p$launchLat<-launchPos@coords[2]
  p$launchLon<-launchPos@coords[1]
  p$missionName<-missionName
  p$missionTrackList<-missionTrackList
  p$demFn<-demFn
  p$flightAltitude<-flightAltitude
  p$presetFlightTask<-presetFlightTask
  p$maxSpeed<-maxSpeed
  p$followSurfaceRes<-followSurfaceRes
  p$maxFL=maxFL
  p$windCondition<-windCondition
  p$rcRange<-rcRange
  p$uavType<-uavType
  p$curvesize<-0
  p$rotationdir<-0
  p$gimbalmode<-0
  p$gimbalpitchangle<--90
  p$launchAltitude<-launchAltitude
  p$aboveTreeAlt<-aboveTreeAlt
  p$altFilter<-altFilter
  p$projectDir<-projectDir
    
  
  ############ 
  
  
  p$task<- getPresetTask("treetop")


  fullTreeList<-makeFlightPath(treeList,p,uavType,task,demFn,logger)

  
}
##########################################################




readTreeTrack<- function(treeTrack){
  tTkDF<-read.csv(treeTrack,sep="\t",header = TRUE)
  sp::coordinates(tTkDF) <- ~x+y
  sp::proj4string(tTkDF) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84 +no_defs")
  tTkDF<-spTransform(tTkDF, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  return(tTkDF)
}

makeFlightPath<- function(treeList,p,uavType,task,demFn,logger){
  
  # calc next coordinate
  lns<-list()
  
  # assign launching point 
  
  #lns[length(lns)+1]<-makeUavPoint(launchPos,uavViewDir,group=99,p)
  fileConn<-file("treepoints.csv")
  for(i in 2:nrow(treeList)-1){
    
    #if (mode =="track"){group<-99}
    #if (uavType=="djip3"){lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group,p)}
    #if (uavType=="solo"){lnsMAV[length(lnsMAV)+1]<-makeUavPointMAV(pos,uavViewDir,group,p)}
    
    #print(treeListDf[i,4])
  
  
  if (uavType=="djip3"){
    forward<-geosphere::bearing(treeList@coords[i,],treeList@coords[i+1,], a=6378137, f=1/298.257223563)
    backward<-geosphere::bearing(treeList@coords[i+1,],treeList@coords[i,], a=6378137, f=1/298.257223563)
    p$task<- getPresetTask("treetop")
    lns[length(lns)+1]<- makeUavPoint(treeList@coords[i,],forward,p,group=99)
    p$task<- getPresetTask("nothing")
    posUp<- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],heading=forward,distance=5)
    lns[length(lns)+1]<- makeUavPoint  (posUp,forward,p,group=1)
    posDown<- calcNextPos(treeList@coords[i+1,][1],treeList@coords[i+1,][2],backward,5)
    lns[length(lns)+1]<- makeUavPoint(posDown,forward,p,group=1)
    writeLines(unlist(lns), fileConn)
  }
  else if (uavType=="solo"){
    forward<-geosphere::bearing(treeList@coords[i,],treeList@coords[i+1,], a=6378137, f=1/298.257223563)
    backward<-geosphere::bearing(treeList@coords[i+1,],treeList@coords[i,], a=6378137, f=1/298.257223563)
    p$task<- getPresetTask("treetop")
    lns[length(lns)+1]<- makeUavPointMAV(treeList@coords[i,],forward,p,group=99)
    p$task<- getPresetTask("nothing")
    posUp<- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],heading=forward,distance=5)
    lns[length(lns)+1]<- makeUavPointMAV  (posUp,forward,p,group=1)
    posDown<- calcNextPos(treeList@coords[i+1,][1],treeList@coords[i+1,][2],backward,5)
    lns[length(lns)+1]<- makeUavPointMAV(posDown,forward,p,group=1)
    writeLines(unlist(lns), fileConn)

  }
  
  }
  close(fileConn)
  if (uavType=="djip3"){
    cat("calculating DEM related stuff\n")
    djiDF<-read.csv("treepoints.csv",sep=",",header = FALSE)
    names(djiDF) <-unlist(strsplit( makeUavPoint(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = " "))
    sp::coordinates(djiDF) <- ~lon+lat
    sp::proj4string(djiDF) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    result<-getAltitudes(demFn ,djiDF,p,followSurfaceRes=5,logger)
    #result<-demCorrection(demFn, djiDF,p,p$altFilter,p$followSurface,p$followSurfaceRes,logger,projectDir)
    #result<-demCorrection(demFn ,djiDF,p,followSurface=followSurface,followSurfaceResfollowSurfaceRes,logger=logger,projectDir=projectDir)
    #    write.csv(djiDF@data,file = paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/","mission",".csv"),quote = FALSE,row.names = FALSE)
    #writeDjiTreeCsv(result[[2]],p$missionName)
    writeDjiTreeCSV(result[[2]],p$missionName,1,94,p,logger,round(result[[4]],digit=0),trackSwitch,result[[3]],result[[6]])
    
    return(result)
    
  } else if (uavType=="solo"){
    cat("calculating DEM related stuff\n")
    df<-read.csv("treepoints.csv",sep="\t",header = FALSE)
    #names(df) <-unlist(strsplit( makeUavPointMAV(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = " "))
    names(df) <-c("a","b","c","d","e","f","g","lat","lon","latitude","longitude","altitude","id","j")
    sp::coordinates(df) <- ~lon+lat
    sp::proj4string(df) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    result<-getAltitudes(demFn ,df,p,followSurfaceRes=5,logger)
    MAVTreeCSV(flightPlanMode="track",trackDistance=10000,logger=logger,p=p,param=result,maxSpeed=p$maxSpeed)
    
    return(result)
    
  }
}

getAltitudes<- function(demFn ,df,p,followSurfaceRes,logger){
  
  if (is.null(demFn)){
     log4r::levellog(logger, 'WARN', "CAUTION!!! no dem file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    stop("CAUTION!!! no dem file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
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
  }
    # brute force projection check    
    if (is.null(dem@crs)) {stop("the DSM is not georeferencend")}
      # resample dem to followTerrainRes and UTM  
      gdalwarp(srcfile = "tmpdem.tif", dstfile = "prjdem.tif", overwrite=TRUE,  tr=c(as.numeric(followSurfaceRes),as.numeric(followSurfaceRes)))
      # deproject it again to latlon
      demll<-gdalwarp(srcfile = "prjdem.tif", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )
 
  # extract all waypoint altitudes
  altitude<-raster::extract(demll,df)
  # get maximum altitude of the task area
  maxAlt<-max(altitude,na.rm = TRUE)
   log4r::levellog(logger, 'INFO', paste("maximum DEM Altitude : ", maxAlt," m"))
  # if no manually provided launch altitude exist get it from DEM
  pos<-as.data.frame(cbind(p$launchPos@bbox[2],p$launchPos@bbox[1]))
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
   log4r::levellog(logger, 'INFO', paste("launching Altitude : ", p$launchAltitude," m"))
  launchAlt<-p$launchAltitude
  # calculate the flight altitude shift due to launching and max altitude
  p$flightAltitude=as.numeric(p$flightAltitude)+(maxAlt-as.numeric(launchAlt))
  p$aboveTreeAlt=as.numeric(p$aboveTreeAlt)+(maxAlt-as.numeric(launchAlt))
  
  rthFlightAlt<-p$flightAltitude
  p$rthAltitude=rthFlightAlt
   log4r::levellog(logger, 'INFO', paste("rthFlightAlt : ", rthFlightAlt," m"))
   rawAltitude<-altitude
   altitude<-altitude+as.numeric(p$flightAltitude)-maxAlt
   df$altitude<-altitude
   

  taltitude<-as.data.frame(rawAltitude+as.numeric(p$aboveTreeAlt)-maxAlt)
  taltitude$id=df@data$id
  names(taltitude)<-c("altitude","id")
  tmp<-df@data
  tmp$altitude[tmp$id == 99 ]<-taltitude$altitude[taltitude$id==99 ]
  df$altitude<-tmp$altitude
  return(c(pos,df,demll,rthFlightAlt,launchAlt,maxAlt,p,retdem))
}

readLaunchPos<- function(fN,extend=FALSE){
  if (class(fN) != "numeric") {
  flightBound<-importsurveyArea(fN)
  launchLon<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,1] 
  launchLat<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,2] 
  }
  else{
    # create SPDF
    # points from scratch
    coords = cbind(fN[1],fN[2])
    launchPos = sp::SpatialPoints(coords)
    launchPos = SpatialPointsDataFrame(coords, as.data.frame("LaunchPosition"))
    # promote data frame to spatial
    sp::proj4string(launchPos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    }
  return(launchPos)
}

# export data to xternal format deals with the splitting of the mission files
writeDjiTreeCsv <-function(df,mission){
  # max numbers of waypoints is 99
  nofiles<-ceiling(nrow(df@data)/96)
  maxPoints<-96
  minPoints<-1
  maxFlightLength <- 15
  


  for (i in 1:nofiles) {
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
    write.csv(df@data[minPoints:maxPoints,1:(ncol(df@data)-2)],file = paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,i,".csv"),quote = FALSE,row.names = FALSE)
      minPoints<-maxPoints
      maxPoints<-maxPoints+96

    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
}

# export data to xternal format deals with the splitting of the mission files
writeDjiTreeCSV <-function(df,mission,nofiles,maxPoints,p,logger,rth,trackSwitch=FALSE,dem,maxAlt){
  minPoints<-1
  if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  # store launchposition and coordinates we need them for the rth calculations
  row1<-df@data[1,1:(ncol(df@data))]

  launchLat<-p$launchLat
  launchLon<-p$launchLon
  
  for (i in 1:nofiles) {
    # take current start position of the split task
    startLat<-df@data[minPoints,1]
    startLon<-df@data[minPoints,2]
    # take current end position of split task
    endLat<-df@data[maxPoints,1]
    endLon<-df@data[maxPoints,2]
    # generate flight lines from lanch to start and launch to end point of splitted task
    yhome <- c(launchLat,endLat)
    xhome <- c(launchLon,endLon)
    ystart <- c(launchLat,startLat)
    xstart <- c(launchLon,startLon)
    start<-SpatialLines(list(Lines(Line(cbind(xstart,ystart)), ID="start")))
    home<-SpatialLines(list(Lines(Line(cbind(xhome,yhome)), ID="home")))
    sp::proj4string(home) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    sp::proj4string(start) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # calculate minimum rth altitude for each line by identifing max altitude
    homeRth<-max(unlist(raster::extract(dem,home)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    startRth<-max(unlist(raster::extract(dem,start)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    
    # calculate rth heading 
    homeheading<-geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    startheading<-geosphere::bearing(c(startLon,startLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    
    
    altitude<-startRth
    latitude<-  launchLat<-p$launchLat
    longitude<-launchLon<-p$launchLon
    heading<-startheading
    # generate ascent waypoint to realize save fly home altitude
    rowStart<-cbind(latitude,longitude,altitude,heading,row1[5:ncol(df@data)])
    
    # calculate rth ascent from last task position
    pos<-calcNextPos(endLon,endLat,homeheading,10)
    
    # generate rth waypoints
    heading<-homeheading
    altitude<-homeRth
    latitude<-pos[2]
    longitude<-pos[1]
    # generate ascent waypoint to realize save fly home altitude
    ascentrow<-cbind(latitude,longitude,altitude,heading,rowStart[5:ncol(df@data)])
    # generate home position with heading and altitude
    homerow<-cbind(rowStart[1:2],altitude,heading,rowStart[5:ncol(df@data)])
    # genrate launch to start waypoint to realize save fly home altitude
    heading<-homeheading
    altitude<-startRth
    startrow<-cbind(rowStart[1:2],altitude,heading,rowStart[5:ncol(df@data)])
    
    # append this three points to each part of the splitted task
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


# write autonoumous flight track to MAV format 
# deals with the splitting of the mission files


