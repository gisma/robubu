#' Tool to generate litchi compatible autonomous flightplans focussing an optimal picture retrieval for DSM/DEM and orthophoto generation 
#' with the phantom 3 UAV
#' 
#' @description  makeFlightPlan creates intermediate flight control files for the dji
#'   phantom x UAVs. It is designed for the  \code{litchi} flight control app. The
#'   reason using litchi is on the one hand that litchi is more straightforward to use 
#'   on the other hand (and much more important) that lichti provides additionally to the 
#'   cloud based mission planer an offline mission planer tool to import a csv formated waypoint
#'   file based to perform autonomous flights. \cr
#'   From convenience reasons some presets and default settings are defined.
#'   
#' @references
#' 
#' 
#' @note 
#' 
#'   To define a flight area you have to provide either 4 Points (or 3 lines). 
#'   You may take more complex vectors like a multi point polygon,
#'   but only the first 4 coordinates x1, x2, x3 and x4 (for the launching position) 
#'   are used in exactly this order. 
#'   If you take a rectangle the 4th corner coordinate will be the launching point!
#'   \cr\cr
#'   The concept is looking like the following sketch.
#'  \preformatted{ 
#'   x2------x3           x2-------x1
#'   | a                 /
#'   |                  /
#'   |   x4            / x4    
#'   |  /             / /
#'   x1/             x3/
#'   }
#'   This coordinates the length of the line and the angle are used to calculate extend and paralells 
#'   of the flightplan according to the flight altitude, overlap etc. Note the flight direction depends on 
#'   the order of the points. If the \code{flightPlanMode} is equal \code{tracks}.
#'   \cr\cr
#'   The result look like this.
#'   
#'  \preformatted{ 
#'  
#'   #--#  #-->             #--#  #
#'   |  |  |               /  /  /
#'   |  |  |              /  /  /
#'   |  |  |             /  /  / 
#'   #  #--#         <--#  #--#

#'   }
#'  If \code{flightPlanMode} is equal \code{waypoints} the result is an equal spatial distribution of waypoints:
#'  \preformatted{ 
#'  
#'   #--#  #-->             #--#  #
#'   |  |  |               /  /  /
#'   #  #  #              #  #  #
#'   |  |  |             /  /  / 
#'   #  #--#         <--#  #--#
#'   
#'   }
#'   
#'  
#'   \code{waypoints} is optimal for autonoumous flights under calm conditions in complex terrain 
#'   because the camara takes a picture at every waypoint\cr
#'   \code{track} is optimal for relatively plain areas and automatically triggered picture capturing
#'   Note: Automatically picture capturing in a time interval works only within the range of the remote control. 
#'   because the the uav needs a trigger signal for taking pictures.
#'   \cr
#'   \cr
#'   The \code{followSurface} switch is used to adapt the fixed flight altitude into a terrain following flight altitude.\cr 
#'   ----------------------------------------------------------------------------------------------------------\cr
#'   NOTE: You have to be aware that the DJI uav is calibrating the altitude at the launch position in the field!
#'   So you need either a correct coordinate altitude or a high resolution DEM to get a good! estimation of the lauch position and altitude. 
#'   You must choose a clearly defined and reliable launching position both in the map and the field. If you fail I made the experience that the aircraft 
#'   probably will hit the terrain...\cr
#'   ----------------------------------------------------------------------------------------------------------\cr\cr
#'  How it works. Let us assume a defined flightaltitude of 50 m. 
#'  According to the launching point altitude the uav will act like the following sketch shows:
#'   
#' \preformatted{ 
#'                      
#'   ............... x_(uav)_x ........... uav started at 30 m altitude results in 
#'                                            a "real" flight altitude of 30m + 50m => 80m
#'   
#'   
#'                   ___60m____
#'                  |          | 
#'          30m _x__|          | 
#'         ____|               |___  
#'     ___|                        |____ 
#
#'  
#'   
#'                  ___60m____
#'       ..........|          |............ uav started at 0 m altitude results in 
#'              ___|          |___          "real" flight altitude of 50m above 0m
#'         ____|                  |  
#'     ___|                       |__x__ 0m
#
#'   }  
#'  To avoid negative impacts from the P3 auto calibration, the launch altitude is used to
#'  correct the flight altitude according to: \cr
#'  maximumAltitude_of_surveyArea + altitude_of_launchposition\cr
#'  So the adapted flight altitude is always seen as the flight altitude above the highest terrain altitude:
#'   \preformatted{
#'                  
#'  ...................................... real altitude of uav 110 m
#'          
#'
#'                  ___60m____ 
#'                 |          |
#'          30m _x_|          |___
#'         ____|                  |  
#'     ___|                       |______ 
#
#'   }  
#'  To get a fixed scale flight the launch altitude is used to correct the flight altitude according to   maximumAltitude of surveyArea + altitude of launchposition. With the setting auf terrainfoollowing = true tis is calculated for each waypoint.  . So the adapted flight altitude looks like:
#'   \preformatted{
#'                  
#'                  ..........
#'                 |          |
#'             ....|          |.... 
#'        ....|     ___60m____    |    
#'   ....|         |          |   |....... real altitude of uav 50m
#'          30m _x_|          |___
#'         ____|                  |  
#'     ___|                       |___x___ 0m
#
#'   }  

#' @param surveyArea  you may provide either the coordinates by 
#' c(lon1,lat1,lon2,lat2,lon3,lat3,launchLat,launchLon) or
#' an OGR compatible file (preferably geoJSON or KML) with
#' at least 4 coordinates that describe the flight area. 
#' The fourth coordinate is the launch position.
#'  You will find further explanation under the \link{seealso}. 
#' @param launchAltitude absolute altitude of launching position. 
#' It will overwrite the DEM based estimation if any other value than -9999
#' @param demFn  filname of the corresponding DEM data file
#' @param missionName base string for mission filenames
#' @param followSurface  \code{boolean}  TRUE performs an altitude correction 
#' of the missions flight altitude using additional DEM data. 
#' If no DEM data is provided and \code{followSurface} is TRUE, 
#' SRTM data will be downloaded and used
#' Further explanation at \link{seealso}
#' @param altFilter if \code{followingTerrain} is equal \code{TRUE} then 
#' \code{altFilter} is the treshold value of accepted altitude difference bewteen two waypoints in meter.
#'  If this value is not exceeded the waypoint is omitted due to the fact that only 99 waypoints per mission are allowed.
#' @param flightPlanMode type of flightplan. Available are: \code{"waypoints"}, 
#'   \code{"track"}, \code{"manual"}.
#' @param presetFlightTask set the camera action at each waypoint.
#'  Options are: 
#' \code{"simple_ortho"} takes one picture/waypoint, 
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and 
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude of the mission. It is 
#'   assumed that the UAV is started at the highest point of the surveyArea 
#'   otherwise you have to defined the position of launching.
#' @param overlap overlapping of the pictures in percent (1.0 = 100)
#' @param uavViewDir viewing directon of camera default is \code{0}
#' @param curvesize control parameter for the curve angle at waypoints. 
#' By default it is set to (\code{= 0.0}). If set to \code{-99} it will be 
#' calculated from the swath width of the pictures. NOTE This makes only sense for 
#' \code{followingTerrain = TRUE} to smooth curves.
#' For \code{flightPlanMode = "waypoint"} camera actions are DISABLED during curve flights.
#' @param rotationdir camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' @param gimbalmode camera control parameter 
#' \code{0} deactivates the gimbal control
#' \code{1} activates the gimbale for focussing POIs
#' \code{2} activates the gimbale for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
#' @param gimbalpitchangle vertical angle of camera  \code{+30°..-90°}
#' @param actiontype individual actionype settings of the camera c(1,1,...)
#' @param actionparam  corresponding parameter for the above individual actiontype c(0,0,...)
#' @param maxSpeed  cruising speed
#' @param heatMap switch for calculating the overlapping factor on a raster map
#' @param picFootprint switch for calculating the footprint at all waypoints
#' @param followSurfaceRes horizontal step distance for analysing the DEM altitudes
#' @param picRate fastest stable interval (s) for shooting pictures 
#' @param batteryTime estimated life time of battery 
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h
#' @param startLitchi if TRUE it starts an offline Litchi website for converting the data (preliminary workaround)

#' 
#' @author
#' Chris Reudenbach
#'
#' @examples
#' 
#' library(log4r)
#' 
#' # Please keep in mind that there is a bunch of interdependent parameter settings.
#' # Hence here are just some typical examples. 
#' 
#' # (1) simple flight, 50 meters above ground 
#' # assuming a flat topography,
#' # generating a heatmap to estimate overlapping
#' fp<-makeFlightPlan(surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734))
#'
#' # legend  
#' # red circle      the planned launching position of the uav. 
#' # blue circles    the waypoint position
#' # blue rectangles the corresponding field of view (fov)at the ground
#' # raster[[fp2]]   the digitial elevation model (DEM)
#' # raster[[fp5]]   a heatmap abundance of pictures/pixel
#'
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE)
#' 
#' 
#' # (2) adapting viewing angle of the camera, adding coverage map, switching to track mode
#' fp<-makeFlightPlan(surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'                    uavViewDir=30,
#'                    flightPlanMode="track",
#'                    heatMap=TRUE)
#'                   
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE
#' 
#' # (3) increas of the overlap
#' fp<-makeFlightPlan(surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'                    overlap=0.8,
#'                    uavViewDir=30,
#'                    flightPlanMode="track",
#'                    heatMap=TRUE)
#'                   
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE)
#' 
#' 
#' # (4) terrain following flightplan, add DEM
#' fp<-makeFlightPlan(surveyArea = c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734), 
#'                    followSurface = TRUE,
#'                    demFn = "inst/data/mrbiko.tif",
#'                    )
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,)+mapview(fp[[3]],color="red",cex=5)
#' 
#' # (5) same as (4) but with lower flight altitude TAKE CARE!
#' fp<-makeFlightPlan(surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734), 
#'                    followSurface = TRUE, 
#'                    flightAltitude = 25, 
#'                    demFn = "inst/data/mrbiko.tif")
#' 
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,)+mapview(fp[[3]],color="red",cex=5)
#'
#'  
#' # (6) use of external vector data to define the surveyArea...
#' # digitize flight area using leafDraw()
#' leafDraw(preset="uav")
#' 
#' ## assuming resulting file is names "uav.json"
#'fp<-makeFlightPlan(surveyArea = "~/uav.json",
#'                   demFn = "inst/data/mrbiko.tif")
#' 
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)
#' 


#' @export makeFlightPlan
#' @export getPresetTask
#' @aliases  makeFlightPlan
#'               

makeFlightPlan<- function(rootDir="~",
                          workingDir="uav",
                          missionName="litchi_autoflightcontrol",
                          surveyArea=NULL,
                          launchAltitude=-9999,
                          followSurface=FALSE,
                          demFn=NULL,
                          altFilter=1.0,
                          flightPlanMode="track",
                          flightAltitude=100,
                          presetFlightTask="remote",
                          curvesize=0,
                          rotationdir=0,
                          gimbalmode=0,
                          gimbalpitchangle=-90,
                          overlap=0.7,
                          uavViewDir=0,
                          maxSpeed=45.0,
                          picRate=2,
                          heatMap=FALSE,
                          picFootprint=TRUE,
                          followSurfaceRes=-9999,
                          maxFL=10,
                          batteryTime=12,
                          windCondition=1,
                          startLitchi=FALSE,
                          actiontype=NULL,
                          actionparam=NULL)
{
  # assign flight mission name 
  mission<-paste(paste0(missionName,"_",flightAltitude), sep=.Platform$file.sep)
  
  # create log file
  logger <- create.logger(logfile = paste0(strsplit(basename(mission), "\\.")[[1]][1],'.log'))
  level(logger) <- "INFO"
  levellog(logger, 'INFO',"--------------------- START RUN ---------------------------")
  
  # create directory
  if (rootDir=="~"){
    levellog(logger, 'INFO',"NOTE: You are using the default directory settings!")
    cat("NOTE: You are using the default directory settings!")
    }
  
  if (!file.exists(file.path(rootDir, workingDir))){
    dir.create(file.path(rootDir, workingDir),recursive = TRUE)
    levellog(logger, 'INFO',paste(file.path(rootDir, workingDir)," created"))
    cat(paste(file.path(rootDir, workingDir),"created"))
    }

  # setting System Temporary folder to the current working directory
  Sys.setenv(TMPDIR=file.path(rootDir, workingDir))
  
  # (R) set R working directory
  setwd(file.path(rootDir, workingDir))
  
  csvFn<- paste(file.path(rootDir, workingDir), paste0(mission,".csv"), sep=.Platform$file.sep)
  
  

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
      test<-try(flightBound<-readExternalFlightBoundary(surveyArea))
      if (class(test)!="try-error"){
        surveyArea<-flightBound 
      }else{
        levellog(logger, 'FATAL', "### can not find/read input file")        
        stop("### could not read input file")
      }
    }
  }

  # push params
  flightParams=c(flightPlanMode=flightPlanMode,
                 launchAltitude=launchAltitude,
                 flightAltitude=flightAltitude,
                 presetFlightTask=presetFlightTask,
                 curvesize=curvesize,
                 rotationdir=rotationdir,
                 gimbalmode=gimbalmode,
                 gimbalpitchangle=gimbalpitchangle,
                 overlap=overlap,
                 uavViewDir=uavViewDir,
                 actiontype=actiontype,
                 actionparam=actionparam)
  
  # define all flight params
  p<-makeFlightParam(surveyArea,flightParams,followSurface)
  
  
  # assign flightmode 
  mode<-as.character(p$flightPlanMode)


  # assign DEM data file name
  demFile<-demFn
  # assign altitude (meters) threshold from one waypoint to the next (for terrainTrack)
  altdiff<-altFilter
  # assign flight Altitude
  flightAltitude<- as.numeric(flightParams["flightAltitude"])
  #calc & assign overlapping factor as a function of flightAltitude
  fliAltRatio<-1-as.numeric(flightParams["overlap"])
  # calc & assign picture footprint
  trackDistance<-(fliAltRatio*(1.71*flightAltitude))
  crossDistance<-trackDistance
  # assign curvesize
  if (p$curvesize=="-99") {
    p$curvesize<-crossDistance*0.4
  }
  # construct survey area extent
  taskArea<-taskarea(p,csvFn)
  # calculate area
  taskAreaUTM<-spTransform(taskArea, CRS(paste("+proj=utm +zone=",long2UTMzone(p$lon1)," ellps=WGS84",sep='')))
  surveyAreaUTM<-rgeos::gArea(taskAreaUTM)
  # calculate and assign  heading from launch to startpoint
  l2sheading<-geosphere::bearing(c(p$launchLon,p$launchLat),c(p$lon1,p$lat1), a=6378137, f=1/298.257223563)  
  # calculate and assign  heading base flight track W-E
  updir<-geosphere::bearing(c(p$lon1,p$lat1),c(p$lon2,p$lat2), a=6378137, f=1/298.257223563)
  # calculate and assign  heading base flight track E-W
  downdir<-geosphere::bearing(c(p$lon2,p$lat2),c(p$lon1,p$lat1), a=6378137, f=1/298.257223563)
  # calculate and assign  heading base flight track trackline to trackline
  crossdir<-geosphere::bearing(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  # calculate and assign  distance of the base flight track
  len<-geosphere::distGeo(c(p$lon1,p$lat1),c(p$lon2,p$lat2))
  # calculate and assign distance of the cross base flight track
  crosslen<-distGeo(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  if (followSurfaceRes==-9999){followSurfaceRes<-trackDistance}
  # calculate and assign  number of pictures/waypoints along one track
  if (followSurface){
    multiply<-floor(len/followSurfaceRes)
    trackDistance<-followSurfaceRes
    crossDistance<-followSurfaceRes
  } else{
    multiply<-floor(len/trackDistance)  
  }
  # calculate and assign  number of tracklines
  tracks<-floor(crosslen/crossDistance)
  #set initial heading
  heading <-updir
  # set universal view direction of the uav
  if (!is.null(flightParams["uavViewDir"])){uavViewDir<-updir-as.numeric(flightParams["uavViewDir"])}
  else {uavViewDir<-as.numeric(flightParams["uavViewDir"])}
  # init of control id #1 common  #99 turnpoints of single tracks
  group<-1
  # set cumulative flgihtlength to zero
  flightLength<-0
  # define df
  df<-data.frame()  
  dfQGCWPL110<-data.frame()  
  # define output line var
  lns<-list()
  lnsQGCWPL110<-list()
  # assign launching point 
  launchPos<-c(p$launchLon,p$launchLat)
  lns[length(lns)+1]<-makeUavPoint(launchPos,uavViewDir,group=99,p)
  lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(launchPos,uavViewDir,group=99,p)
  pOld<-launchPos
  pos<-calcNextPos(pOld[1],pOld[2],l2sheading,10)
  lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
  lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group=99,p)
  # assign starting point
  pos<-c(p$lon1,p$lat1)
  # calculates the footprint of the first position and returns a SpatialPolygonsDataFrame 
  if (picFootprint) {camera<-cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,0,0)}
  # creates the export control parameter set of the first position
  lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
  lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group=99,p)
  # push pos to old pos
  pOld<-pos
  
  # set counter and params for mode = "track" mode
  if (mode == "track") {
    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
    lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group=99,p)
    trackDistance <- len
    multiply<-1
  } 
  # set counter and params for mode = "waypoints"
  else if (mode == "waypoints") {
    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
    lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group=99,p)}
  # set counter and params for mode = "terrainTrack"
  else if (mode == "terrainTrack") {
    # calculate the real starting point
    ###    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
    # calculate the real starting point
    ###    pos<-calcNextPos(pos[1],pos[2],heading,trackDistance)
    ###    if (picFootprint) {camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))}
    #df <- rbind(df,data.frame(lat=p[2], lon=p[1], latitude=p[2], longitude=p[1],altitude=altitude,heading=uavViewDir,curvesize=curvesize,rotationdir=rotationdir,gimbalmode=gimbalmode,gimbalpitchangle=gimbalpitchangle,actiontype1=actiontype1,actionparam1=actionparam1,actiontype2=actiontype2,actionparam2=actionparam2,actiontype3=actiontype3,actionparam3=actionparam3,actiontype4=actiontype4,actionparam4=actionparam4,actiontype5=actiontype5,actionparam5=actionparam5,actiontype6=actiontype6,actionparam6=actionparam6,actiontype7=actiontype7,actionparam7=actionparam7,actiontype8=actiontype8,actionparam8=actionparam8,id=group))
    ###    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
    group=99
  }
  # then do for the rest  forward and backward
  for (j in seq(1:tracks)){
    for (i in seq(1:multiply)) {
      if (mode=="waypoints" || mode == "terrainTrack") {
        if (i<2 | i > multiply-1) {group<-99}
        else      {group<-1}
      }
      else {i<-2}
      
      # calc next coordinate
      pos<-calcNextPos(pOld[1],pOld[2],heading,trackDistance)
      if (picFootprint) {camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))}
      pOld<-pos
      flightLength<-flightLength+trackDistance
      if (mode =="track"){group<-99}
      lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group,p)
      lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group,p)
    } 
    
    if ((j%%2 != 0) ){
      
      pos<-calcNextPos(pOld[1],pOld[2],crossdir,crossDistance)
      if (picFootprint) {camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))}
      pOld<-pos
      flightLength<-flightLength+crossDistance
      lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group<-99,p)
      lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group<-99,p)
      heading<-downdir
    } 
    
    else if ((j%%2 == 0) ) {
      
      pos<-calcNextPos(pOld[1],pOld[2], crossdir, crossDistance)
      if (picFootprint) {camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))}
      pOld<-pos
      flightLength<-flightLength+crossDistance
      lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group<-99,p)
      lnsQGCWPL110[length(lns)+1]<-makeUavPointQGCWPL110(pos,uavViewDir,group-99,p)
      heading<-updir
    }
  }
  
  fileConn<-file("output2.txt")
  fileConn2<-file("output3.txt")
  writeLines(unlist(lns), fileConn)
  writeLines(unlist(lnsQGCWPL110), fileConn2)
  close(fileConn)
  close(fileConn2)
  df<-read.csv("output2.txt",sep=",",header = FALSE)
  dfQGCWPL110<-read.csv("output3.txt",sep="\t",header = FALSE)
  names(df) <-unlist(strsplit( makeUavPoint(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = " "))
  #names(dfQGCWPL110) <-unlist(strsplit( makeUavPointQGCWPL110(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = "\n"))
  names(dfQGCWPL110) <-c("a","b","c","d","e","f","g","lat","lon","latitude","longitude","altitude","id","j")
  # make sp
  sp::coordinates(df) <- ~lon+lat
  sp::proj4string(df) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  sp::coordinates(dfQGCWPL110) <- ~lon+lat
  sp::proj4string(dfQGCWPL110) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # altitude correction
  
  result<-demCorrection(demFile, df,p,altdiff,followSurface,followSurfaceRes,logger)
  resultQGCWPL110<-demCorrection(demFile, dfQGCWPL110,p,altdiff,followSurface,followSurfaceRes,logger)
  
  
  # wind lookup
  
  if (windCondition==1){
    windConditionFactor<-1
  } else if (windCondition==2){
    windConditionFactor<-0.8
  } else if (windCondition==3){
    windConditionFactor<-0.6
  } else if (windCondition==4){
    windConditionFactor<-0.4
  } else if (windCondition==5){
    windConditionFactor<-0.1
  } else {
    windConditionFactor<-0.0
    levellog(logger, 'INFO', "come on it is a uav not the falcon...")    
  }
  
  levellog(logger, 'INFO', paste("original picture rate: ", picRate,"  (pics/sec) "))    
  #   # calculate speed & time parameters  
  if (maxSpeed>50.0) {
    maxSpeed<-50.0
    levellog(logger, 'INFO', "MaxSpeed forced to 50 km/h \n")
    cat("\n MaxSpeed forced to 50 km/h \n ")
  }
  
  rawTime<-round(((flightLength/1000)/maxSpeed)*60,digit=1)
  #rawTime<-rawTime/windConditionFactor
  picIntervall<-round(rawTime*60/(flightLength/trackDistance),digits = 1)
  levellog(logger, 'INFO', paste("initial speed estimation  : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  while (picIntervall< picRate){
    maxSpeed<-maxSpeed-1
    rawTime<-round(((flightLength/1000)/maxSpeed)*60,digit=1)
    rawTime<-rawTime*windConditionFactor
    picIntervall<-round(rawTime*60/(flightLength/trackDistance),digits = 1)
    levellog(logger, 'INFO', paste("decrease speed to  : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  }
  batteryTime<-batteryTime*windConditionFactor
  
  if (heatMap){
    fovH<-fovHeatmap(camera,result[[2]])
  } else
  {
    fovH <-NULL
  }
  
  # write csv
  writeDroneCSV(result[[1]],mission,rawTime,mode,trackDistance,batteryTime,logger,p,maxFL,len,multiply,tracks)
  writeDroneCSVQGCWPL110(resultQGCWPL110[[1]],mission,rawTime,mode,trackDistance,batteryTime,logger,p,maxFL,len,multiply,tracks,resultQGCWPL110,maxSpeed*3.6)
                           
  # write log file status and params 
  levellog(logger, 'INFO', "---------- use the following mission params! --------------")
  levellog(logger, 'INFO', paste("set speed to               : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  levellog(logger, 'INFO', paste("corresponding  picture rate: ", picIntervall,"  (pics/sec) "))
  levellog(logger, 'INFO', paste("calculated mission time    : ",rawTime,      "  (min)      "))   
  levellog(logger, 'INFO', "----------------------------------------------------")              
  levellog(logger, 'INFO', paste("missionname     : ",mission))
  levellog(logger, 'INFO', paste("DEM filename    : ",demFn))
  levellog(logger, 'INFO', paste("surveyArea      : ",surveyAreaUTM))
  levellog(logger, 'INFO', paste("launchAltitude  : ",launchAltitude))
  levellog(logger, 'INFO', paste("followSurface   : ",followSurface))
  levellog(logger, 'INFO', paste("altfilter       : ",altFilter))
  levellog(logger, 'INFO', paste("flightPlanMode  : ",flightPlanMode))
  levellog(logger, 'INFO', paste("flightAltitude  : ",flightAltitude))
  levellog(logger, 'INFO', paste("presetFlightTask: ",presetFlightTask))
  levellog(logger, 'INFO', paste("curvesize       : ",curvesize))
  levellog(logger, 'INFO', paste("rotationdir     : ",rotationdir))
  levellog(logger, 'INFO', paste("gimbalmode      : ",gimbalmode))
  levellog(logger, 'INFO', paste("gimbalpitchangle: ",gimbalpitchangle))
  levellog(logger, 'INFO', paste("overlap         : ",overlap))
  levellog(logger, 'INFO', paste("uavViewDir      : ",uavViewDir))
  levellog(logger, 'INFO', paste("picFootprint    : ",picFootprint))
  levellog(logger, 'INFO', paste("followSurfaceRes: ",followSurfaceRes))
  levellog(logger, 'INFO', paste("surveyAreaCoords: ",surveyArea))
  levellog(logger, 'INFO', paste("windCondition   : ",windCondition))
  levellog(logger, 'INFO', " ")    
  levellog(logger, 'INFO', "---------- use the following mission params! --------------")
  levellog(logger, 'INFO', paste("speed                      : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  levellog(logger, 'INFO', paste("corresponding  picture rate: ", picIntervall,"  (pics/sec) "))
  levellog(logger, 'INFO', paste("calculated mission time    : ",rawTime,      "  (min)      "))   
  levellog(logger, 'INFO', paste("estimated battery liftime  : ",batteryTime,      "  (min)      "))   
  levellog(logger, 'INFO', paste("Area covered               : ",surveyAreaUTM/10000,      "  (ha)"))   
  levellog(logger, 'INFO', " ")    
  levellog(logger, 'INFO',"--------------------- END RUN ---------------------------")  
  
  # return params for visualisation and main results for overview
  
  if (startLitchi) {
    openLitchi()
    cat("--- END ",mission," Litchi ---")  
  }
  if((flightPlanMode=='track' | flightPlanMode=='terrainTrack') & rawTime>batteryTime)
  {note<- "flighttime > battery lifetime! control files have been splitted. Have Fun..."}
  else if(flightPlanMode=='waypoints')
  {note<-"control files are splitted after max 98 waypoints (litchi control file restricted number)"}
  else {note<-" Have Fun "}
  
  return(c(cat(" wrote ", csvFn, " file(s)...\n",
               "\n +  mission areav                 : ",surveyAreaUTM/1000000,      "  (km**2)     +",
               "\n ",
               "\n ---- set the following mission params! -------------------",
               "\n +  set RTH flight altitude to    : ", round(result[[4]],digit=0),    "        (m)         + ",
               "\n +  set mission speed to a min of : ", round(maxSpeed,digit=1),    "         (km/h)      + ",
               "\n +  set pic rate to a min of      : ", picIntervall,"        (pics/sec)  + ",
               "\n ",
               "\n ---- estimated battery/mission time and area -------------",
               "\n +  max terrain Altitude          : ",round(result[[6]],digits = 0),      "        (m)         +",   
               "\n +  launching Altitude            : ",round(result[[5]],digits = 0),      "        (m)         +",   
               "\n +  calculated mission time       : ",rawTime,      "       (min)       +",
               "\n +  estimated battery lifetime    : ",batteryTime,      "        (min)       +",                
                  
               "\n ----------------------------------------------------------",
               "\n ",
               "\n NOTE:",as.character(note),"",
                "\n "),    
            result[[1]],
           result[[2]],
           result[[3]],
           camera,
           fovH,
           taskArea))

  

}

##################################################
##################################################

demCorrection<- function(demFile ,df,p,altdiff,followSurface,followSurfaceRes,logger){
  
  if (is.null(demFile)){
    levellog(logger, 'WARN', "CAUTION!!! no dem file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    cat("CAUTION!!! no dem file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    # download corresponding srtm data
    dem<-robubu::getGeoData(name="SRTM",xtent = extent(p$lon1,p$lon3,p$lat1,p$lat3), zone = 3.0,merge = TRUE)
    dem<- raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
    # extract the altitudes
    df$Altitude<- raster::extract(dem,df)
  } else {
    # read local dem file
    if (class(demFile)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
      dem<-demFile
    } else{
      dem<-raster::raster(demFile)
    }
    # brute force deproject file    
    llcheck1<-strsplit(as.character(dem@crs), " ")[[1]][1]
    llcheck2<-strsplit(as.character(dem@crs), " ")[[1]][2]
    llcheck3<-strsplit(as.character(dem@crs), " ")[[1]][3]
    if (llcheck3!="+proj=longlat" & llcheck2 != "+datum=WGS84" & llcheck1!="+no_defs") {
    dem<-raster::projectRaster(dem,crs = CRS("+proj=longlat +datum=WGS84 +no_defs"),method = "bilinear")
    }
    # crop it for speeding up
    dem<-raster::crop(dem,extent(min(p$lon1,p$lon3,p$lon2)-0.009,max(p$lon1,p$lon2,p$lon3)+0.009,min(p$lat1,p$lat2,p$lat3)-0.007,max(p$lat1,p$lat2,p$lat3)+0.007))
  }
  # resample the DEM to followSurfaceRes
  # project it to UTM because it is easier to recalculate resolution 
  demutm<-raster::projectRaster(dem,crs = CRS(paste0("+proj=utm +zone=",long2UTMzone(p$lon1))),method = "bilinear")
  # extract the ratio of height width
  fakmax<-max(res(demutm)[1]/followSurfaceRes,abs(res(demutm)[2]/followSurfaceRes))
  fakmin<-min(res(demutm)[1]/followSurfaceRes,abs(res(demutm)[2]/followSurfaceRes))
  # to get equally sized pixel apply factor vice versa
  if (nrow(demutm)<=ncol(demutm)){
    tmpdem <- raster::raster(nrow=nrow(demutm)*fakmax,ncol=ncol(demutm)*fakmin)  
  } else {
    tmpdem <- raster::raster(nrow=nrow(demutm)*fakmin,ncol=ncol(demutm)*fakmax)    
  }
  # add real crs and extent
  tmpdem@crs <-demutm@crs
  tmpdem@extent<-demutm@extent
  # resamle it 
  tmpdem<-raster::resample(demutm,tmpdem,method='ngb')
  
  #FineResampRaster <- disaggregate(tmpdem,fact=1.5,fun=max)
  # we need the dem in latlon
  demll<-raster::projectRaster(tmpdem,crs = CRS("+proj=longlat +datum=WGS84 +no_defs"),method = "bilinear")
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
    pos$altitude<-raster::extract(demll,pos)  
    p$launchAltitude<-pos$altitude
    # otherwise take it from the parameter set
  } else 
  {
    pos$altitude<-as.numeric(p$launchAltitude)
  }
  levellog(logger, 'INFO', paste("launching Altitude : ", pos$altitude," m"))
  launchAlt<-pos$altitude
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
      fDF<-sDF[sDF$id=="99" | sDF$dif > altdiff , ]
      sDF<- sDF[-c(ncol(sDF),ncol(sDF)-1) ]
      fDF$lon<-fDF$longitude
      fDF$lat<-fDF$latitude
      sp::coordinates(fDF) <- ~lon+lat
      sp::proj4string(fDF) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
      df<-fDF
    }
  }
  return(c(df,demll,pos,rthFlightAlt,launchAlt,maxAlt,p))
}

# export data to xternal format deals with the splitting of the mission files
writeDroneCSV <-function(df,mission,rawTime,flightPlanMode,trackDistance,batteryTime,logger,p,maxFL,len,multiply,tracks){
  # max numbers of waypoints is 99
  nofiles<-ceiling(nrow(df@data)/98)
  maxPoints<-98
  minPoints<-1
  maxFlightLength <- maxFL
  
  #dif<-abs(as.data.frame(diff(as.matrix(sDF$altitude))))
  #mat <- distm(list1[,c('longitude','latitude')], list2[,c('longitude','latitude')], fun=distVincentyEllipsoid)
  #for (j in seq(1:nrow(df)))
  #accuLen<-distGeo(c(df$latitudelon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  #}
  
  if ((flightPlanMode =="track" | flightPlanMode =="terrainTrack")){
    if (nofiles<ceiling(rawTime/batteryTime)){
    nofiles<- ceiling(rawTime/batteryTime)
    maxPoints<-ceiling(nrow(df@data)/nofiles)
    mp<-maxPoints
    minPoints<-1
    }
  }  
  for (i in 1:nofiles) {
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
    write.csv(df@data[minPoints:maxPoints,1:(ncol(df@data)-2)],file = paste0(mission,i,".csv"),quote = FALSE,row.names = FALSE)
    levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
    if (flightPlanMode =="track" & rawTime > batteryTime) {
      minPoints<-maxPoints
      maxPoints<-maxPoints+mp} 
    else{
      minPoints<-maxPoints
      maxPoints<-maxPoints+98
    }
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
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


# calculate a new position from given lat lon
calcNextPos<- function(lon,lat,heading,distance){
  p<-geosphere::destPoint(c(lon,lat), heading, distance)
  return(c(p[1],p[2]))
}


# create and recalculates all arguments for a drone waypoint
makeFlightParam<- function(surveyArea,flightParams,followSurface){
  # retrieve and recalculate the arguments to provide the flight paramaer for litchi
  validPreset<-c("multi_ortho","simple_ortho","simple_pano","remote")
  validFlightPlan<-c("waypoints","track","manual")
  stopifnot(flightParams["presetFlightTask"] %in% validPreset)
  stopifnot(flightParams["flightPlanMode"] %in% validFlightPlan)
  if (followSurface == TRUE){
    flightParams["flightPlanMode"] = "terrainTrack"
  }
  p<-list()
  
  # user controlled camera action at wp
  if (flightParams["flightPlanMode"] =="waypoints" | flightParams["flightPlanMode"] =="manual" ){
    if (length(flightParams)>9) {
      task<-makeTaskParamList(flightParams[9:length(flightParams)])
    }
    # preset camera action at waypoints 
    else {
      task<-getPresetTask(flightParams["presetFlightTask"])  
    }
  } 
  # no camera action at waypoint
  else if (flightParams["flightPlanMode"] =="terrainTrack" | 
           flightParams["flightPlanMode"] =="track"  |
           (flightParams["flightPlanMode"] =="waypoints" & flightParams["presetFlightTask"] =="remote"))
  {
    task<- makeTaskParamList(c(actiontype=c(-1),actionparam=c(0)))
  }
  
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
getPresetTask<- function (param=NULL){
  #' shows existing camera action presets 
  #' @description 
  #' NOTE: only for flightPlanMode = "waypoint")
  # preset waypoints & orthophoto
  if (is.null(param))
  {
    return (cat('param == "multi_ortho"\n actiontype=c(1,4,5,1,5,1)\n actionparam=c(0,180,-60,0,-90,0)\n
param == "simple_ortho"\n actiontype=c(5,1)\n actionparam=c(-90,0)\n 
param == "simple_pano"\n actiontype=c(4,1,4,1,4,1,4,1,4,1,4,1,4,1,-1)\n actionparam=c(-180,0,-128,0,-76,0,-24,0,28,0,80,0,132,0,0)\n')  
    )        
  }
  
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
  return(task)
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

# calculates the flight area footprint 
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

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# obsolet function old csv format
makeCsvLine<- function(pos,uavViewDir,group,p){
  action<-""
  for (i in seq(1:length(p$task[,1]))){ 
    action<-paste0(action,p$task[i,]$actionNames[1],"=",p$task[i,]$x[1]," ")
  }
  tmp <-    paste0(" lon=",pos[1], " lat=",pos[2], " latitude=",pos[2], " longitude=",pos[1],
                   " altitude=",as.character(p$flightAltitude),
                   " heading=",as.character(uavViewDir),
                   " curvesize=",as.character(p$curvesize),
                   " rotationdir=",as.character(p$rotationdir),
                   " gimbalmode=",as.character(p$gimbalmode),
                   " gimbalpitchangle=",as.character(p$gimbalpitchangle),
                   action,
                   " id=",group)
}
long2UTMzone <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
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

#' Colourise text for display in the terminal.
#' https://github.com/hadley/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r
#'
#' If R is not currently running in a system that supports terminal colours
#' the text will be returned unchanged.
#'
#' Allowed colours are: black, blue, brown, cyan, dark gray, green, light
#' blue, light cyan, light gray, light green, light purple, light red,
#' purple, red, white, yellow
#'
#' @param text character vector
#' @param fg foreground colour, defaults to white
#' @param bg background colour, defaults to transparent
#' @export
#' @examples
#' print(colourise("Red", "red"))
#' cat(colourise("Red", "red"), "\n")
#' cat(colourise("White on red", "white", "red"), "\n")
colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")
  
  if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }
  
  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }
  
  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) {
    col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  }
  
  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

.fg_colours <- c(
  "black" = "0;30",
  "blue" = "0;34",
  "green" = "0;32",
  "cyan" = "0;36",
  "red" = "0;31",
  "purple" = "0;35",
  "brown" = "0;33",
  "light gray" = "0;37",
  "dark gray" = "1;30",
  "light blue" = "1;34",
  "light green" = "1;32",
  "light cyan" = "1;36",
  "light red" = "1;31",
  "light purple" = "1;35",
  "yellow" = "1;33",
  "white" = "1;37"
)

.bg_colours <- c(
  "black" = "40",
  "red" = "41",
  "green" = "42",
  "brown" = "43",
  "blue" = "44",
  "purple" = "45",
  "cyan" = "46",
  "light gray" = "47"
)

rcmd_running <- function() {
  nchar(Sys.getenv('R_TESTS')) != 0
}



# export data to xternal format deals with the splitting of the mission files

writeDroneCSVQGCWPL110 <-function(df,mission,rawTime,flightPlanMode,trackDistance,batteryTime,logger,p,maxFL,len,multiply,tracks,param,speed=180){
  nofiles<-1
  # max numbers of waypoints is 99
  maxFlightLength <- maxFL
  
  if ((flightPlanMode =="track" | flightPlanMode =="terrainTrack")){
    nofiles<- ceiling(rawTime/batteryTime)
    maxPoints<-ceiling(nrow(df@data)/(rawTime/batteryTime))
    mp<-maxPoints
    minPoints<-1
    
  }

  
  
  for (i in 1:nofiles) {

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
    lnsnew[2,1] <-                 paste0("0",sep,"1",sep,"0",sep,"16",sep,"0",sep,"0",sep,"0",sep,"0",sep,p$launchLat,sep,p$launchLon,sep,as.character(param$launchAltitude),sep,"1")
    # CREATE takeoff
    lnsnew[3,1] <-                 paste0("1",sep,"0",sep,"3",sep,"22",sep,"200.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,as.character(param$flightAltitude),sep,"1")
    #set mission speed
    lnsnew[4,1] <-                 paste0("2",sep,"0",sep,"3",sep,"178",sep,"0.0",sep,speed,sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    
    for (j in 1:length(lns[,1])) {
      lnsnew[j+4,1]<-paste0(as.character(j+2),"\t",lns[j,])
    }
    
    #set rth altitude
    lnsnew[length(lnsnew[,1])+1,1]<-  paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"30",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,param$rthAltitude,sep,"1")
    #set max return speed
    lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"178",sep,"0.0",sep,"250",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    # trigger rth event
    lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"20",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    
    
    write.table(lnsnew, paste0(mission,i,"_solo.waypoints"), sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE,na = "")
    
    
    
    
    levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
    #if (flightPlanMode =="track" & rawTime > batteryTime) {
    #  minPoints<-maxPoints
    #  maxPoints<-maxPoints+mp} 
    #else{
      minPoints<-maxPoints
      maxPoints<-maxPoints+mp
    #}
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
  
}


# create the full argument list for one waypoint
makeUavPointQGCWPL110<- function(pos,uavViewDir,group,p,header=FALSE,sep="\t",speed="180"){
  # create the value lines
  if (!header){
    # CREATE NORMAL WAYPOINT
    tmp <-    paste0("0",sep,"3",sep,"16",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,pos[2],sep,pos[1],sep,pos[2],sep,pos[1],sep,as.character(p$flightAltitude),sep,group,sep,"1\n")
    
      }
  # create the header
}


