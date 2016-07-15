#' Tool to generate litchi compatible autonomous flightplans focussing an optimal picture retrieval for DSM/DEM and orthophoto generation 
#' with the phantom 3 UAV
#' 
#' @description  makeFlightPlan creates flight control files for the dji
#'   phantom 3 UAV. It ONLY works with the  \code{litchi} flight control app. The
#'   reason using litchi is on the one hand that litchi's better performs on the other hand
#'   and much more important that lichti provides an offline mission planer tool 
#'   with csv format based import/export interface. \cr
#'   So there is no need to use the dji-cloud.\cr
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
#'  "terrainTrack" tries to minimy the waypoints by ommiting altitude differences that exceed
#'   the treshold \code{optFil}:
#'  \preformatted{  
#'  
#'   #--#  #-->             #--#  #
#'   |  |  |               /  /  /
#'   #  #  |              /  /  #
#'   |  |  |             /  /  / 
#'   #  #--#         <--#  #--#
#'  
#'    }
#'   \code{waypoints} is optimal for autonoumous flights under calm conditions in complex terrain 
#'   because the camara takes a picture at every waypoint\cr
#'   \code{terrainTrack}  is optimal for autonoumous flights under calm conditions in less complex terrain 
#'   to maximize the area with the available number of 99 waypoints per mission. It works only with 
#'   automatically triggered picture capturing.\cr
#'   \code{track} is optimal for relatively plain areas and automatically triggered picture capturing
#'   Note: Automatically picture capturing in a time interval works only within the range of the remote control. 
#'   because the the uav needs a trigger signal for taking pictures.
#'   \cr
#'   \cr
#'   The \code{followSurface} switch is used to correct the fixed flight altitude into a terrain following flight altitude. 
#'   NOTE: You have to be aware that the DJI uav is calibrating the altitude at the launch position in the field!
#'   So we need always a DEM to get an estimation of the lauch position. 
#'   You must choose a clearly defined and reliable launching position both in the map and the field. If you fail the aircraft 
#'   probably will hit the terrain...\cr
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
#'  maximumAltitude_of_flightArea + altitude_of_launchposition\cr
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
#'  To get a fixed scale flight the launch altitude is used to correct the flight altitude according to   maximumAltitude of flightArea + altitude of launchposition. With the setting auf terrainfoollowing = true tis is calculated for each waypoint.  . So the adapted flight altitude looks like:
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

#' @param flightArea  you may provide either the coordinates by 
#' c(lon1,lat1,lon2,lat2,lon3,lat3,launchLat,launchLon) or
#' an OGR compatible file (preferably geoJSON or KML) with
#' at least 4 coordinates that describe the flight area. 
#' The fourth coordinate is the launch position.
#'  You will find further explanation under the \link{seealso}. 
#' @param demFn  filname of the corresponding DEM data file
#' @param csvFn filename of output CSV file(s)
#' @param followSurface  \code{boolean}  TRUE performs an altitude correction 
#' of the missions flight altitude using additional DEM data. 
#' If no DEM data is provided and \code{followSurface} is TRUE, 
#' SRTM data will be downloaded and used
#' Further explanation at \link{seealso}
#' @param altFilter if \code{flightPlanMode} is equal \code{"terrainTrack"} then 
#' \code{altFilter} is the treshold value of accepted altitude difference bewteen two waypoints in meter.
#'  If this value is not exceeded the waypoint is omitted due to the fact that only 99 waypoints per mission are allowed.
#' @param flightPlanMode type of flightplan. Available are: \code{"waypoints"}, 
#'   \code{"track"}, \code{"terrainTrack"},  \code{"manual"}.
#' @param presetFlightTask set the camera action at each waypoint.
#'  Options are: 
#' \code{"simple_ortho"} takes one picture/waypoint, 
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and 
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude of the mission. It is 
#'   assumed that the UAV is started at the highest point of the flightarea 
#'   otherwise you have to defined the position of launching.
#' @param overlap overlapping of the pictures in percent (1.0 = 100)
#' @param uavViewDir viewing directon of camera default is \code{0}
#' @param curvesize control parameter for the curve angle at waypoints. 
#' By default it is set to (\code{= 0.0}). If set to \code{-99} it will be 
#' calculated from the swath width of the pictures. NOTE This makes only sense for 
#' \code{flightPlanMode = "terrainTrack"} to smooth curves.
#' For \code{flightPlanMode = "waypoint"} camera actions are DISABLED during curve flights.
#' @param rotationdir camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' @param gimbalmode camera control parameter 
#' \code{0} deactivates the gimbal control
#' \code{1} activates the gimbale for focussing POIs
#' \code{2} activates the gimbale for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
#' @param gimbalpitchangle vertical angle of camera  \code{+30°..-90°}
#' @param actiontype individual actionype settings of the camera c(1,1,...)
#' @param actionparam  corresponding parameter for the above individual actiontype c(0,0,...)
#' @param picRate  picture per second as triggerd by the RC
#' @param heatMap switch for calculating the overlapping factor on a raster map
#' 
#' @author
#' Chris Reudenbach
#'
#' @examples
#' 
#' # Please keep in mind that there is a bunch of interdependent parameter settings.
#' # Hence here are just some typical examples. 
#' 
#' # simple flight, 50 meters above ground 
#' # assuming a flat topography,
#' # generating a heatmap to estimate overlapping
#' fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734),
#'                    flightPlanMode="track",
#'                    heatMap=TRUE)
#'
#' # legend  
#' # red circle      the planned launching point of the uav. 
#' # blue circles    the waypoint position
#' # blue rectangles the corresponding field of view (fov)at the ground
#' # raster[[fp2]]   the digitial elevation model (DEM)
#' # raster[[fp5]]   a heatmap abundance of pictures/pixel
#'
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE)
#' 
#' 
#' # adapting viewing angle
#' fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'                    uavViewDir=30,
#'                    heatMap=TRUE)
#'                   
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE
#' 
#' # adapting the overlap
#' fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'                    overlap=0.8,
#'                    heatMap=TRUE) 
#'                   
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE)
#' 
#' 
#' # terrain following flightplan
#' fp<-makeFlightPlan(flightArea = c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734), 
#'                    followSurface = TRUE,
#'                    demFn = "inst/data/mrbiko.tif",
#'                    )
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,)+mapview(fp[[3]],color="red",cex=5)
#' 
#' ## high resolution (depending on the DEM!) followSurface flight altitude
#' ## camera is controlled manually due to presetFlightTask = "remote"
#' fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734), 
#'                    followSurface = TRUE, 
#'                    flightAltitude = 25, 
#'                    demFn = "inst/data/mrbiko.tif",
#'                    uavViewDir=0,
#'                    presetFlightTask = "remote")
#' 
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,)+mapview(fp[[3]],color="red",cex=5)
#'
#'  
#' # you may also use external vector data defining the flightarea...
#' # digitize flight area using leafDraw()
#' leafDraw(preset="uav")
#' 
#' ## assuming resulting file is names "uav.json"
#'fp<-makeFlightPlan(flightArea = "~/uav.json",
#'                   demFn = "inst/data/mrbiko.tif")
#' 
#' mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+mapview(fp[[3]],color="red",cex=5)
#' 


#' @export makeFlightPlan
#' @export getPresetTask
#' @aliases  makeFlightPlan
#'               

makeFlightPlan<- function(flightArea=NULL,
                          followSurface=FALSE,
                          demFn=NULL,
                          csvFn="dji_litchi_auto_control.csv",
                          altFilter=1.0,
                          flightPlanMode="waypoints",
                          flightAltitude=50,
                          presetFlightTask="remote",
                          curvesize=0,
                          rotationdir=0,
                          gimbalmode=2,
                          gimbalpitchangle=-90,
                          overlap=0.6,
                          uavViewDir=0,
                          picRate=1,
                          heatMap=FALSE,
                          actiontype=NULL,
                          actionparam=NULL)
  {
  
  if (is.null(flightArea)) {stop("### external flight area file or coordinates missing - don't know what to to")
  }
  else {
    # import flight area if provided by an external vector file
    if (class(flightArea)=="numeric" & length(flightArea)>= 8){
      flightArea<-flightArea
    }
    else if (class(flightArea)=="numeric" & length(flightArea)< 8){
      stop("### you did not provide a launching coordinate")
    }
    else {
      test<-try(flightBound<-getFlightBoundary(flightArea))
      if (class(test)!="try-error"){
        flightArea<-flightBound 
      }else{
        stop("### could not read input file")
      }
    }
  }
  # creates the temporyry directory for the CRS, data and layer transfer
  
  flightParams=c(flightPlanMode=flightPlanMode,
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
  
  p<-makeFlightParam(flightArea,flightParams)
  
  
  # flightmode way terrainTrack track
  mode<-as.character(p$flightPlanMode)
  # flight mission name
  mission<-csvFn
  # DEM data 
  demFile<-demFn
  # for terrainTrack filtering altitude in meters from one waypoint to the next
  altdiff<-altFilter
  
  # derived params
  flightAltitude<- as.numeric(flightParams["flightAltitude"])
  
  # overlapping factor as a function of flightAltitude
  fliAltRatio<-1-as.numeric(flightParams["overlap"])
  
  # calculate distances between parallel flight tracks using an empirical relation
  trackDistance<-(fliAltRatio*(1.71*flightAltitude))
  crossDistance<-trackDistance
  if (p$curvesize=="-99") {
  p$curvesize<-crossDistance*0.4
  }
  # calculate speed
  speed<-trackDistance/picRate*3600/1000/picRate
  estSpeed<-speed
  oldPicRate<-picRate
  if (speed> 40){
    speed<-40
    picRate<-trackDistance*60*60/1000/40}
  
  # calculate heading base flight track W-E
  updir<-geosphere::bearing(c(p$lon1,p$lat1),c(p$lon2,p$lat2), a=6378137, f=1/298.257223563)
  
  # calculate heading base flight track E-W
  downdir<-geosphere::bearing(c(p$lon2,p$lat2),c(p$lon1,p$lat1), a=6378137, f=1/298.257223563)
  
  # calculate heading base flight track trackline to trackline
  crossdir<-geosphere::bearing(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  
  
  # calcualte distance of the base flight track
  len<-geosphere::distGeo(c(p$lon1,p$lat1),c(p$lon2,p$lat2))
  
  # calcualte distance of the cross base flight track
  crosslen<-distGeo(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  
  
  # calculate number of pictures/waypoints along one track
  multiply<-floor(len/trackDistance)
  
  # calculate number of tracklines
  tracks<-floor(crosslen/trackDistance)
  
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
  lns<-list()
  
  # starting point
  pos<-c(p$lon1,p$lat1)
  camera<-cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,0,0)
  pOld<-pos
  lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
  #  counter and settings for "track" mode
  if (mode == "track") {
    ##    #tmp <- data.frame(lat=p[2], lon=p[1], latitude=p[2], longitude=p[1],altitude=altitude,heading=uavViewDir,curvesize=curvesize,rotationdir=rotationdir,gimbalmode=gimbalmode,gimbalpitchangle=gimbalpitchangle,actiontype1=actiontype1,actionparam1=actionparam1,actiontype2=actiontype2,actionparam2=actionparam2,actiontype3=actiontype3,actionparam3=actionparam3,actiontype4=actiontype4,actionparam4=actionparam4,actiontype5=actiontype5,actionparam5=actionparam5,actiontype6=actiontype6,actionparam6=actionparam6,actiontype7=actiontype7,actionparam7=actionparam7,actiontype8=actiontype8,actionparam8=actionparam8,id=group)
    ##    #df <- rbind(df, tmp)
    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
    trackDistance <- len
    multiply<-1
  } 
  else if (mode == "waypoints") {
    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)} 
  else if (mode == "terrainTrack") {
    # calculate the real starting point
    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
    # calculate the real starting point
    pos<-calcNextPos(pos[1],pos[2],heading,trackDistance)
    camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))
    #df <- rbind(df,data.frame(lat=p[2], lon=p[1], latitude=p[2], longitude=p[1],altitude=altitude,heading=uavViewDir,curvesize=curvesize,rotationdir=rotationdir,gimbalmode=gimbalmode,gimbalpitchangle=gimbalpitchangle,actiontype1=actiontype1,actionparam1=actionparam1,actiontype2=actiontype2,actionparam2=actionparam2,actiontype3=actiontype3,actionparam3=actionparam3,actiontype4=actiontype4,actionparam4=actionparam4,actiontype5=actiontype5,actionparam5=actionparam5,actiontype6=actiontype6,actionparam6=actionparam6,actiontype7=actiontype7,actionparam7=actionparam7,actiontype8=actiontype8,actionparam8=actionparam8,id=group))
    lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)
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
      camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))
      pOld<-pos
      flightLength<-flightLength+trackDistance
      if (mode =="track"){group<-99}
      lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group,p)
    } 
    
    if ((j%%2 != 0) ){
      
      pos<-calcNextPos(pOld[1],pOld[2],crossdir,crossDistance)
      camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))
      pOld<-pos
      flightLength<-flightLength+crossDistance
      lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group<-99,p)
      heading<-downdir
    } 
    
    else if ((j%%2 == 0) ) {
      
      pos<-calcNextPos(pOld[1],pOld[2], crossdir, crossDistance)
      camera<-spRbind(camera,cameraExtent(pos[1],pos[2],uavViewDir,trackDistance,flightAltitude,i,j))
      pOld<-pos
      flightLength<-flightLength+crossDistance
      lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group<-99,p)
      heading<-updir
    }
  }
  
  fileConn<-file("output2.txt")
  writeLines(unlist(lns), fileConn)
  close(fileConn)
  df<-read.csv("output2.txt",sep=",",header = FALSE)
  names(df) <-unlist(strsplit( makeUavPoint(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = " "))
  
  # make sp
  sp::coordinates(df) <- ~lon+lat
  sp::proj4string(df) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # altitude correction
  
  result<-demCorrection(demFile, df,p,altdiff,followSurface)
  
  
  # calculate time parameters  
  rawTime<-((flightLength/1000)/speed)*60
  litchiTime<-rawTime+0.3*rawTime
  if (heatMap){
  fovH<-fovHeatmap(camera,result[[2]])
  } else
  {
    fovH <-NULL
  }
  # write csv
  writeDroneCSV(result[[1]],csvFn,litchiTime,mode,trackDistance)
  
  
  return(c(cat(" wrote ", mission, " file(s)...\n",
               "\n You asked for", oldPicRate, "pics/sec",
               "\n hence the uav has to go ",estSpeed, " (km/h)",
               "\n ",
               "\n ---------- Please ADAPT flight Params --------------",
               "\n + set speed to:        ", speed,"      (km/h)     + ",
               "\n + set picture rate to: ", picRate,"     (pics/sec) + ",
               "\n ----------------------------------------------------",
               "\n ",
               "\n calculated mission raw time            (min)   : ",rawTime,
               "\n empirically adjusted mission time      (min)   : ",litchiTime,
               "\n ",
               "\n NOTE: ",
               "\n For flightPlanMode='track' files are splitted",
               "\n equally if the task is longer than 20 minutes",
               "\n for flightPlanMode='waypoints' or 'terrainTrack' files ",
               "\n are splitted after 99 waypoints => please check mission time!"),result[[1]],result[[2]],result[[3]],camera,fovH))
  
  
}

##################################################
##################################################

demCorrection<- function(demFile ,df,p,altdiff,followSurface){
  
  if (is.null(demFile)){
    cat("no dem file provided I try to download SRTM data...")
    # download corresponding srtm data
    dem<-robubu::getGeoData(name="SRTM",xtent = extent(p$lon1,p$lon3,p$lat1,p$lat3), zone = 3.0,merge = TRUE)
    dem<- raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
    # extract the altitudes
    df$Altitude<- raster::extract(dem,df)
  } else {
    dem<-raster::raster(demFile)
    dem<-raster::crop(dem,extent(min(p$lon1,p$lon3,p$lon2)-0.005,max(p$lon1,p$lon2,p$lon3)+0.005,min(p$lat1,p$lat2,p$lat3)-0.005,max(p$lat1,p$lat2,p$lat3)+0.005))
  }
  
  # we need the dem in latlon
  demll<-raster::projectRaster(dem,crs = CRS("+proj=longlat +datum=WGS84 +no_defs"),method = "bilinear")
  pos<-as.data.frame(cbind(p$launchLat,p$launchLon))
  
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  
  altitude<-raster::extract(demll,df)
  pos$altitude<-raster::extract(demll,pos)
  maxAlt<-max(altitude,na.rm = TRUE)

  p$flightAltitude=as.numeric(p$flightAltitude)+(maxAlt-as.numeric(pos$altitude))
  
  
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
  return(c(df,dem,pos))
}

# export data to xternal format deals with the splitting of the mission files
writeDroneCSV <-function(df,mission,litchiTime,flightPlanMode,trackDistance){
  
  nofiles<-ceiling(nrow(df@data)/98)
  maxPoints<-98
  minPoints<-1
  if (flightPlanMode =="track" & litchiTime > 20) {
    nofiles<- ceiling(litchiTime/20)
    maxPoints<-ceiling(nrow(df@data)/nofiles)
    mp<-maxPoints
    minPoints<-1
    
  }  
  for (i in 1:nofiles) {
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
    write.csv(df@data[minPoints:maxPoints,1:(ncol(df@data)-2)],file = paste0(mission,i,".csv"),quote = FALSE,row.names = FALSE)
    if (flightPlanMode =="track" & litchiTime > 20) {
      minPoints<-maxPoints
      maxPoints<-maxPoints+mp} 
    else{
      minPoints<-maxPoints
      maxPoints<-maxPoints+98
    }
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
}

# imports external flight
importFlightArea<- function(fN){
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

getFlightBoundary<- function(fN,extend=FALSE){
  flightBound<-importFlightArea(fN)
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
makeFlightParam<- function(flightArea,flightParams){
  # retrieve and recalculate the arguments to provide the flight paramaer for litchi
  validPreset<-c("multi_ortho","simple_ortho","simple_pano","remote")
  validFlightPlan<-c("waypoints","terrainTrack","track","manual")
  stopifnot(flightParams["presetFlightTask"] %in% validPreset)
  stopifnot(flightParams["flightPlanMode"] %in% validFlightPlan)
  
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
  p$lat1<-flightArea[1]
  p$lon1<-flightArea[2]
  p$lat2<-flightArea[3]
  p$lon2<-flightArea[4]
  p$lat3<-flightArea[5]
  p$lon3<-flightArea[6]
  p$launchLat<- flightArea[7]
  p$launchLon<- flightArea[8]
  
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

# obsolet old csv format
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
    
    ID = paste0("CameraExtend_",flightaltitude,"_",floor(runif(1, min=0, max=10000000)))
#  rawPolygon <- sp::Polygon(cbind(c(minx,minx,maxx,maxx,minx),c(miny,maxy,maxy,miny,miny)))
  rawPolygon <- sp::Polygon(cbind(c(xulc,xurc,xlrc,xllc,xulc),c(yulc,yurc,ylrc,yllc,yulc)))
  tileExtend <- sp::Polygons(list(rawPolygon), ID = ID)
  tileExtend <- sp::SpatialPolygons(list(tileExtend))
  #(pid <- sapply(slot(tileExtend, "polygons"), function(x) slot(x, "ID")) )
  df <- data.frame( ID=1:length(rawPolygon), row.names = ID)
  frame <- sp::SpatialPolygonsDataFrame(tileExtend, df)
  sp::proj4string(frame) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  return(frame)
  } 
fovHeatmap<- function(footprint,dem){
  p<-split(footprint,footprint@plotOrder)
  t <- raster(nrow=nrow(dem)*5,ncol=ncol(dem)*5)
  t@crs <-dem@crs
  t@extent<-dem@extent
  
  t<-resample(dem,t)
  t[]<-0
  s<-t
  for (i in seq(1:length(footprint))) {
    tmp<-rasterize(p[[i]],t)
    s <- stack(tmp, s)
  }
  fovhm <- stackApply(s, indices= nlayers(s), fun=sum)
  fovhm[fovhm<1]=NaN
  return(fovhm)
}