#' Basic utility to create litchi compatible autonomous flightplans for the 
#' phantom 3 drone
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
#'  "optway" tries to minimy the waypoints by ommiting altitude differences that exceed
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
#'   \code{optway}  is optimal for autonoumous flights under calm conditions in less complex terrain 
#'   to maximize the area with the available number of 99 waypoints per mission. It works only with 
#'   automatically triggered picture capturing.\cr
#'   \code{track} is optimal for relatively plain areas and automatically triggered picture capturing
#'   Note: Automatically picture capturing in a time interval works only within the range of the remote control. 
#'   because the the uav needs a trigger signal for taking pictures.
#'   \cr
#'   \cr
#'   The \code{terrainfollowing} switch is used to correct the fixed flight altitude into a terrain following flight altitude. 
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

#' @param flightArea  \code{numeric:}  you may provide either the coordinates by 
#'   numbers c(lon1,lat1,lon2,lat2,lon3,lat3,launchLat,launchLon) or an OGR file (preferably geoJSON or KML) with at least 5 coordinates that describe the flight area. You will find further explanation under the \link{note}. 
#' @param useExt \code{boolean:} TRUE uses instead of the point coordinates the extend of \code{flightArea}
#' @param terrainfollowing  \code{boolean}  TRUE performs an altitude correction of the missions flight altitude using DEM data. You will find further explanation under the \link{note}
#' If no DEM data is provided and \code{terrainfollowing} is TRUE  SRTM data will be downloaded and used
#' @param demfN \code{string} filname linking to the DEM data 
#' @param ofN \code{string:}  csv output filename
#' @param flightPlanMode \code{string} set the type of flightplan. Available are: \code{waypoints}, 
#'   \code{track}  \code{optway}  \code{manual}.
#' @param terrainFil \code{mumeric} If \code{flightPlanMode} is equal \code{optway} 
#'   \code{terrainFil} determines the treshold of accepted altitude difference  (m) that is 
#'   considered between regular waypoints. If not exceeded the waypoint is omitted
#' @param flightAltitude \code{numeric} set the flight altitude of the whole flight. It is 
#'   assumed that the UAV is started at the highest point of the flightarea otherwise you have to dermine defined coordinate of launching.
#' @param presetFlightTask \code{string} set the camera action at each 
#'   waypoint.  \code{simple_ortho} takes one picture/waypoint, 
#'   \code{multi_ortho} takes 4 picture at a waypoint, two vertically down and 
#'   two in forward and backward viewing direction and an angele of -60deg and 
#'   \code{simple_pano} takes a 360 deg panorama picture
#' @param overlap \code{numeric} overlapping ratio of the pictures
#' @param curvesize \code{numeric} necessary litchi params you may use them but actually there 
#'   is no need
#' @param rotationdir \code{numeric} necessary litchi params you may use them but actually 
#'   there is no need
#' @param gimbalmode \code{numeric} necessary litchi params you may use them but actually there
#'   is no need
#' @param gimbalpitchangle \code{numeric} necessary litchi params you may use them but actually
#'   there is no need
#' @param uavViewDir \code{numeric} viewing angle of the camera optimal is 90 degree rotated 
#'   against the flight direction
#' @param actiontype \code{numeric} the actionype of the camera control c(1,1,...)
#' @param actionparam \code{numeric} the parameter for the corresponding actiontype c(0,0,...)
#' @param launchPos \code{numeric} coordinates of launching position
#' @param picRate \code{numeric} picture per second as triggerd by the remote control


#' @param uavStartCoordinate c(lat.lon) of the planned launch position note this is important due to the altitude correction

#' @author
#' Chris Reudenbach
#'
#' @examples

#' ### we need sp and raster ###
#' library(mapview)
#' library(raster)
#' library(sp)
#' library(geosphere)
#' library(rgdal)
#' library(tools)
#' library(maptools)
#'

#'## flight 50 meters above ground over a flat topography
#' fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734))
#' 
#'# -> \code{red circle} mark the planned launching point of the uav. 
#'# -> \code{blue circles} mark the waypoint position
#'# -> \code{blue rectangles} mark the corresponding field of view (fov)at the ground
#'# -> \code{raster[[fp2]]} the digitial elevation model (DEM)
#'# -> \code{raster[[fp5]]} represents a heatmap pictures/pixel (of the DEM)
#'
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,color="blue")+mapview(fp[[3]],color="red",cex=5)+mapview(fp[[5]],legend=TRUE,alpha.regions = 1)
#' 
#' 
#' ## changing area and overlapping by adapting the viewing angle of the camera
#'fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'                   uavViewDir=30) 
#'                   
#'mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,color="blue")+mapview(fp[[3]],color="red",cex=5)
#' 
#' ## changing area and overlapping by adapting the overlap
#'fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'                   overlap=0.8) 
#'                   
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,color="blue")+mapview(fp[[3]],color="red",cex=5)
#' 
#' 
#' ## make a terrain following flightplan
#' fp<-makeFlightPlan(flightArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734), 
#'                    terrainfollowing=TRUE,
#'                    demfN="~/mrbiko.tif")
#'                    
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,color="blue")+mapview(fp[[3]],color="red",cex=5)
#' 
#' 
#' 
#' ## digitize flight area using leafDraw()
#' leafDraw(preset="uav")
#' 
#' ## assuming resulting file is names "uav.json"
#' fp<-makeFlightPlan(flightArea = "~/uav.json")
#' 
#' mapview(fp[[2]])+mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4,color="blue")+mapview(fp[[3]],color="red",cex=5)
#' 


#' @export makeFlightPlan
#' @export getPresetTask
#' @aliases  makeFlightPlan
#'               

makeFlightPlan<- function(flightArea=NULL,
                          useExt=FALSE,
                          terrainfollowing=FALSE,
                          demfN=NULL,
                          ofN="dji_litchi_auto_control.csv",
                          terrainFil=1.0,
                          flightPlanMode="waypoints",
                          flightAltitude=50,
                          presetFlightTask="simple_ortho",
                          curvesize=0.2,
                          rotationdir=0,
                          gimbalmode=0,
                          gimbalpitchangle=0,
                          overlap=0.6,
                          uavViewDir=90,
                          picRate=2.2,
                          launchPos=NULL,
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
      test<-try(flightBound<-getFlightBoundary(flightArea,useExt))
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
  
  p<-makeFlightParam(flightArea,flightParams,useExt)
  
  
  # flightmode way optway track
  mode<-as.character(p$flightPlanMode)
  # flight mission name
  mission<-ofN
  # DEM data 
  demFile<-demfN
  # for optway filtering altitude in meters from one waypoint to the next
  altdiff<-terrainFil
  
  # derived params
  flightAltitude<- as.numeric(flightParams["flightAltitude"])
  
  # overlapping factor as a function of flightAltitude
  fliAltRatio<-1-as.numeric(flightParams["overlap"])
  
  # calculate distances between parallel flight tracks using an empirical relation
  trackDistance<-(fliAltRatio*(1.71*flightAltitude))
  crossDistance<-trackDistance
  
  # calculate speed
  speed<-trackDistance/picRate*60*60/1000
  if (speed> 50){
    speed<-50
    picRate<-trackDistance*60*60/1000/50}
  
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
  else if (mode == "optway") {
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
      if (mode=="waypoints" || mode == "optway") {
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
  
  result<-demCorrection(demfN, df,p,altdiff,terrainfollowing)
  
  
  # calculate time parameters  
  rawTime<-((flightLength/1000)/speed)*60
  litchiTime<-rawTime+0.3*rawTime
  fovH<-fovHeatmap(camera,result[[2]])
  
  # write csv
  writeDroneCSV(result[[1]],ofN,litchiTime,mode,trackDistance)
  
  
  return(c(cat("wrote ", mission, " file(s)\n         ",
               "\n calculated speed for 1 pic each", picRate, " sec  (km/h)  : ", speed,
               "\n calculated mission raw time               (min)   : ",rawTime,
               "\n empirically adjusted mission time         (min)   : ",litchiTime,
               "\n NOTE: ",
               "\n For flightPlanMode='track' files are splitted",
               "\n equally if the task is longer than 20 minutes",
               "\n for flightPlanMode='way' or 'optway' files ",
               "\n are splitted after 99 waypoints => please check mission time!"),result[[1]],result[[2]],result[[3]],camera,fovH))
  
  
}

##################################################
##################################################

demCorrection<- function(demFile ,df,p,altdiff,terrainfollowing){
  
  if (is.null(demFile)){
    cat("no dem file provided I try to download SRTM data...")
    # download corresponding srtm data
    dem<-robubu::getGeoData(name="SRTM",xtent = extent(p$lon1,p$lon3,p$lat1,p$lat3), zone = 3.0,merge = TRUE)
    dem<- raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
    # extract the altitudes
    df$Altitude<- raster::extract(dem,df)
  } else {
    dem<-raster::raster(demFile)
    dem<-raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
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
  
  
  if (terrainfollowing) {
    altitude<-altitude+as.numeric(p$flightAltitude)-maxAlt
    df$altitude<-altitude
    
    if ( as.character(p$flightPlanMode) == "optway") {
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

getFlightBoundary<- function(fN,extend){
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
makeFlightParam<- function(flightArea,flightParams,useExt){
  # retrieve and recalculate the arguments to provide the flight paramaer for litchi
  validPreset<-c("multi_ortho","simple_ortho","simple_pano")
  validFlightPlan<-c("waypoints","optway","track","manual")
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
  else if (flightParams["flightPlanMode"] =="optway" | flightParams["flightPlanMode"] =="track"){
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
  p$flightPlanMode<- flightParams["flightPlanMode"] # waypoints, optway track
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
    flightParams=actiontype=c(1,0,4,180,5,-60,1,0,5,-90,1,0)
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
  t<-dem*0
  s<-t
  for (i in seq(1:length(footprint))) {
    tmp<-rasterize(p[[i]],dem)
    s <- stack(tmp, s)
  }
  fovhm <- stackApply(s, indices= nlayers(s), fun=sum)
  fovhm[fovhm<1]=NaN
  return(fovhm)
}