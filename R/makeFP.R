if (!isGeneric('makeFP')) {
  setGeneric('makeFP', function(x, ...)
    standardGeneric('makeFP'))
}
#' make flight plans (makeFP) is a tool to generate autonomous flight plans for
#' an optimal picture retrieval with respect to DSM/DEM and orthophoto
#' calculation.
#'
#' @description The basic idea is to provide an easy to use workflow for
#'   controlling rtf UAVs from planning and flying autonoumous surveys to
#'   derivation and postclassification of the data. makeFP (Make Uav Remote
#'   Controlled Survey) creates either intermediate flight control files for the
#'   dji phantom x UAVs or ready to upload control files for the 3DR Solo. The
#'   dji control files are designed for using with the propietary litchi flight
#'   control app exchange format, while the 3DR Solo files are using the MAVLINK
#'   common message set, that is used by the PixHawk flight controller family.
#'   Both are implemented very rudimentary.\cr\cr DJI:\cr The reason using DJI
#'   is their absolute straightforward usage. Everybody can fly with a DJI but
#'   the price ist a hermetically closed system. Only the  litchi app provides
#'   additionally to a cloud based mission planer an offline/standalone
#'   interface to upload a csv formated waypoint file for autonomous flights to
#'   the Phantom.\cr\cr PixHawk/3DR Solo:\cr The open uav community is focussed
#'   on the PixHawk autopilot unit and the Mission Planner software. It is well
#'   documented and serveral APIs are provided. Nevertheless a terrain following
#'   autonomous flight planning tool is not available. makeFP creates static
#'   implementation of the MAV format that is ready to be uploaded directly on
#'   the Pixhawk controller using the upload2Solo function.\cr\cr
#' @section Warning: Take care! There are still a lot of construction zones
#'   around. This script is far beyond to be in a mature state. Please control
#'   and backup all controls again while planning and performing autonomous
#'   flight plans and missions. You will have a lot of chances to make a small
#'   mistake what may yield in a damage of your uav or even worse in involving
#'   people, animals or non-cash assets. Check your risk use parachute systems
#'   and even if it is running like a charm keep alert!

#'
#'
#'
#' @section Basic Introduction:
#' \subsection{Survey Area}{
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
#'   }
#'   \subsection{Terrain Following flightplan}{

#'
#'   The \code{followSurface} switch is used to adapt the fixed flight altitude into a terrain following flight altitude.\cr
#'   ----------------------------------------------------------------------------------------------------------\cr
#'   NOTE: You have to be aware that the DJI uav is calibrating the altitude at the launch position in the field!
#'   So you need either a correct coordinate altitude or a high resolution DEM to get a good! estimation of the lauch position and altitude.
#'   You must choose a clearly defined and reliable launching position both in the map and the field. If you fail I made the experience that the aircraft
#'   probably will hit the terrain...\cr
#'   ----------------------------------------------------------------------------------------------------------\cr\cr
#'
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
#'   }

#' @param projectDir path to the main folder where several projects can be hosted
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
#' @param overlap overlapping of the pictures in percent (1.0 = 100)
#' @param djiBasic c(0,0,0,-90)
#' \cr curvesize (DJI only) controls the curve angle of the uav passing waypoints.
#' By default it is set to (\code{= 0.0}).
#' \cr rotationdir (DJI only) camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' \cr gimbalmode (DJI only) camera control parameter
#' \code{0} deactivates the gimbal control
#' \code{1} activates the gimbale for focussing POIs
#' \code{2} activates the gimbale for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
#' \cr gimbalpitchangle (DJI only) vertical angle of camera  \code{+30 deg..-90 deg}
#' \cr actiontype (DJI only) individual actionype settings of the camera c(1,1,...)
#' \cr actionparam (DJI only) corresponding parameter for the above individual actiontype c(0,0,...)
#' \code{uavViewDir} viewing directon of camera default is \code{0}
#' @param maxSpeed  cruising speed
#' @param heatMap switch for calculating the overlapping factor on a raster map
#' @param picFootprint switch for calculating the footprint at all waypoints
#' @param followSurfaceRes horizontal step distance for analysing the DEM altitudes
#' @param picRate fastest stable interval (s) for shooting pictures
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h
#' @param startLitchi if TRUE it starts an offline Litchi website for converting the data (preliminary workaround)
#' @param maxFlightTime user defined estimation of the lipo lifetime (20 min default)
#' @param rcRange range of estimated range of remote control
#' @param uavType type of uav. currently "djip3" and "solo" are supported
#' @param dA if TRUE the real extent of the used DEM is returned helful for low altitudes flightplanning
#' @param cameraType depending on uav system for dji the dji4k is default for solo you can choose GP3+8MP GP3+11MP and MAPIR2
#'
#' @note
#' To use the script you need to install quite a lot of R-packages and at least the binary GDAL tools as well as SAGA GIS and GRASS GIS according to your system needs. Please find more information at NASA EarthData project: \href{http://giswerk.org/doku.php?id=rs:micrors:uavrs:intro}{uav based Remote Sensing at giswerk.org}).
#'
#'
#' @author
#' Chris Reudenbach
#'
#' @examples
#'
#'\dontrun{
#' # Please keep in mind that there is a bunch of interdependent parameter settings.
#'
#' # The following spatial data sets are returned
#'
#' # lp      the planned launching position of the uav.
#' # wp      waypoints inclusive all informations
#' # oDEM    the original (input) digitial surface model (DSM)
#' # rDEM    the resampled (used) DSM
#' # fp      optimized footprints of the camera
#' # fA      flight area with at least 2 overlaps
#' # rcA     area covered by the RC according to the range and line of sight
#' # hm    a heatmap abundance of pictures/pixel (VERY SLOW, only if heatMap = TRUE)
#'
#'
#' # load example DEM data
#' data(mrbiko) # to use the example data it's easier to write same in tif format
#' writeRaster(mrbiko,"~/dem.tif")
#'
#' #
#'
#'
#' ## (2) simple flight, 50 meters above ground
#' ##     assuming a flat topography,
#' ##     generating a heatmap to estimate overlapping
#'
#' fp<-makeFP(surveyArea = c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734),
#'               demFn = "~/dem.tif",
#'               heatMap = TRUE)
#'
#' ## view results
#' mapview(fp$lp,color="red",cex=5)+
#' mapview(fp$wp,zcol = "altitude",lwd=1,cex=4)+
#' mapview(fp$oDEM)+
#' mapview(fp$fA,color="red", alpha.regions = 0.1,lwd=1.0)+
#' mapview(fp$hm)+
#' fp$demA
#'
#'
#' ## (2) typical real case scenario
#' ##     a flight altitude BELOW 50 m is extreme
#' ##     U have to use a high resulution DSM
#' ##     (here simulated with a standard DEM)
#'
#' fp<-makeFP(projectDir ="/home/creu/uav/test",
#' missionName = "test30",
#' surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#' followSurface = TRUE,
#' flightAltitude = 30,
#' demFn = "~/dem.tif",
#' windCondition = 3,
#' uavType = "djip3",
#' followSurfaceRes = 5,
#' altFilter = .75)
#'
#'
#' ## (3) use of external vector data to define the surveyArea...
#' ##     digitize flight area using leafDraw()
#' ##     save vectors as JS "json" or "kml" files
#' ##     provide full filename+upper extensions!
#'
#' leafDraw(preset="uav")
#'
#' ## assuming resulting file is named "uav.json"
#' ## use it for planning
#'
#' fp<-makeFP(projectDir="~/uav/uav/test",
#'                    missionName = "50m",
#'                    surveyArea="~/uav.json",
#'                    followSurface = TRUE,
#'                    flightAltitude = 50,
#'                    overlap = 0.7,
#'                    demFn = "~/mrbiko.tif",
#'                    altFilter = 3.5,
#'                    maxSpeed = 50,
#'                    windCondition = 3)
#'
#' ## view results
#'
#'  mapview(fp$fA,color="red", alpha.regions = 0.1,lwd=0.5)+
#'  mapview(fp$lp,zcol = "altitude",lwd=1,cex=4)+
#'  mapview(fp$oDEM,color="red",cex=5)+
#' }


#' @export makeFP
#' @export getPresetTask
#' @aliases  makeFP
#'


makeFP <- function(projectDir = "~",
                   missionName = "autoflightcontrol",
                   surveyArea = NULL,
                   flightAltitude = 100,
                   launchAltitude = NULL,
                   followSurface = FALSE,
                   followSurfaceRes = NULL,
                   demFn = NULL,
                   altFilter = 1.0,
                   flightPlanMode = "track",
                   presetFlightTask = "remote",
                   overlap = 0.7,
                   maxSpeed = 20.0,
                   maxFlightTime = 10,
                   picRate = 2,
                   windCondition = 1,
                   uavType = "djip3",
                   cameraType = "MAPIR2",
                   uavViewDir = 0,
                   djiBasic = c(0, 0, 0,-90, 0),
                   dA = FALSE,
                   heatMap = FALSE,
                   picFootprint = FALSE,
                   rcRange = NULL,
                   startLitchi = FALSE)
{
  ###  setup environ and params
  cat("setup environ and params...\n")
  # assign flight mission name
  mission <-
    paste(paste0(missionName, "_", flightAltitude), sep = .Platform$file.sep)
  
  workingDir <- missionName
  # create directories if needed
  if (!file.exists(file.path(projectDir, workingDir))) {
    dir.create(file.path(projectDir, workingDir), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir, workingDir, "run"))) {
    dir.create(file.path(projectDir, workingDir, "/run"), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir, workingDir, "control"))) {
    dir.create(file.path(projectDir, workingDir, "control"), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir, "data"))) {
    dir.create(file.path(projectDir, "data"), recursive = TRUE)
  }
  if (!is.null(demFn)) {
    file.copy(demFn, paste0(file.path(projectDir, "data"), "/", basename(demFn)))
    demFn <-
      paste0(file.path(projectDir, "data"), "/", basename(demFn))
  }
  # setting R environ temp folder to the current working directory
  Sys.setenv(TMPDIR = file.path(projectDir, workingDir, "run"))
  
  # set R working directory
  setwd(file.path(projectDir, workingDir, "run"))
  
  # set common read write permissions
  #Sys.chmod(list.dirs("../.."), "777")
  
  # create log file
  logger <-
    create.logger(logfile = paste0(
      file.path(projectDir, workingDir, "control/"),
      strsplit(basename(mission), "\\.")[[1]][1],
      '.log'
    ))
  level(logger) <- "INFO"
  levellog(logger, 'INFO', "                                                           ")
  levellog(logger, 'INFO', "                                                           ")
  levellog(logger,
           'INFO',
           "--------------------- START RUN ---------------------------")
  levellog(logger, 'INFO', paste("Working folder: ", file.path(projectDir, workingDir)))
  
  # generate misson control filename
  csvFn <-
    paste(
      file.path(projectDir, workingDir, "control"),
      paste0(mission, ".csv"),
      sep = .Platform$file.sep
    )
  
  # get survey area
  surveyArea <- getSurveyExtent(surveyArea, projectDir, logger)
  
  # need picfootprint for calculating the heatmap
  if (heatMap) {
    picFootprint = TRUE
  }
  
  # uav depending parameter setting
  if (uavType == "djip3") {
    factor <- 1.71
    flightParams = c(
      flightPlanMode = flightPlanMode,
      launchAltitude = launchAltitude,
      flightAltitude = flightAltitude,
      presetFlightTask = presetFlightTask,
      overlap = overlap,
      curvesize = djiBasic[1],
      #curvesize
      rotationdir = djiBasic[2],
      #rotationdir
      gimbalmode = djiBasic[3],
      #gimbalmode
      gimbalpitchangle = djiBasic[4],
      #gimbalpitchangle
      uavViewDir = uavViewDir,
      followSurfaceRes = followSurfaceRes
    )
    #calc & assign overlapping factor as a function of flightAltitude
    fliAltRatio <- 1 - overlap
    
    # FOV*agl*(1-overlap)
    uavOptimumSpeed <-
      ceiling(factor * flightAltitude * fliAltRatio)
    
  }
  else if (uavType == "solo") {
    if (cameraType == "MAPIR2") {
      factor <- 1.55
    } else if (cameraType == "GP3+8MP") {
      factor <- 2.3
    } else if (cameraType == "GP3+11MP") {
      factor <- 2.3
    }
    
    
    flightParams = c(
      flightPlanMode = flightPlanMode,
      launchAltitude = launchAltitude,
      flightAltitude = flightAltitude,
      presetFlightTask = presetFlightTask,
      overlap = overlap,
      uavViewDir = uavViewDir,
      followSurfaceRes = followSurfaceRes
    )
    #calc & assign overlapping factor as a function of flightAltitude
    fliAltRatio <- 1 - overlap
    
    # FOV*agl*(1-overlap)
    uavOptimumSpeed <-
      ceiling(factor * flightAltitude * fliAltRatio)
  }
  
  # adapt default flight params to runtime request
  p <- makeFlightParam(surveyArea, flightParams, followSurface)
  
  # assign flightmode
  mode <- as.character(p$flightPlanMode)
  
  # assign flight Altitude
  flightAltitude <- as.numeric(flightParams["flightAltitude"])
  
  
  
  # calc distance beteen two pictures using a camera dependent multiplicator
  trackDistance <-
    calcTrackDistance(fliAltRatio, flightAltitude, factor)
  totalTrackdistance <- trackDistance
  # to keep it simple we tacke picture as squares
  crossDistance <- trackDistance
  
  # calculate survey area
  # create an sp polygon object of the mission area
  taskArea <- taskarea(p, csvFn)
  # reproject it to UTM
  taskAreaUTM <-
    spTransform(taskArea, CRS(
      paste(
        "+proj=utm +zone=",
        long2UTMzone(p$lon1),
        " ellps=WGS84",
        sep = ''
      )
    ))
  # calculate area
  surveyAreaUTM <- rgeos::gArea(taskAreaUTM)
  
  # calculate heading from launch position to mission start position
  launch2startHeading <-
    geosphere::bearing(
      c(p$launchLon, p$launchLat),
      c(p$lon1, p$lat1),
      a = 6378137,
      f = 1 / 298.257223563
    )
  
  # calculate and assign  heading base flight track W-E
  updir <-
    geosphere::bearing(c(p$lon1, p$lat1),
                       c(p$lon2, p$lat2),
                       a = 6378137,
                       f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track E-W
  downdir <-
    geosphere::bearing(c(p$lon2, p$lat2),
                       c(p$lon1, p$lat1),
                       a = 6378137,
                       f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track trackline to trackline
  crossdir <-
    geosphere::bearing(c(p$lon2, p$lat2),
                       c(p$lon3, p$lat3),
                       a = 6378137,
                       f = 1 / 298.257223563)
  
  # calculate and assign  distance of the base flight track
  len <- geosphere::distGeo(c(p$lon1, p$lat1), c(p$lon2, p$lat2))
  
  # calculate and assign distance of the cross base flight track
  crosslen <-
    distGeo(c(p$lon2, p$lat2),
            c(p$lon3, p$lat3),
            a = 6378137,
            f = 1 / 298.257223563)
  if (is.null(followSurfaceRes)) {
    followSurfaceRes <- trackDistance
  }
  
  # IF followSurface set track/crossDistance to followSurfaceRes
  if (followSurface) {
    multiply <- floor(len / followSurfaceRes)
    trackDistance <- followSurfaceRes
    #crossDistance<-followSurfaceRes
  } else{
    multiply <- floor(len / trackDistance)
  }
  
  # calculate and assign  number of tracklines
  tracks <- floor(crosslen / crossDistance)
  
  #set initial heading
  heading <- updir
  
  # set universal view direction of the uav
  if (!is.null(flightParams["uavViewDir"])) {
    uavViewDir <- updir - as.numeric(flightParams["uavViewDir"])
  }
  else {
    uavViewDir <- as.numeric(flightParams["uavViewDir"])
  }
  
  # init of control id #1 common  #99 turnpoints of single tracks
  group <- 1
  
  # set cumulative flightlength to zero
  flightLength <- 0
  
  # initialize djiDF and
  djiDF <- data.frame()
  mavDF <- data.frame()
  
  # define output line var
  lns <- list()
  
  
  lns <-
    launch2flightalt(p, lns, uavViewDir, launch2startHeading, uavType)
  
  
  # assign starting point
  pos <- c(p$lon1, p$lat1)
  # calculates the footprint of the first position and returns a SpatialPolygonsDataFrame
  if (picFootprint) {
    camera <-
      cameraExtent(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0)
  }
  else {
    camera = "NULL"
  }
  # creates the export control parameter set of the first position
  if (uavType == "djip3") {
    lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
  }
  if (uavType == "solo") {
    lns[length(lns) + 1] <-
      makeUavPointMAV(
        lat = pos[2],
        lon = pos[1],
        head = uavViewDir,
        group = 99
      )
  }
  # push pos to old pos
  pOld <- pos
  
  # set counter and params for mode = "track" mode
  if (mode == "track") {
    if (uavType == "djip3") {
      lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    }
    if (uavType == "solo") {
      lns[length(lns) + 1] <-
        makeUavPointMAV(
          lat = pos[2],
          lon = pos[1],
          head = uavViewDir,
          group = 99
        )
    }
    trackDistance <- len
    multiply <- 1
  }
  # set counter and params for mode = "waypoints"
  else if (mode == "waypoints") {
    if (uavType == "djip3") {
      lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    }
    if (uavType == "solo") {
      lns[length(lns) + 1] <-
        makeUavPointMAV(
          lat = pos[2],
          lon = pos[1],
          head = uavViewDir,
          group = 99
        )
    }
  }
  # set counter and params for mode = "terrainTrack"
  else if (mode == "terrainTrack") {
    group = 99
  }
  #
  cat("calculating waypoints...\n")
  pb <- pb <- txtProgressBar(max = tracks, style = 3)
  # then do for the rest  forward and backward
  for (j in seq(1:tracks)) {
    for (i in seq(1:multiply)) {
      if (mode == "waypoints" || mode == "terrainTrack") {
        if (i >= multiply) {
          group <- 99
        }
        else      {
          group <- 1
        }
      }
      else {
        i <- 2
      }
      
      # calc next coordinate
      pos <- calcNextPos(pOld[1], pOld[2], heading, trackDistance)
      if (picFootprint) {
        camera <-
          spRbind(
            camera,
            cameraExtent(
              pos[1],
              pos[2],
              uavViewDir,
              trackDistance,
              flightAltitude,
              i,
              j
            )
          )
      }
      pOld <- pos
      flightLength <- flightLength + trackDistance
      if (mode == "track") {
        group <- 99
      }
      if (uavType == "djip3") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = group, p)
      }
      if (uavType == "solo") {
        lns[length(lns) + 1] <-
          makeUavPointMAV(
            lat = pos[2],
            lon = pos[1],
            head = uavViewDir,
            group = group
          )
      }
    }
    
    if ((j %% 2 != 0)) {
      pos <- calcNextPos(pOld[1], pOld[2], crossdir, crossDistance)
      if (picFootprint) {
        camera <-
          spRbind(
            camera,
            cameraExtent(
              pos[1],
              pos[2],
              uavViewDir,
              trackDistance,
              flightAltitude,
              i,
              j
            )
          )
      }
      pOld <- pos
      flightLength <- flightLength + crossDistance
      if (uavType == "djip3") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      }
      if (uavType == "solo") {
        lns[length(lns) + 1] <-
          makeUavPointMAV(
            lat = pos[2],
            lon = pos[1],
            head = uavViewDir,
            group = 99
          )
      }
      heading <- downdir
    }
    
    else if ((j %% 2 == 0)) {
      pos <- calcNextPos(pOld[1], pOld[2], crossdir, crossDistance)
      if (picFootprint) {
        camera <-
          spRbind(
            camera,
            cameraExtent(
              pos[1],
              pos[2],
              uavViewDir,
              trackDistance,
              flightAltitude,
              i,
              j
            )
          )
      }
      pOld <- pos
      flightLength <- flightLength + crossDistance
      if (uavType == "djip3") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      }
      if (uavType == "solo") {
        lns[length(lns) + 1] <-
          makeUavPointMAV(
            lat = pos[2],
            lon = pos[1],
            head = uavViewDir,
            group = 99
          )
      }
      heading <- updir
    }
    # status bar
    setTxtProgressBar(pb, j)
  }
  close(pb)
  
  #estimate time regarding parameter
  ft <-
    calculateFlightTime(
      maxFlightTime,
      windCondition,
      maxSpeed,
      uavOptimumSpeed,
      flightLength,
      totalTrackdistance,
      picRate,
      logger
    )
  rawTime <- ft[1]
  maxFlightTime <- ft[2]
  maxSpeed <- ft[3]
  picIntervall <- ft[4]
  
  
  # postprocessing
  fileConn <- file("tmp.csv")
  cat("preprocessing DEM related stuff...\n")
  if (uavType == "djip3") {
    # dump lns to file for read in as csv
    writeLines(unlist(lns[1:length(lns) - 1]), fileConn)
    djiDF <- read.csv("tmp.csv", sep = ",", header = FALSE)
    # add correct header
    names(djiDF) <-
      unlist(strsplit(
        makeUavPoint(
          pos,
          uavViewDir,
          group = 99,
          p,
          header = TRUE,
          sep = ' '
        ),
        split = " "
      ))
    # make it spatial
    sp::coordinates(djiDF) <- ~ lon + lat
    sp::proj4string(djiDF) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs")
    # now DEM stuff
    result <-
      demCorrection(
        demFn,
        djiDF,
        p,
        altFilter,
        followSurface,
        followSurfaceRes,
        logger,
        projectDir,
        dA,
        workingDir
      )
    # assign adapted dem to demFn
    demFn <- result[[3]]
    dfcor <- result[[2]]
    
    # max numbers of dji waypoints is due to factory limits 98
    # according to start and rth safety we need 6 points for organizig the splitted task
    nofiles <- ceiling(nrow(dfcor@data) / 90)
    maxPoints <- 90
    minPoints <- 1
    # check if the flighttime is forcing more files
    if (nofiles < ceiling(rawTime / maxFlightTime)) {
      nofiles <- ceiling(rawTime / maxFlightTime)
      maxPoints <- ceiling(nrow(dfcor@data) / nofiles) + 1
      mp <- maxPoints
      minPoints <- 1
    }
    # start the creation of the control file(s)
    cat('generate control files...')
    generateDjiCSV(
      result[[2]],
      mission,
      nofiles,
      maxPoints,
      p,
      logger,
      round(result[[6]], digit = 0),
      trackSwitch,
      "flightDEM.tif",
      result[[8]],
      projectDir,
      workingDir
    )
    
  }
  else if (uavType == "solo") {
    writeLines(unlist(lns), fileConn)
    mavDF <- read.csv("tmp.csv", sep = "\t", header = FALSE)
    names(mavDF) <-
      c(
        "a",
        "b",
        "c",
        "d",
        "e",
        "f",
        "g",
        "latitude",
        "longitude",
        "altitude",
        "id",
        "j",
        "lat",
        "lon"
      )
    sp::coordinates(mavDF) <- ~ lon + lat
    sp::proj4string(mavDF) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    if (is.null(launchAltitude)) {
      result <-
        demCorrection(
          demFn,
          mavDF,
          p,
          altFilter,
          followSurface,
          followSurfaceRes,
          logger,
          projectDir,
          dA,
          workingDir
        )
      
      # assign adapted dem to demFn
      lauchPos <- result[[1]]
      dfcor <- result[[2]]
      demFn <- result[[3]]
      
      nofiles <- ceiling(rawTime / maxFlightTime)
      maxPoints <- ceiling(nrow(dfcor@data) / nofiles) + 1
      
    }
    generateMavCSV(
      result[[2]],
      mission,
      nofiles,
      rawTime,
      mode,
      trackDistance,
      maxFlightTime,
      logger,
      p,
      len,
      multiply,
      tracks,
      result,
      maxSpeed / 3.6,
      uavType,
      "flightDEM.tif",
      maxAlt = result[[6]],
      projectDir,
      workingDir
    )
  }
  close(fileConn)
  
  # if heatMap is requested
  if (heatMap) {
    cat("calculating picture coverage heat map\n")
    fovH <- fovHeatmap(camera, demFn)
  } else
  {
    fovH <- "NULL"
  }
  
  
  
  # call rcShed
  if (!is.null(rcRange)) {
    envGIS <-
      initRGIS(root.dir = projectDir,
               working.dir = workingDir,
               fndem = demFn)
    cat("calculating RC-range\n")
    rcCover <-
      rcShed(
        envGIS,
        launchP = c(as.numeric(p$launchLon), as.numeric(p$launchLat)),
        flightAlt =  as.numeric(p$flightAltitude),
        rcRange = rcRange,
        dem = envGIS$fn
      )
  } else {
    rcCover = "NULL"
  }
  
  
  
  
  # write log file status and params
  levellog(logger, 'INFO', paste("missionname     : ", mission))
  levellog(logger, 'INFO', paste("DEM filename    : ", names(demFn)))
  levellog(logger, 'INFO', paste("surveyArea      : ", surveyAreaUTM))
  levellog(logger, 'INFO', paste("launchAltitude  : ", launchAltitude))
  levellog(logger, 'INFO', paste("followSurface   : ", followSurface))
  levellog(logger, 'INFO', paste("altfilter       : ", altFilter))
  levellog(logger, 'INFO', paste("flightPlanMode  : ", flightPlanMode))
  levellog(logger, 'INFO', paste("flightAltitude  : ", flightAltitude))
  levellog(logger,
           'INFO',
           paste("presetFlightTask: ", presetFlightTask))
  levellog(logger, 'INFO', paste("curvesize       : ", p$curvesize))
  levellog(logger, 'INFO', paste("rotationdir     : ", p$rotationdir))
  levellog(logger, 'INFO', paste("gimbalmode      : ", p$gimbalmode))
  levellog(logger,
           'INFO',
           paste("gimbalpitchangle: ", p$gimbalpitchangle))
  levellog(logger, 'INFO', paste("overlap         : ", overlap))
  levellog(logger, 'INFO', paste("uavViewDir      : ", uavViewDir))
  levellog(logger, 'INFO', paste("picFootprint    : ", picFootprint))
  levellog(logger,
           'INFO',
           paste("followSurfaceRes: ", followSurfaceRes))
  levellog(logger, 'INFO', paste("surveyAreaCoords: ", surveyArea))
  levellog(logger, 'INFO', paste("windCondition   : ", windCondition))
  levellog(logger, 'INFO', "-")
  levellog(logger,
           'INFO',
           "----- use the following mission params! --------------")
  levellog(logger,
           'INFO',
           paste("set RTH flight altitude to    : ", round(result[[6]], digit = 0), " (m)"))
  levellog(logger,
           'INFO',
           paste(
             "set mission speed to a max of: ",
             round(maxSpeed, digit = 1),
             "  (km/h)      "
           ))
  levellog(logger,
           'INFO',
           paste("set pic rate to at least : ", picIntervall, "  (sec/pic) "))
  levellog(logger,
           'INFO',
           paste("calculated mission time    : ", rawTime,      "  (min)      "))
  levellog(logger,
           'INFO',
           paste("estimated battery liftime  : ", maxFlightTime,      "  (min)      "))
  levellog(logger,
           'INFO',
           paste("Area covered               : ", surveyAreaUTM / 10000,      "  (ha)"))
  # return params for visualisation and main results for overview
  if (startLitchi) {
    openLitchi()
    cat("--- END ", mission, " Litchi ---")
  }
  if ((flightPlanMode == 'track' |
       flightPlanMode == 'terrainTrack') & rawTime > maxFlightTime)
  {
    note <-
      "flighttime > battery lifetime! control files have been splitted. Have Fun..."
  }
  else if (flightPlanMode == 'waypoints')
  {
    note <-
      "control files are splitted after max 98 waypoints (litchi control file restricted number)"
  }
  else {
    note <- " Have Fun "
  }
  dumpFile(paste0(
    file.path(projectDir, workingDir, "control/"),
    strsplit(basename(mission), "\\.")[[1]][1],
    '.log'
  ))
  cat(
    "\n ",
    "\n NOTE 1:",
    as.character(note),
    "",
    "\n NOTE 2: You will find all parameters in the logfile:",
    paste0(
      file.path(projectDir, workingDir, "control/"),
      strsplit(basename(mission), "\\.")[[1]][1],
      '.log'
    ),
    "",
    "\n "
  )
  x <- c(result[[1]],
         # launch Pos
         result[[2]],
         # waypoints
         result[[5]],
         # resampled dem contour
         result[[3]],
         # original DEM
         result[[4]],
         # resampled dem
         camera,
         # camera footprint (DJI only)
         taskArea,
         # Area of flight task
         rcCover,
         # Estimated area that is covered by RC
         fovH)                 # Heatmap of overlapping Pictures
  names(x) <- c("lp", "wp", "demA", "oDEM", "rDEM", "fp", "fA", "rcA", "hm")
  return(x)
}