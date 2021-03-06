% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeFP.R
\name{makeFP}
\alias{makeFP}
\title{make flight plans (makeFP) is a tool to generate autonomous flight plans for
an optimal picture retrieval with respect to DSM/DEM and orthophoto
calculation.}
\usage{
makeFP(projectDir = "~", missionName = "autoflightcontrol",
  surveyArea = NULL, flightAltitude = 100, launchAltitude = NULL,
  followSurface = FALSE, followSurfaceRes = NULL, demFn = NULL,
  altFilter = 1, flightPlanMode = "track", presetFlightTask = "remote",
  overlap = 0.7, maxSpeed = 20, maxFlightTime = 10, picRate = 2,
  windCondition = 1, uavType = "djip3", cameraType = "MAPIR2",
  uavViewDir = 0, djiBasic = c(0, 0, 0, -90, 0), dA = FALSE,
  heatMap = FALSE, picFootprint = FALSE, rcRange = NULL,
  startLitchi = FALSE)
}
\arguments{
\item{projectDir}{path to the main folder where several projects can be hosted}

\item{missionName}{base string for mission filenames}

\item{surveyArea}{you may provide either the coordinates by
c(lon1,lat1,lon2,lat2,lon3,lat3,launchLat,launchLon) or
an OGR compatible file (preferably geoJSON or KML) with
at least 4 coordinates that describe the flight area.
The fourth coordinate is the launch position.
 You will find further explanation under the \link{seealso}.}

\item{flightAltitude}{set the default flight altitude of the mission. It is
assumed that the UAV is started at the highest point of the surveyArea
otherwise you have to defined the position of launching.}

\item{launchAltitude}{absolute altitude of launching position.
It will overwrite the DEM based estimation if any other value than -9999}

\item{followSurface}{\code{boolean}  TRUE performs an altitude correction
of the missions flight altitude using additional DEM data.
If no DEM data is provided and \code{followSurface} is TRUE,
SRTM data will be downloaded and used
Further explanation at \link{seealso}}

\item{followSurfaceRes}{horizontal step distance for analysing the DEM altitudes}

\item{demFn}{filname of the corresponding DEM data file}

\item{altFilter}{if \code{followingTerrain} is equal \code{TRUE} then
\code{altFilter} is the treshold value of accepted altitude difference bewteen two waypoints in meter.
 If this value is not exceeded the waypoint is omitted due to the fact that only 99 waypoints per mission are allowed.}

\item{flightPlanMode}{type of flightplan. Available are: \code{"waypoints"},
\code{"track"}, \code{"manual"}.}

\item{presetFlightTask}{(DJI only) strongly recommended to use "remote"
       \cr
 Options are:
\code{"simple_ortho"} takes one picture/waypoint,
\code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
\code{"simple_pano"} takes a 360 deg panorama picture and
\code{"remote"} which assumes that the camera is controlled by the remote control (RC)}

\item{overlap}{overlapping of the pictures in percent (1.0 = 100)}

\item{maxSpeed}{cruising speed}

\item{maxFlightTime}{user defined estimation of the lipo lifetime (20 min default)}

\item{picRate}{fastest stable interval (s) for shooting pictures}

\item{windCondition}{1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h}

\item{uavType}{type of uav. currently "djip3" and "solo" are supported}

\item{cameraType}{depending on uav system for dji the dji4k is default for solo you can choose GP3+8MP GP3+11MP and MAPIR2}

\item{djiBasic}{c(0,0,0,-90)
\cr curvesize (DJI only) controls the curve angle of the uav passing waypoints.
By default it is set to (\code{= 0.0}).
\cr rotationdir (DJI only) camera control parameter set the UAV basic turn direction to right (0) or left (1)
\cr gimbalmode (DJI only) camera control parameter
\code{0} deactivates the gimbal control
\code{1} activates the gimbale for focussing POIs
\code{2} activates the gimbale for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
\cr gimbalpitchangle (DJI only) vertical angle of camera  \code{+30 deg..-90 deg}
\cr actiontype (DJI only) individual actionype settings of the camera c(1,1,...)
\cr actionparam (DJI only) corresponding parameter for the above individual actiontype c(0,0,...)
\code{uavViewDir} viewing directon of camera default is \code{0}}

\item{dA}{if TRUE the real extent of the used DEM is returned helful for low altitudes flightplanning}

\item{heatMap}{switch for calculating the overlapping factor on a raster map}

\item{picFootprint}{switch for calculating the footprint at all waypoints}

\item{rcRange}{range of estimated range of remote control}

\item{startLitchi}{if TRUE it starts an offline Litchi website for converting the data (preliminary workaround)}
}
\description{
The basic idea is to provide an easy to use workflow for
  controlling rtf UAVs from planning and flying autonoumous surveys to
  derivation and postclassification of the data. makeFP (Make Uav Remote
  Controlled Survey) creates either intermediate flight control files for the
  dji phantom x UAVs or ready to upload control files for the 3DR Solo. The
  dji control files are designed for using with the propietary litchi flight
  control app exchange format, while the 3DR Solo files are using the MAVLINK
  common message set, that is used by the PixHawk flight controller family.
  Both are implemented very rudimentary.\cr\cr DJI:\cr The reason using DJI
  is their absolute straightforward usage. Everybody can fly with a DJI but
  the price ist a hermetically closed system. Only the  litchi app provides
  additionally to a cloud based mission planer an offline/standalone
  interface to upload a csv formated waypoint file for autonomous flights to
  the Phantom.\cr\cr PixHawk/3DR Solo:\cr The open uav community is focussed
  on the PixHawk autopilot unit and the Mission Planner software. It is well
  documented and serveral APIs are provided. Nevertheless a terrain following
  autonomous flight planning tool is not available. makeFP creates static
  implementation of the MAV format that is ready to be uploaded directly on
  the Pixhawk controller using the upload2Solo function.\cr\cr
}
\note{
To use the script you need to install quite a lot of R-packages and at least the binary GDAL tools as well as SAGA GIS and GRASS GIS according to your system needs. Please find more information at NASA EarthData project: \href{http://giswerk.org/doku.php?id=rs:micrors:uavrs:intro}{uav based Remote Sensing at giswerk.org}).
}
\section{Warning}{
 Take care! There are still a lot of construction zones
  around. This script is far beyond to be in a mature state. Please control
  and backup all controls again while planning and performing autonomous
  flight plans and missions. You will have a lot of chances to make a small
  mistake what may yield in a damage of your uav or even worse in involving
  people, animals or non-cash assets. Check your risk use parachute systems
  and even if it is running like a charm keep alert!
}

\section{Basic Introduction}{

\subsection{Survey Area}{

  To define a flight area you have to provide either 4 Points (or 3 lines).
  You may take more complex vectors like a multi point polygon,
  but only the first 4 coordinates x1, x2, x3 and x4 (for the launching position)
  are used in exactly this order.
  If you take a rectangle the 4th corner coordinate will be the launching point!
  \cr\cr
  The concept is looking like the following sketch.
 \preformatted{
  x2------x3           x2-------x1
  | a                 /
  |                  /
  |   x4            / x4
  |  /             / /
  x1/             x3/
  }
  This coordinates the length of the line and the angle are used to calculate extend and paralells
  of the flightplan according to the flight altitude, overlap etc. Note the flight direction depends on
  the order of the points. If the \code{flightPlanMode} is equal \code{tracks}.
  \cr\cr
  The result look like this.

 \preformatted{

  #--#  #-->             #--#  #
  |  |  |               /  /  /
  |  |  |              /  /  /
  |  |  |             /  /  /
  #  #--#         <--#  #--#
  }
 If \code{flightPlanMode} is equal \code{waypoints} the result is an equal spatial distribution of waypoints:
 \preformatted{

  #--#  #-->             #--#  #
  |  |  |               /  /  /
  #  #  #              #  #  #
  |  |  |             /  /  /
  #  #--#         <--#  #--#

  }


  \code{waypoints} is optimal for autonoumous flights under calm conditions in complex terrain
  because the camara takes a picture at every waypoint\cr
  \code{track} is optimal for relatively plain areas and automatically triggered picture capturing
  Note: Automatically picture capturing in a time interval works only within the range of the remote control.
  because the the uav needs a trigger signal for taking pictures.
  }
  \subsection{Terrain Following flightplan}{

  The \code{followSurface} switch is used to adapt the fixed flight altitude into a terrain following flight altitude.\cr
  ----------------------------------------------------------------------------------------------------------\cr
  NOTE: You have to be aware that the DJI uav is calibrating the altitude at the launch position in the field!
  So you need either a correct coordinate altitude or a high resolution DEM to get a good! estimation of the lauch position and altitude.
  You must choose a clearly defined and reliable launching position both in the map and the field. If you fail I made the experience that the aircraft
  probably will hit the terrain...\cr
  ----------------------------------------------------------------------------------------------------------\cr\cr

 How it works. Let us assume a defined flightaltitude of 50 m.
 According to the launching point altitude the uav will act like the following sketch shows:

\preformatted{

  ............... x_(uav)_x ........... uav started at 30 m altitude results in
                                           a "real" flight altitude of 30m + 50m => 80m


                  ___60m____
                 |          |
         30m _x__|          |
        ____|               |___
    ___|                        |____


                 ___60m____
      ..........|          |............ uav started at 0 m altitude results in
             ___|          |___          "real" flight altitude of 50m above 0m
        ____|                  |
    ___|                       |__x__ 0m
  }
 To avoid negative impacts from the P3 auto calibration, the launch altitude is used to
 correct the flight altitude according to: \cr
 maximumAltitude_of_surveyArea + altitude_of_launchposition\cr
 So the adapted flight altitude is always seen as the flight altitude above the highest terrain altitude:
  \preformatted{

 ...................................... real altitude of uav 110 m


                 ___60m____
                |          |
         30m _x_|          |___
        ____|                  |
    ___|                       |______
  }
 To get a fixed scale flight the launch altitude is used to correct the flight altitude according to   maximumAltitude of surveyArea + altitude of launchposition. With the setting auf terrainfoollowing = true tis is calculated for each waypoint.  . So the adapted flight altitude looks like:
  \preformatted{

                 ..........
                |          |
            ....|          |....
       ....|     ___60m____    |
  ....|         |          |   |....... real altitude of uav 50m
         30m _x_|          |___
        ____|                  |
    ___|                       |___x___ 0m
  }
  }
}
\examples{

\dontrun{
# Please keep in mind that there is a bunch of interdependent parameter settings.

# The following spatial data sets are returned

# lp      the planned launching position of the uav.
# wp      waypoints inclusive all informations
# oDEM    the original (input) digitial surface model (DSM)
# rDEM    the resampled (used) DSM
# fp      optimized footprints of the camera
# fA      flight area with at least 2 overlaps
# rcA     area covered by the RC according to the range and line of sight
# hm    a heatmap abundance of pictures/pixel (VERY SLOW, only if heatMap = TRUE)


# load example DEM data
data(mrbiko) # to use the example data it's easier to write same in tif format
writeRaster(mrbiko,"~/dem.tif")

#


## (2) simple flight, 50 meters above ground
##     assuming a flat topography,
##     generating a heatmap to estimate overlapping

fp<-makeFP(surveyArea = c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734),
              demFn = "~/dem.tif",
              heatMap = TRUE)

## view results
mapview(fp$lp,color="red",cex=5)+
mapview(fp$wp,zcol = "altitude",lwd=1,cex=4)+
mapview(fp$oDEM)+
mapview(fp$fA,color="red", alpha.regions = 0.1,lwd=1.0)+
mapview(fp$hm)+
fp$demA


## (2) typical real case scenario
##     a flight altitude BELOW 50 m is extreme
##     U have to use a high resulution DSM
##     (here simulated with a standard DEM)

fp<-makeFP(projectDir ="/home/creu/uav/test",
missionName = "test30",
surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
followSurface = TRUE,
flightAltitude = 30,
demFn = "~/dem.tif",
windCondition = 3,
uavType = "djip3",
followSurfaceRes = 5,
altFilter = .75)


## (3) use of external vector data to define the surveyArea...
##     digitize flight area using leafDraw()
##     save vectors as JS "json" or "kml" files
##     provide full filename+upper extensions!

leafDraw(preset="uav")

## assuming resulting file is named "uav.json"
## use it for planning

fp<-makeFP(projectDir="~/uav/uav/test",
                   missionName = "50m",
                   surveyArea="~/uav.json",
                   followSurface = TRUE,
                   flightAltitude = 50,
                   overlap = 0.7,
                   demFn = "~/mrbiko.tif",
                   altFilter = 3.5,
                   maxSpeed = 50,
                   windCondition = 3)

## view results

 mapview(fp$fA,color="red", alpha.regions = 0.1,lwd=0.5)+
 mapview(fp$lp,zcol = "altitude",lwd=1,cex=4)+
 mapview(fp$oDEM,color="red",cex=5)+
}
}
\author{
Chris Reudenbach
}

