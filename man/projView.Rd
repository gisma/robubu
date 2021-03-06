% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projView.R
\name{projView}
\alias{projView}
\title{projView generates projected leaflet maps using (local) or remote tiles and arbitrary vector for obverlaying.}
\usage{
projView( x, zcol, color, na.color, map.types, cex, lwd, alpha, legend, legend.opacity, verbose, use.layer.names,layer.name,popup,internalList, externalList)
}
\arguments{
\item{x}{a \code{\link{sp}}* object}

\item{zcol}{attribute name(s) or column number(s) in attribute table
of the column(s) to be rendered}

\item{color}{color (palette) for points/polygons/lines}

\item{na.color}{color for missing values}

\item{map.types}{an obligate list of map tiles see \link{details}}

\item{cex}{attribute name(s) or column number(s) in attribute table
of the column(s) to be used for defining the size of circles}

\item{lwd}{line width}

\item{alpha}{opacity of the lines or points}

\item{legend}{should a legend be plotted}

\item{legend.opacity}{opacity of the legend}

\item{verbose}{should some details be printed during the process}

\item{use.layer.names}{should layer names of the Raster* object be used?}

\item{layer.name}{the name of the overlay layer to be shown on the map}

\item{popup}{a character vector of the HTML content for the popups. See
\code{\link{addControl}} for details.}

\item{internalList}{default is FALSE if set to TRUE it is possible to pipe a command with externalList}

\item{externalList}{list of two strings  first item is the keyword for the kind of data (currently just "arctic-nasa" is implemented, second is the R command to be evaluated. example: externalList = c("arctic-nasa","visEarthPole(groupList='1000',dateString='2014-02-04',createList = TRUE)"))}

\item{values}{a vector of values for the visualisation of the layers.
Per default these are calculated based on the supplied raster* object.}
}
\description{
projView maps existing local or online tiles in the choosen target projection. The first raster tile is taken for the default background.
}
\details{
Please note: If you use in the list ovlBounds coordinates with
 numbers that are bigger then Latitude/Longitude values would be (i.e.
 -180/+180, -90/+90), projView assumes that you are providing *correct*
 extend coordinates and will not touch them. This can be helpful if the
 transformation fails for projections that are not "common" or well described
 by an EPSG, ESRI or other code. Otherwise it construct a rectangle and
 project the coordinates towards the target system. \cr For all other
 information  please have a look at the vignette or at the online help of
 \href{http://gisma.github.io/projView/projView1_0_9.html}{projView}
}
\examples{
\dontrun{
 ##  packages
  require(mapview)
  require(raster)

 ## load data of the arctic stations
  data("campsQ2")

 ## load Greenlands roads data from geofabrik
  data("roadsGRL")

 ## We need to define online data providers. this is a bit tricky and yields sometimes just frustration...
 ## under details you'll find a link for further explanations.
 ## The map.typeList as provided contains five examples. 2 OSM and 3 WMS services
  data("map.types")

 ### finally let's start mapping

 ## map the antarctic facilities data using the NASA EarthData tiles
  projView(campsQ2, map.types= map.types$NASA)

 ## same as before but now using the visEarthPole function as a "plugin"
  projView(campsQ2, map.types= map.types$NASA,
                  internalList =TRUE,
                  externalList = c("arctic-nasa","visEarthPole(groupList='1000',dateString='2014-02-04',createList = TRUE)"))

 ### similiar job in the North

 ## map Greenland's roads using the CAFF tiles for sea and landsurface temerature
  mapview::projView(roadsGRL, map.types= map.types$CAFF)

 ## again Greenland's roads using the HAV map tiles
  mapview::projView(roadsGRL, map.types= map.types$HAV)

 ## reproject HAV and roadsGRL to EPSG:3995
  map.types$HAV$params$t_srs <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  map.types$HAV$params$t_epsg <- "EPSG:3995"
  mapview::projView(roadsGRL, map.types= map.types$HAV)

 ## move center
  map.types$HAV$params$mapCenter$cLon="70"
  map.types$HAV$params$mapCenter$cLat="15"
  mapview::projView(roadsGRL, map.types= map.types$HAV)

 ## load the list of the above example
  data("map.types")

 ## map the ArcticConnect map
  mapview::projView(roadsGRL, map.types = map.types$AC)

}
}
\author{
Chris Reudenbach
}
\references{
Online maps and tile services:\cr
NASA EarthData project: \href{https://wiki.earthdata.nasa.gov/display/GIBS}{NASA EOSDIS GIBS}\cr
Conservation of Arctic Fauna and Flora: \href{http://www.caff.is/about-caff}{CAFF}, The CAFF \href{http://dev.caff.is:8080/geoserver/web/}{Geoserver}\cr
\href{http://arcticconnect.org/}{ArcticConnect} project using their \href{http://webmap.arcticconnect.org/}{ArcticWebMap} server.\cr
Environmental maps from the Authorithy of Mecklenburg-Vorpommern \href{https://www.umweltkarten.mv-regierung.de}{L-MV}\cr
Swedish Agency for Marine and Water Management (Havs- och vattenmyndigheten): \href{https://www.havochvatten.se/kunskap-om-vara-vatten/kartor-och-geografisk-information/karttjanster.html}{HAV}\cr\cr
For the used overlay data see: \code{\link{campsQ2}}, \code{\link{roadsGRL}}\cr\cr
JS libraries:\cr
Leaflet 0.7.7: \href{http://leafletjs.com/}{Leaflet}\cr
The kartena projection plugin for leaflet: \href{http://kartena.github.io/Proj4Leaflet/}{Proj4Leaflet}\cr
}

