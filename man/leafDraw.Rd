% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leafDraw.R
\name{leafDraw}
\alias{leafDraw}
\title{digitizing vector features using leaflet draw}
\usage{
leafDraw(mapCenter = c(50.80801, 8.72993), zoom = 15, line = TRUE,
  rectangle = TRUE, poly = TRUE, circle = TRUE, point = TRUE,
  remove = TRUE, position = "topright", maplayer = c("CartoDB.Positron",
  "OpenStreetMap", "Esri.WorldImagery", "Thunderforest.Landscape",
  "OpenTopoMap"), overlay = NULL, preset = "all", cex = 10, lwd = 2,
  alpha = 0.6, opacity = 0.7)
}
\arguments{
\item{zoom}{initial zoom level}

\item{line}{enable the draw tool line tool}

\item{poly}{enable the draw polygon tool}

\item{circle}{enable the draw circle tool}

\item{point}{enable the draw point tool}

\item{remove}{enable/disable the remove feature of the draw tool}

\item{position}{place to put the toolbar (topright, topleft, bottomright, bottomleft)}

\item{maplayer}{string as provided by leaflet-provider}

\item{overlay}{optional sp object}

\item{preset}{textstring "NULL" full draw version, "uav" for flightarea digitizing, "ext" for rectangles}

\item{mapcenter}{c(lat,lon) central point of the leaflet map}

\item{intersection}{enable/disable th possibility to overlay lines or polygons}
}
\description{
leafDraw is based on the leaflet draw plugin. It provides a bunch of leaflet maps as base layers for digitizing vector features.
}
\note{
Yu can either save the digitized object to a json file or you ma grab the json string via the clipboard
}
\examples{

# all features
leafDraw()

# preset for digitizing uav flight areas
leafDraw(preset="uav")

#' # preset for digitizing extents
leafDraw(preset="ext")
}
\author{
Chris Reudenbach
}

