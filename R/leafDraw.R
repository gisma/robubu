#' digitizing vector features using leaflet draw
#'
#' @description  leafDraw is based on the leaflet draw plugin. It provides a bunch of leaflet maps as base layers for digitizing vector features. 
#'
#' @note Yu can either save the digitized object to a json file or you ma grab the json string via the clipboard
#' @param mapcenter c(lat,lon) central point of the leaflet map
#' @param zoom initial zoom level
#' @param line enable the draw tool line tool
#' @param poly enable the draw polygon tool 
#' @param circle enable the draw circle tool
#' @param point enable the draw point tool
#' @param remove enable/disable the remove feature of the draw tool
#' @param position place to put the toolbar (topright, topleft, bottomright, bottomleft)
#' @param intersection enable/disable th possibility to overlay lines or polygons
#' @param maplayer string as provided by leaflet-provider 
#' @param preset textstring "NULL" full draw version, "uav" for flightarea digitizing, "ext" for rectangles
#' 
#' @author
#' Chris Reudenbach
#'
#' @examples
#' 
#' #most basic and simple with all features
#' leafDraw()
#' 
#' # preset for digitizing uav flight areas
#' leafDraw(preset="uav")
#' 
#' #' # preset for digitizing extents
#' leafDraw(preset="ext")
#' @export leafDraw
#'               

leafDraw <- function(mapCenter=c(50.80801,8.72993),
                     zoom=15, 
                     line = TRUE, rectangle = TRUE, poly = TRUE, circle = TRUE, point = TRUE,
                     remove = TRUE, position= "topright", 
                     maplayer=c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap"),
                     overlay=NULL, preset = "all") {
  
  if ( preset == "uav") {
    if (is.null(mapCenter)){
      mapCenter<-c(50.80801,8.72993)}
    else {
      mapCenter<-mapCenter
    }
    zoom<-15 
    line<-TRUE
    rectangle<-FALSE
    poly<-FALSE
    circle<-FALSE
    point<-FALSE
    remove<-TRUE
    maplayer=c("Esri.WorldImagery","OpenStreetMap","Thunderforest.Landscape","OpenTopoMap")
    overlay=NULL
  } 
  else if (preset == "ext") {
    mapCenter<-c(50.80801,8.72993)
    zoom<-10
    line<-FALSE
    rectangle<-TRUE
    poly<-FALSE
    circle<-FALSE
    point<-FALSE
    remove<-FALSE   
    position<-"topright"
    maplayer=c("OpenStreetMap","CartoDB.Positron","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap")
    overlay=NULL
  } else {
    mapCenter<-mapCenter
    zoom<-zoom
    line<-line
    maplayer=maplayer
    overlay=overlay
    rectangle<-rectangle
    poly<-poly
    circle<-circle
    point<-point
    remove<-remove
    position<-position
  }
  
  # create tmp path
  tmpPath<- createTempDataTransfer()
  
  ### create the rest of the JS strings
  CRSvarMapCenter<-paste0('var mapCenter = [',mapCenter[1],',',mapCenter[2],'];')
  CRSinitialZoom<-paste('var initialZoom = ',zoom,';')
  
  ### write it to CRS.js
  # assign tmpfilename for CRS definition
  tmpCRS <- paste0(tmpPath,"/crs.js")
  # write the proj4leaflet CRS
  write(CRSinitialZoom,tmpCRS,append = TRUE)
  write(CRSvarMapCenter,tmpCRS,append = TRUE)
  
  # create parameter list for the widget
  x <- list(data  = 'undefined',
            layer=maplayer,
            overlayLayer=overlay,
            zoom = zoom,
            #refpoint=refpoint,
            line=line,
            rectangle=rectangle,
            poly=poly,
            circle=circle,
            point=point,
            remove=remove,
            position=position,
            scaleBar=TRUE
  )
  leafDrawInternal(tmpPath, x = x)  
}


# create dependencies
digiDependencies <- function(tmpPath) {
  
  data_dir <- paste0(tmpPath,sep=.Platform$file.sep)
  
  
  list(
    htmltools::htmlDependency(name = "crs",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("crs.js")),
    
    htmltools::htmlDependency(name = "jsondata",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("jsondata")),
    
    htmltools::htmlDependency(
      name = "leaflet-draw",
      version= "0.7.3",
      src = c(file = tmpPath),
      script = list("leaflet.draw.js"),
      stylesheet=list("leaflet.draw.css")
    )
    
  )
}

###  creates temporary file structure for data transfer =================================================
createTempDataTransfer <- function (){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  return(tmpPath)
}

leafDrawInternal <- function(tmpPath, x = NULL) {
  deps<-digiDependencies(tmpPath) 
  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'leafDraw',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
    package = 'robubu'
  )
}

### Widget output function for use in Shiny =================================================
#
leafDrawOutput <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'leafDraw', width, height, package = 'robubu')
}

### Widget render function for use in Shiny =================================================
#
renderleafDraw<- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, projViewOutput, env, quoted = TRUE)
}