if (!isGeneric('makeTile')) {
  setGeneric('makeTile', function(x, ...)
    standardGeneric('makeTile'))
}

#'Create a leaflet conform set of raster tiles from a GDAL/raster input file
#'
#'@description makeTile creates from a GDAL raster file a leaflet compatible
#'  tile structure in a given directory.  Additionally it produces a correct
#'  map.types list for the use with \link{projView}
#'
#'@usage makeTile(x = NULL, outPath = NULL, scalec = (0,8848), s_epsg =NULL ,
#'  t_srs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0
#'  +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs", t_epsg =
#'  "EPSG:3857", rType = "average", attribution = "to be done", cLat = NULL,
#'  cLon = NULL, zoom = NULL, res = NULL, srvrUrl = "http://localhost:4321/")
#'
#'@param x character GDAL raster file name or a \code{\link{raster*}} object.
#'@param outPath character path where the tiles will be generated. Note it is always
#'  extented by \code{tiles/}
#'@param scale  numeric. (c(src_min,src_max,dst_min,dst_max)). Rescale the input
#'  pixels values from the range src_min to src_max to the range dst_min to
#'  dst_max. \code{\link{gdal_translate}}.
#'@param s_epsg character source proj4 string
#'@param t_srs character target proj4 string
#'@param t_epsg character target EPSG code
#'@param rType  character resampling method for \code{\href{http://www.gdal.org/gdal2tiles.html}{gdal2tile}} and \code{\link{gdal_translate}} (average,near,bilinear,cubic,cubicspline,lanczos,antialias) - default 'average'
#'#'@param attribution charcter attribution of the map default is "still to be
#'  done"
#'@param cLat numeric center of Latitude of the leaflet map object has to be in
#'  decimal degrees
#'@param cLon numeric center of Longitude of the leaflet map object has to be in
#'  decimal degrees
#'@param zoom numeric if you want to override the automatic calculation of
#'  maxZoom you can provide here a value up to 21
#'@param res numeric if you want to override the automatic calculation of the
#'  resolution you can  provide a predifined list like c(512,256,128)
#'@param srvrUrl character the root address of the locale http daemon default
#'  see \link{details}, \link{seealso}
#'@param GDALFormat character see
#'  \href{http://www.gdal.org/formats_list.html}{GDAL formats}
#'
#'@details The integration of local tiles in leaflet is first of all
#'  straightforward. You can easyly generate them with
#'  \href{http://www.gdal.org/gdal2tiles.html}{gdal2tyles}. Actually the
#'  \href{https://github.com/commenthol/gdal2tiles-leaflet}{modified version} of
#'  \href{https://github.com/commenthol}{commenthol} is used, that has some
#'  leaflet specific arguments and engages all available cpus for tiling.
#'
#'  The basic concept is focused on piping the result to \link{projView}. That
#'  means for each tile a nested map.types list is generated. For convenience
#'  reasons this list is also saved in the root directory of the tiles for later
#'  use. For more information about this list and serving have a look at online
#'  help of \href{http://gisma.github.io/projView/projView1_0_9.html}{projView}
#'
#'  Nevertheless one has to take care of some pitfalls.\cr
#'
#'  For the first it seems to be more efficient to override the concept of
#'  \link{createWidget} which copys all necessary files of a htmlwidget instance
#'  to the temporary path and then serves location with the the build in http
#'  daemon of RStudio. Imagine if you just have 6-8 SRTM tiles you need to copy
#'  about 1-2 GB of tiles to the temporary directory. An easy solution is to use
#'  a seperate http daemon as \link{httd} from the package servr.\cr\cr
#'
#' The physical generation of raster tiles is with the help of the gdal tools
#' fairly simple. Nevertheless it is a bit tricky to glue the raster tiles at
#' the correct position in the map frame because Leaflet is made for mapping of
#' seamless and continuously existing tiles like the OSM ones.\cr\cr
#'  Owing to this concept you have to  rescale each raster on the given canvas of the browser.
#' Additionally there you must define the zoom levels which you want to use. Unlike the OSM tiles that
#'  have a defined resolution, the local raster files usually have a generic pixel resolution
#'  as a function of the sensor and the post-processing. To meet the 256 pixel tiling
#'  concept this original resolution has to be scaled to the tiles.\cr \cr
#'  In the end maketile uses the original or reprojected resolution of the pixel  as reference for the
#' maximum  zoom level which is synchronized with the OSM zoom levels. Therefore the pixel resolution
#'  depends on (1) the original and/or (2) the projected resolution. \cr To keep
#'  overlays and raster tiles geometrically in line, the leaflet "bounds" settings
#'  is applied strictly, so you sometimes you may have to zoom some levels into
#'  or pan around the map  to see the background tiles ...
#'
#'
#'
#'@seealso
#'
#'NOTE: tile serving usually would work from the local file system without
#'engaging a http daemon. Due to security issues this scenario is a pretty
#'complex topic. To do soo you have to override the default security properties
#'of your browser wich is NOT encouraged. See
#'\href{https://en.wikipedia.org/wiki/JavaScript#Security}{Wikipedia} for some
#'basic expalanation. You may override this setting (here for
#'\href{http://www.chrome-allow-file-access-from-file.com/}{chrome/chromium}),
#'but again this is a first order security vulneration.\cr So it is strictly
#'recommended to use a local http server to provide full access to local tiles
#'and files. There are several solutions within R. Most easiest way to do so is
#'the package \code{\link{httd}}.\cr Nevertheless it is much more convenient to
#'install seperatly a http daemon. If you are not used to the topic the
#'\href{http://twistedmatrix.com}{twistd} daemon  is a very good cross platform
#'powerful, save and simple solution. For some basic information and
#'installation advices have a look at stackoverflow
#'\href{http://stackoverflow.com/questions/12905426/what-is-a-faster-alternative-to-pythons-simplehttpserver}{simplehttpserver}.
#'
#'@author Chris Reudenbach
#'
#'@examples
#'
#'\dontrun{
#' ### we need a running gdal with all python bindings  ###
#' require(raster)
#' require("curl")
#' require(servr)
#'
#' ### Typical workflow (1) start http deamon (2) makeTile,  (3) use projView()
#'
#' ## get Denmark vector data
#'  gadmDNK <- getGeoData(name="GADM",country="DNK",level="1")
#'
#' ## get SRTM  elevation data for the Danish coast area
#'  r<-getGeoData(name="SRTM",xtent = extent(11,11,58,58), merge=TRUE)
#'
#' ## start http daemon
#'  servr::httd("~/tmp/data/makeTile/srtm",daemon=TRUE)
#'
#' ## First pseudomercator
#'  map.typesList<-makeTile(x=r,outPath="/home/creu/tmp/data/makeTile/srtm",
#'                              cLat=56.5,
#'                              cLon=11,
#'                              attribution = " <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | <a href='http://srtm.csi.cgiar.org/'>SRTM CGIAR</a> | <a href='http://www.gadm.org'>GADM</a> | <a href='http://spatialreference.org/ref/sr-org/7483/'> EPSG:3857</a>")
#'
#' ## map it
#'  mapview::projView(gadmDNK , map.types = map.typesList$layerName)
#'
#' ## define new target projection note then you must provide the source projection
#'  s_epsg<-"EPSG:4326"
#'  t_epsg<-"EPSG:32632"
#'  t_srs<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'
#' ## create tiles and parameter list
#'  map.typesList<-makeTile(x=r,outPath="/home/creu/tmp/data/makeTile/srtm",
#'                              s_epsg=s_epsg,
#'                              t_srs=t_srs,
#'                              t_epsg=t_epsg,
#'                              cLat=56.5,
#'                              cLon=11,
#'                              attribution = "<a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | <a href='http://srtm.csi.cgiar.org/'>SRTM CGIAR</a> | <a href='http://www.gadm.org'>GADM</a> | <a href='http://spatialreference.org/ref/epsg/32632/'> EPSG:32632</a>")
#'
#' ## map it
#'  mapview::projView(gadmDNK , map.types = map.typesList$layerName)
#'
#' ## stop all daemon instances
#'  servr::daemon_stop(daemon_list())
#'
#' ####### data from Quantarctica, http://www.quantarctica.org #######
#'
#' ## create persistant temp folder
#'  tmpDir<-"~/tmp/data/quantartica"
#'
#' ## get Camps data
#' data(campsQ2)
#'
#' ## choose a file
#'  fn<-paste0(tmpDir,"/Quantarctica2/Basemap/Terrain/ETOPO1_DEM.tif")
#'
#' ## define target projection
#'  s_epsg="EPSG:3031"
#'  t_epsg="EPSG:3031"
#'  t_srs="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#'
#' ## create tiles and parameter list
#'
#'  map.typesList<-makeTile(x=fn,outPath="~/tmp/data/makeTile/quantica",
#'                               s_epsg=s_epsg,
#'                               t_srs=t_srs,
#'                               t_epsg=t_epsg,
#'                               cLat=-90,
#'                               cLon=0,
#'                               attribution = "<a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | <a href='http://www.quantarctica.org'> Quantarctica </a> provided by: <a href='http://www.npolar.no/en'>Norwegian Polar Institute</a> &nbsp;| <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> |<a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>")
#'
#' ## strt http daemon
#'  servr::httd("~/tmp/data/makeTile/quantica/",daemon=TRUE)
#'
#' ## map it
#'  mapview::projView(campsQ2 , map.types = map.typesList$layerName)
#'
#' ## stop all daemon instances
#'  daemon_stop(daemon_list())
#'
#'}
#'
#'@name makeTile
#'@export makeTile
#'@rdname makeTile


library(raster)


makeTile <- function(x=NULL,
                     outPath=NULL,
                     scale=NULL,
                     #s_srs="+proj=longlat +datum=WGS84 +ellps=WGS84",
                     t_srs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs",
                     s_epsg="EPSG:4326",
                     t_epsg="EPSG:3857",
                     rType="average",
                     attribution="still to be done",
                     cLat=NULL,
                     cLon=NULL,
                     zoom=NULL,
                     res=NULL,
                     GDALFormat="JPEG",
                     srvrUrl="http://localhost:4321/"
)
{
  # creates the temporyry directory for the CRS, data and layer transfer


  tmpPath<- createTempDataPath(outPath)
  # define GDAL fpath+filename
  tmpGDAL <-paste(tmpPath, paste0("rawTile.",GDALFormat), sep=.Platform$file.sep)

  # if of class raster
  if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    if (nchar(x@file@name) < 1){
      raster::writeRaster(x,paste0(outPath,"/tile.tif"),overwrite =TRUE)
      fn <-paste0(outPath,"/tile.tif")
    }
    fnx<-x
    fn<-x@file@name
  } else if (nchar(x)>0){
    fnx<-raster::raster(x)
    fn<-x
  }
  else{
    cat("no idea about the input type")
    return()
  }
  if (is.null(scale)) {scale<-c(raster::minValue(fnx),raster::maxValue(fnx))}
  #nodata<- as.numeric(strsplit(gdalinfo(path.expand(x))[grep("NoData Value=",gdalinfo(path.expand(x)))], split='=', fixed=TRUE)[[1]][2])
  #nodata<- fnx@file@nodatavalue

  # if necessary project data
  if (t_epsg != s_epsg){
  gdalUtils::gdalwarp(path.expand(fn),
           path.expand(paste0(tmpPath,"/rawTile.tif")) ,
           s_srs = s_epsg,
           t_srs = t_epsg,
           r = rType,
           #srcnodata = nodata,
           dstnodata = -9999,
           multi = TRUE,
           overwrite = TRUE,
           q = TRUE
  )
    fn <- paste0(tmpPath,"/rawTile.tif")
  }

  # transform and scale to jpeg for speedup TODO better scaling better way to PNG
  rx<- gdalUtils::gdal_translate(path.expand(fn),
                      path.expand(tmpGDAL) ,
                      output_Raster = FALSE,
                      overwrite= TRUE,
                      verbose=TRUE,
                      scale=scale,
                      of=GDALFormat,
                      q = TRUE
  )
  fnTranslate<-path.expand(tmpGDAL)
  # put last raster image in fnx
  fnx<-raster::raster(fnTranslate)

  # calculate zoom level from extent of input raster
  if ( is.null(zoom)){
    zfacs<-estimateZoom(fnx,t_srs)
    zoom<-zfacs[[1]]
    #scaleFac<-zfacs[[5]]
  }

  # make tiles
  if (rType == "nearest") {rType<-"near"}
  r <- system(paste0("inst/htmlwidgets/lib/gdaltiles/gdal2tiles-multiprocess.py -l  --profile=raster -r ",rType,"  -z  0-",zoom," -s  ",t_epsg," -w all --verbose ", fnTranslate ," ", path.expand(paste0(tmpPath,"/tiles"))),intern=T)

  # generate map.type list
  map.typesList<-makeMapTypesList(outPath,s_srs,t_srs,t_epsg,fnx,zoom,attribution,cLat,cLon,srvrUrl)

  return(map.typesList)
}


# function that calculates the zoom resolution and so on
estimateZoom <- function(ext=NULL,proj4){
  if (length(grep("+proj=longlat", proj4,fixed=TRUE))>= 1){
    lola<-TRUE
  }else
  {lola <- FALSE}
  rad<-pi/180
  earth<- 6378137
  if (lola) {
    lat1<-ext@extent@ymin
    lon1<-ext@extent@xmin
    lat2<-ext@extent@ymax
    lon2<-ext@extent@xmax
    dLat <- (lat2-lat1)*rad
    dLon <- (lon2-lon1)*rad
    a <- sin(dLat/2) * sin(dLat/2) + cos(lat1*rad) * cos(lat2*rad) * sin(dLon/2) * sin(dLon/2)
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    dist <- earth * c
    #https://msdn.microsoft.com/en-us/library/bb259689.aspx
    groundResolution<- (cos(lat1* rad) * dist / ext@ncols)
    #gR<-(cos(lat1 * rad) * 2 * pi * dist) / (256 * 2 **(i-1))
    mScale =  groundResolution * 96 / 0.0254
    latL <- earth * cos(lat1*rad)
    UlatL <- 2 * latL * pi
    fulldiskPixel<-UlatL/dist*ext@ncols

  } else {
    y1<-ext@extent@ymin
    x1<-ext@extent@xmin
    y2<-ext@extent@ymax
    x2<-ext@extent@xmax
    dist<- abs(x1-x2)
    groundResolution<- dist / ext@ncols
    fulldiskPixel<- 2*earth*pi/groundResolution
  }

  # calculate native zoom of the input raster
  maxExact<-max(log2(ext@nrows/256),log2(ext@ncols/256))
  zoom<- ceiling(max(log2(ext@nrows/256),log2(ext@ncols/256)))
  #scaleFac<-zoom/maxExact
  #zoom<-ceiling(min(ext@ncols,ext@nrows)/256)
  return(c(zoom,groundResolution,dist,ext)
         )
}

### calculates the resolution and scales for the tiles and
### produces a list that can be read as input of projView
makeMapTypesList <- function(outPath=NULL,s_srs=NULL,t_srs=NULL,t_epsg=NULL,fnx=NULL,zoom=NULL,attribution,cLat,cLon,srvrUrl){

  pathtoTile<-outPath
  ts<-TRUE
  layerName<-names(fnx)
  minx<-fnx@extent@xmin
  miny<-fnx@extent@ymin
  maxx<-fnx@extent@xmax
  maxy<-fnx@extent@ymax

  # calculates generic resolution
  res<-max(maxx-minx,maxy-miny)
  #if (is.null(res)) {res<-abs(ulx) + abs(uly)}
  maxResolution <- res / 256
  resolution<- list()
  for ( i in seq(0,zoom)){
    resolution[i+1] <- maxResolution /  2^i
  }
  minRes<-as.numeric(resolution[length(resolution)])

  # create resolution string
  # resolution<-c(paste(resolution,collapse = ","))
  # LProjResolution<-paste0("[",tmpres,"]")

  # if no map centre is set calculate it
  if (is.null(cLat)||is.null(cLon)){
    # create an polygon from the xtend of the raster image
    tmpPoly = sp::Polygon(cbind(c(minx,minx,maxx,maxx,minx),c(miny,maxy,maxy,miny,miny)))
    tmpPoly = sp::Polygons(list(tmpPoly), ID = "bbox")
    bbox = sp::SpatialPolygons(list(tmpPoly))
    # if the raster was reprojected
    if (s_epsg != t_epsg) {
      # assign the target projection
      sp::proj4string(bbox) <-CRS(t_srs)
      # reproject it to latlong
      tmp<-sp::spTransform(bbox, CRS("+proj=longlat +datum=WGS84 +no_defs"))
      # get the xtent
      xt<-raster::extent(tmp)
      # get map center and extent
      xtrLL<-raster::extent(xt)
      cLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
      cLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin
    }
    else {
      sp::proj4string(bbox) <-CRS(s_srs)
      # reproject it to latlong
      tmp<-sp::spTransform(bbox, CRS("+proj=longlat +datum=WGS84 +no_defs"))
      xt<-raster::extent(bbox)
      # get map center and extent
      xtrLL<-raster::extent(xt)
      cLat <- (xtrLL@ymax-xtrLL@ymin) * 0.5  + xtrLL@ymin
      cLon <- (xtrLL@xmax-xtrLL@xmin) * 0.5 + xtrLL@xmin
    }
  } # end map center

  # to do +proj=stere +lat_0=-90 +lat_0=+90
  if (t_epsg == "EPSG:3031" || t_epsg == "EPSG:3995" ) {
    res<-max(maxx-minx,maxy-miny)
    #if (is.null(res)) {res<-abs(ulx) + abs(uly)}
    maxRes <- res / 256
    for (i in seq(1,21)) {
      tmp <-256 * 2**i
      if (tmp>minRes & ts) {
        tileSize<-tmp
        ts<-FALSE
      }
    }

    initRes<-log(256, base = 2)
    if (initRes <= 0) {initRes <-1}
    #as.numeric(resolution[length(resolution)])
    tmpres<-2^(initRes:(zoom + initRes))
    tmpres<-sort(tmpres,decreasing = TRUE)
    resolution<-tmpres

    diff <-(sqrt(abs(minx)**2+abs(miny)**2)/2)/zoom
    olx<-(minx-diff)
    oly<- (miny-diff)* (-1)

  }
  # if not polarstereographic
  else {

    olx<-minx
    oly<-maxy

    # rescale the canvas to the real tilesize
    # yes I had to think about it a while
    # finally I got the idea that this is the most
    # weird calculation of the pixel size I ever did
    # so I keep it as a monument of circular thinking
    # and take the raster function for operational use
#     minSide<-min(fnx@ncols,fnx@nrows)
#     maxSide<-max(fnx@ncols,fnx@nrows)
#     mainScale<-minSide/maxSide
#     minSide256<-8**2*256
#     tileSize<-(minSide/minSide256)*256*mainScale

    # it is called tileSize but it IS pixel resolution
    tileSize<-raster::xres(fnx)

    # now calculate the optimal pixel resolution to meet
    # the zooming scale for the available resolution
    exp<-ceiling(256/tileSize)
    initRes<- (log(256, base = 2))-exp
    if (initRes <= 0) {initRes <-0}
    tmpres<-2^(initRes:(zoom + initRes))
    tmpres<-sort(tmpres,decreasing = TRUE)
    resolution<-tmpres
    exp<-8-log(tmpres[length(tmpres)],base=2)
    # scaled pixel resolution according to the map resolution levels
    tileSize<-tileSize*2**exp
  }

  # create param list
  map.types<-list(layerName=list(service="OSM",
                                 L.tileLayer=srvrUrl,
                                 layer=list( layerName   = list("tiles","{z}/{x}/{y}")
                                 ),
                                 format="image/png",
                                 tileSize=tileSize,
                                 tms="false",
                                 minZoom=0,
                                 maxZoom=zoom,
                                 noWrap ="true",
                                 continuousworld="false",
                                 attribution=attribution,
                                 params=list(t_epsg=t_epsg,
                                             t_srs=t_srs,
                                             mapCenter=list(cLat=cLat,
                                                            cLon=cLon),
                                             initialZoom="0",
                                             zoomLevels=zoom,
                                             initialResolution=c(resolution),
                                             ovlBounds=list(minx=minx,
                                                            miny=miny,
                                                            maxx=maxx,
                                                            maxy=maxy),
                                             origin=list(olx=olx,
                                                         oly=oly),
                                             useBounds="TRUE"
                                 )) # end of list
  ) # end of total list
  save(map.types,file = paste0(pathtoTile,"/",layerName,".rda"))
  return(map.types)
}

###  creates temporary file structure for data transfer =================================================

createTempDataPath<- function (path=NULL){

  if (is.null(path)){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  }
  else{
  dir.create(path,showWarnings = FALSE,recursive = TRUE)
    tmpPath<-path
  }
  #pathFN <- paste0(tmpPath,"/",f)
  return(tmpPath)

}

