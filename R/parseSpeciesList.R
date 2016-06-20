#' parseSpeciesList is a prototype function to parse a species lists
#'
#' @description parseSpeciesList is a first prototype to parse beetle
#'   species lists as provided by the enthusiasts (coleopterists) of the beetle community.
#'
#'   This is a very raw and simple approach and due to parsing the text line by line not really
#'   in R style.
#'
#'   Unfortunately it has to be performed line by line because some keywords are missing and the rules are not always matching.
#'   So in a first try we use the "family", "genus" and "subgenus" as keywords. They are always placed in the beginning of a
#'   line. After "genus" or "subgenus" there is a unique line for each single species.
#'   In the species textline we will find a more or less systematic list of country
#'   codes that indicate all countries with known occurrence of this special species.
#'
#'   The resulting dataframe is a not normalized relation ( so it means a huge table with mostly redundant informations).
#'
#'   It looks like:
#'
#'   familiy;   genus;   subgenus;  species;     loctype; country\cr
#'   Carabidae; Carabus; Carabinae; irregularis; A:;      GE\cr
#'   Carabidae; Carabus; Carabinae; irregularis; N:;      CZ\cr
#'   .
#'   .
#'   .
#'
#'
#' @param inputTXT a Text of the specified format
#' @param short logical parameter if TRUE (default) the function trys to get only the names and country codes. If FALSE the full text
#'   will put in the data frame.
#'
#' @author Chris Reudenbach, Flo Detsch
#'
#'@references Löbl, I. & A. Smetana (eds): Catalogue of Palaearctic Coleoptera: \url{http://www.apollobooks.com/palaearcticcoleoptera.htm}

#' @examples
#'  ### examples parseSpeciesList ###
#'
#'  ### we need the stringr lib
#'  library(stringr)
#'  library(foreach)
#'
#'  ### first the basic parsing
#'  inputFile <- system.file("extdata", "species.chunk",   package="parseSpeciesList")
#'  df <- getspecies(inputFile)
#'
#'  ### all entries only for CZ
#'  cz<- subset(df, df$loc =='CZ')
#'
#'  ### all entries for porculus
#'  porculus<- subset(df, (df$species =="porculus"))
#'
#'  ######################################
#'  ###  now a basic mapping example  ####
#'
#'  ###  we need some more libs ;)
#'    if (!require(devtools)) {install.packages("devtools")} # for installation from github
#'    if (!require(maptools)) {install.packages("maptools")} # for read shapes
#'    if (!require(sp)) {install.packages("sp")}             # for manipulationg spatial data sp objects
#'    library(devtools)
#'    library(maptools)
#'    library(sp)
#'    if (!require(mapview)) {install_github("environmentalinformatics-marburg/mapview")}
#'    library(mapview)  # for modern mapping
#'
#'    ###  load prepared mapdata (source: http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip)
#'    load("data/world.Rdata")
#'
#'    ### now all findings of porculus (whatever it is ;))
#'    porculus<- subset(df, (df$species =="porculus"))
#'
#'    ### join the world countries to our data
#'    ### (iso2 fit most but there is no Code for the regions)
#'    joinSpdf <- joinData2Map(
#'    porculus
#'      , nameMap = sPDF
#'      , nameJoinIDMap = "ISO2"
#'      , nameJoinColumnData = "loc")
#'
#'    #### no we have to  project it
#'    proj4string(joinSpdf) <- CRS("+init=epsg:4326")
#'
#'    ### plot it with e.g. mapview (and have some colors and interactivity)
#'    mapView(joinSpdf,zcol="species")
#'
#' @export
#' @name getspecies
#' @rdname getspecies
#'




# main function parse the text and provides a first raw dataframe
getspecies <- function (inputFile, short = TRUE) {

  lns <- readLines(inputFile)
  df <- data.frame(matrix(ncol = 6))
  names(df) <- c("family", "genus", "subgenus", "species", "loctype", "loc")

  # casually they seem not to exist
  fam <- gen <- subgen <- species <- loc <- sloc <- NA

  lst_all <- foreach(i = 1:length(lns) ) %do%  {

    oneLine <- lns[i]

    ## family
    if (charmatch("family",oneLine ,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "family"))
      if (short) {tmp<- strsplit(tmp, ",")
                  fam <- substring(trimws(tmp[[2]][1][1]), 0, regexpr(" ", trimws(tmp[[2]][1][1]))-1)}
      else       {fam <- trimws(tmp[[2]])}

      ## genus
    } else if (charmatch("genus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "genus"))
      if (short) {tmp<- strsplit(tmp, ",")
      gen <- substring(trimws(tmp[[2]][1][1]), 0, regexpr(" ", trimws(tmp[[2]][1][1]))-1)}
      else       {gen <- trimws(tmp[[2]])}

      ## subgenus
    } else if (charmatch("subgenus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "subgenus"))
      if (short) {tmp<- strsplit(tmp, ",")
      subgen <- substring(trimws(tmp[[2]][1][1]), 0, regexpr(" ", trimws(tmp[[2]][1][1]))-1)}
      else       {subgen <- trimws(tmp[[2]])}

      ## everything else
    } else {
      # all lines without keywords has to contain species

      species <- oneLine

      # so we call a special parser for them
      loc <- parseCountryCode(species)

      if (short) {
        species <- substring(trimws(species), 0, regexpr(" ", trimws(species))-1)
      } else {
          if (length(loc) > 0) {
            for (z in 1:length(loc))
              species <- gsub(loc[z], "", species)
          }
        }


      # then we reorganise the returned lists
      if (length(loc) > 0) {
        lst_loc <- lapply(seq(1,length(loc)), function(h) {
          sloc <- unlist(strsplit(loc[[h]], " "))
          if (length(sloc) > 0) {
            lst_sloc <- lapply(seq(2,length(sloc)), function(j) {
              data.frame(loctype = sloc[1], loc = sloc[j])
            })
            dat_sloc <- do.call("rbind", lst_sloc)
          }
        })
        dat_loc <- do.call("rbind", lst_loc)
      } else {
        dat_loc <- data.frame(loctype = NA, loc = NA)
      }
    }

    if (!exists("dat_loc"))
      dat_loc <- data.frame(loctype = NA, loc = NA)

    dat <- data.frame(family = fam,
                      genus = gen,
                      subgenus = subgen,
                      species = species,
                      dat_loc)

    rm(dat_loc)
    return(dat)
  }

  dat_all <- do.call("rbind", lst_all)
  dat_all <- dat_all[complete.cases(dat_all),]
  return(dat_all)

}