#' parseCountrycodes is a helper function to parse a modiefied iso2 country code as provided by the beatle list

#'
#'@description parseCountrycodes parses the assuminly iso2 country code list of the running beetle species linestring. It provides a reionally E: N: A: sorted list of country coded occurency
#'   .
#'
#'@param inLine a textstring of the specified format
#'
#'@author Chris Reudenbach
#'
#'@examples
#'  ### examples parseCountryCode ###
#'
#'  inLine='nubilus Fabricius, 1777: 213  E: AU BH BU BY CR CT CZ DE EN FI FR GB GE GR HU IR IT LA LT MC NL NR NT PL PT RO SK SL SP ST SV SZ TR UK YU "Transcaucasus"  A: IN TR'
#'  countryList<-parseCountryCode(inLine)
#'
#'@export
#'@name parseCountryCode
#'@rdname parseCountryCode
#'




parseCountryCode <- function (intext,x) {
  # initialize the vars
  x = list()
  aloc = ''
  eloc = ''
  nloc = ''
  # get position of the differnt location tags in the string
  ePos <-  stringr::str_locate_all(pattern = ' E: ',intext)[[1]][2] - 4
  aPos <-  stringr::str_locate_all(pattern = ' A: ',intext)[[1]][2] - 4
  nPos <-  stringr::str_locate_all(pattern = ' N: ',intext)[[1]][2] - 4
  # ugly workaround because of totally chotic organization of the location codes
  # and tags we have to derive a fixed order that we will be able to point on
  # the correct labels after extracting them. i did it using a dataframe.
  # finally we have a ordered list and we will submit the tags with the return
  names <- c("a", "e", "n")
  values <- c(aPos,ePos,nPos)
  df <- data.frame(names,values)
  df <- df[order(values),]
  df <- df[complete.cases(df),]
  # if there is a location tag
  if (nrow(df) > 0) {
    for (i in seq(1,nrow(df))) {
      z <- df$values[i + 1]

      if (i == nrow(df)) {
        z <- nchar(intext)
      }
      var <- trimws(substring(intext,df$values[i] + 2 ,z + 1))
      x[i] <- var #paste0(df$names[i],'loc')
      assign(paste0(df$names[i],'loc'), var)
    }
  }
  c(x, c = aloc)
  c(x, c = eloc)
  c(x, c = nloc)

  return(x)
}
