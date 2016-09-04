#' cleanSpeciesDf is a prototype function to parse a species lists
#'
#' @description cleanSpeciesDf is a function to clean a dataframe as provided by
#'   the enthusiasts (coleopterists) of the beetle community.

#'
#'   It uses the full power of df manipulation in R and is way (about 35 times ;) faster than the parseSpeciesList approach
#'
#' The resulting dataframe is a not normalized relation ( so it means a huge table with mostly redundant informations).
#'
#'   It looks like:
#'
#'   familiy;   genus;   subgenus; country[1];country[2];country[n];...\cr
#'   alpestris; STAPHYLINIDAE; Quedius; Raphirus;0;1;1;...\cr
#'   aereipennis; STAPHYLINIDAE; Quedius; Raphirus;1;0;1;...\cr
#'   .
#'   .
#'   .
#'
#' @param inputTXT a Text of the specified format
#'
#'
#'@author Jonas Hagge
#'
#'@references Loebl, I. & A. Smetana (eds): Catalogue of Palaearctic Coleoptera: \url{http://www.apollobooks.com/palaearcticcoleoptera.htm}
#'
#'@examples
#'  ### first the basic parsing
#'  inputFile <- system.file("extdata", "species.chunk",   package="parseSpeciesList")
#'  df <- cleanSpeciesDf(inputFile)
#'
#'@export
#'@name cleanSpeciesDf
#'@rdname cleanSpeciesDf
#'

cleanSpeciesDf<- function (inputFile) {
############################################################
## Step 01
## Überführen der Informationen des
## "Catalogue of Palaearctic Coleoptera" in eine Dataframe
## Stand: 26.10.2015 19:43
## FM Makroökologie SoSe 2015
############################################################

## Löscht alle Objekt die im R Environment erstellt wurden
rm(list = ls())

## Daten einlesen (erstmal Skript mit kleinem Testdatensatz erstellen)
#input <- read.delim("example.txt", header = F)
input <- read.delim(inputFile, header = F)

## Aufspalten der Zeilen: Zu welchem taxonimichen Level gibt die Zeile Aussagen?

# Subfamily --> da nicht einheiltich für alle Arten vorliegt, werden alle Zeilen gelöscht
rows_subfam <- grep("subfamily", input$V1)
input <- as.data.frame(input[-c(rows_subfam),])
colnames(input) <- "V1"

# Tribus --> da nicht einheiltich für alle Arten vorliegt, werden alle Zeilen gelöscht
rows_tribe <- grep("tribe", input$V1)
input <- as.data.frame(input[-c(rows_tribe),])
colnames(input) <- "V1"

# Zeilen mit Familien
rows_fam <- grep("family", input$V1)

# Zeilen mit Subgenus
rows_subgen <- grep("subgenus", input$V1)

# Zeilen mit Genus
#rows_gen <- grep("genus", input$V1)[!grep("genus", input$V1) %in% rows_subgen]
rows_gen <- grep("^genus", input$V1)

## Species Spalte
data_spe <- as.vector(input[-c(rows_fam, rows_gen, rows_subgen),])
df <- data.frame(substring(data_spe, 0, regexpr(" ", data_spe)-1))
colnames(df) <- "species"

## Location Spalte
df$loc <- substring(data_spe, regexpr("  ", data_spe)+2, length(data_spe))

## Familien Spalte
data_fam <- as.vector(input[rows_fam,])
data_fam <- substring(data_fam, 8, nchar(data_fam))
data_fam <- substring(data_fam, 0, regexpr(" ", data_fam)-1)
data_fam_spe <- as.data.frame(input[-c(rows_gen, rows_subgen),])
rows_fam_data <- grep("family", data_fam_spe[,1])
df$family <- rep(data_fam, diff(c(rows_fam_data, nrow(data_fam_spe)+1))-1)

## Genus Spalte
data_gen <- as.vector(input[rows_gen,])
data_gen <- substring(data_gen, 7, nchar(data_gen))
data_gen <- substring(data_gen, 0, regexpr(" ", data_gen)-1)
data_gen_spe <- as.data.frame(input[-c(rows_fam, rows_subgen),])
rows_gen_data <- grep("^genus", data_gen_spe[,1])
df$genus <- rep(data_gen, diff(c(rows_gen_data, nrow(data_gen_spe)+1))-1)

## Subgenus Spalte
data_subgen <- as.vector(input[rows_subgen,])
data_subgen <- substring(data_subgen, 10, nchar(data_subgen))
data_subgen <- substring(data_subgen, 0, regexpr(" ", data_subgen)-1)
data_subgen_spe <- as.data.frame(input[-c(rows_fam, rows_gen),])
rows_subgen_data <- grep("subgenus", data_subgen_spe[,1])
df$subgenus <- rep(data_subgen, diff(c(rows_subgen_data, nrow(data_subgen_spe)+1))-1)

## Anordnung der Spalten
df <- df[,c("family","genus","subgenus","species","loc")]

## Aufspalten der Location Spalte
# Gerde nur eine Auswahl --> Gesamtlist sollte aus Buch übernommen werden

country_code <- read.csv2("inst/extdata/countrylist_clean.csv") # Vorher kurz in Exel bereinigt
country_code$loc_code <- substring(country_code$Ccode.Country, 0, regexpr(" ", country_code$Ccode.Country)-1)
country_code$loc_desc <- substring(country_code$Ccode.Country, regexpr(" ", country_code$Ccode.Country)+1, nchar(as.character(country_code$Ccode.Country)))
country_code <- country_code[,-1]

#loc_list <- c("AU", "BY", "CT", "CZ", "DE", "EN", "FI", "FR", "GB", "GE", "IT", "IR", "LA", "NL", "NR", "NT", "PL", "SK", "SV", "SZ")
loc_list <- country_code$loc_code

for (i in 1: length(loc_list)) {
   df[loc_list[i]]<- rep(0, nrow(df))
}

for (i in 1: length(loc_list)) {
  df[grep(loc_list[i], df$loc), loc_list[i]] <- 1
  grep("E", df$loc)
}

## Ausschreiben der Rohdaten für weiter Analyseschritte
df <- df[,-c(5)]
write.csv2(df, "cpc_vol_2.csv")

sort(colSums(df[,5:150]), decreasing = TRUE)
return(df)
}