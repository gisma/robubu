# This file was generated by Rcpp::compileAttributes
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

gsubC <- function(pattern, replacement, x) {
    .Call('robubu_gsubC', PACKAGE = 'robubu', pattern, replacement, x)
}

brewPopupRowC <- function(colname, value) {
    .Call('robubu_brewPopupRowC', PACKAGE = 'robubu', colname, value)
}

brewPopupRowAltC <- function(colname, value) {
    .Call('robubu_brewPopupRowAltC', PACKAGE = 'robubu', colname, value)
}

brewPopupCoords <- function(colname, value) {
    .Call('robubu_brewPopupCoords', PACKAGE = 'robubu', colname, value)
}

mergePopupRows <- function(names, values) {
    .Call('robubu_mergePopupRows', PACKAGE = 'robubu', names, values)
}

createTemplate <- function(tmpPath) {
    .Call('robubu_createTemplate', PACKAGE = 'robubu', tmpPath)
}

listPopupTemplates <- function(x, names, tmpPath) {
    .Call('robubu_listPopupTemplates', PACKAGE = 'robubu', x, names, tmpPath)
}

df2String <- function(x) {
    .Call('robubu_df2String', PACKAGE = 'robubu', x)
}

one2JSON <- function(x) {
    .Call('robubu_one2JSON', PACKAGE = 'robubu', x)
}

all2JSONlist <- function(x) {
    .Call('robubu_all2JSONlist', PACKAGE = 'robubu', x)
}

