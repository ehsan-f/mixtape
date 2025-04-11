#' Load commonly used packages
#'
#' @description
#' Loads a set of commonly used R packages for data science workflows.
#' Includes packages for data manipulation, cloud services, and date handling.
#'
#' @export
mix_load_packages <- function() {

  #-- Base
  library(arrow)
  library(tidyverse)
  library(jsonlite)
  library(janitor)
  library(zoo)
  library(lubridate)
  library(httr2)
  library(devtools)

  #-- Cloud
  library(AzureStor)
  library(googledrive)
  library(googlesheets4)
  library(googleCloudStorageR)
  library(bigrquery)

}
