#' Install common R packages
#'
#' @description
#' Installs a curated set of R packages for data science workflows.
#'
#' @export
mix_r_setup <- function() {

  #----- Core Packages
  v_core_packages <- c(
    #-- Base
    'arrow', 'tidyverse', 'data.table',
    'jsonlite', 'janitor', 'zoo',
    'httr2', 'devtools', 'remotes', 'plumber', 'usethis',

    #-- Cloud
    'AzureStor', 'googledrive', 'googlesheets4', 'googleCloudStorageR', 'bigrquery', 'aws.s3',

    #-- Modelling
    'tidymodels', 'xgboost'
  )

  install.packages(v_core_packages)

}
