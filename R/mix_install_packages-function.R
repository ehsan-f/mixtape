#' @export
mix_install_packages <- function() {
  #----- Base
  install.packages('httr')
  install.packages('devtools')
  install.packages('httpuv')
  install.packages("htmltools")

  install.packages('tidyverse')
  install.packages('DBI')
  install.packages('arrow')

  install.packages('bigrquery')
  install.packages('googlesheets4')
  install.packages('googledrive')
  install.packages('googleCloudStorageR')
  install.packages('aws.s3')

  install.packages('lubridate')
  install.packages('zoo')

  #----- Other
  install.packages('foreach')
  install.packages('readxl')
  install.packages('writexl')
  install.packages('readr')
  install.packages('data.table')
  install.packages('doParallel')
  install.packages('lemon')

  install.packages('plotly')
  install.packages('tm')
  install.packages('corpus')
  install.packages('hunspell')
  install.packages('DiagrammeR')

  install.packages('datapasta')
  install.packages('janitor')


  install.packages('purrr')
  install.packages('sf')
  install.packages('h3jsr')

  # install.packages('RODBC')
  # install.packages('plyr')
  # install.packages('extrafont')

  #----- Modelling
  install.packages('MASS')
  install.packages('car')
  install.packages('unbalanced')
  install.packages('smbinning')
  install.packages('separationplot')
  install.packages('woeBinning')
  install.packages('Information')
  install.packages('xgboost')
  install.packages('dummies')
  install.packages('caret')
  install.packages('ROCR')

  # install.packages('imputeTS')

  # Install Caret from Github
  # httr::set_config(httr::config( ssl_verifypeer = 0L ) )
  # devtools::install_github('topepo/caret/pkg/caret')

  #----- Markdowns
  devtools::install_github('rstudio/rmarkdown')
  install.packages('flexdashboard')

}
