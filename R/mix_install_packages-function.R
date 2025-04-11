#' Install common R packages
#'
#' @description
#' Installs a curated set of R packages for data science workflows.
#' Includes core packages and additional specialized packages.
#'
#' @param install_core_packages Whether to install core packages (default: TRUE)
#' @param exclude_from_core Vector of package names to exclude from core installation (optional)
#' @param install_additional_packages Whether to install additional specialized packages (default: TRUE)
#'
#' @export
mix_install_packages <- function(install_core_packages = T,
                                 exclude_from_core = NULL,
                                 install_additional_packages = T) {

  #----- Core Packages
  if (install_core_packages == T) {
    v_core_packages <- c(
      #-- Base
      'arrow', 'tidyverse', 'jsonlite', 'janitor', 'zoo', 'httr2', 'devtools', 'remotes', 'plumber', 'data.table',

      #-- Cloud
      'AzureStor', 'googledrive', 'googlesheets4', 'googleCloudStorageR', 'bigrquery', 'aws.s3',

      #-- Modelling
      'tidymodels', 'xgboost'
    )

    #-- Exclude packages from installation
    if (!is.null(exclude_from_core)) {
      v_core_packages <- v_core_packages[!(v_core_packages %in% exclude_from_core)]
    }

    #-- Install packages
    for (i in v_core_packages) {
      message('Core package - installing ', i, '...')
      install.packages(i)
    }

  }


  #----- Additional Packages
  if (install_additional_packages == T) {
    v_additional_packages <- c(
      #-- Base
      'readxl', 'writexl',

      #-- Modelling
      'caret', 'foreach', 'doParallel', 'smbinning', 'Information',
      'ROCR', 'woeBinning', 'separationplot', 'MASS', 'car', 'dummies',
      'tm', 'corpus', 'hunspell',
      # 'unbalanced',

      #-- Other
      'datapasta', 'extrafont',

      #-- Geo-spatial
      # 'sf', 'h3jsr',

      #-- Visualisation
      'plotly', 'lemon', 'flexdashboard', 'DiagrammeR', 'kableExtra'
    )

    #-- Install packages
    #-- Install packages
    for (i in v_additional_packages) {
      message('Additional packages - installing ', i, '...')
      install.packages(i)
    }


    #-- Special installations
    remotes::install_github('rstudio/rmarkdown')
  }

}
