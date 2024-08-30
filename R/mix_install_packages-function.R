#' @export
mix_install_packages <- function(install_core_packages = T,
                                 exclude_from_core = NULL,
                                 install_additional_packages = T) {

  #----- Core Packages
  if (install_core_packages == T) {
    v_core_packages <- c(
      #-- Base
      'arrow', 'tidyverse', 'jsonlite', 'janitor', 'zoo', 'httr2', 'devtools', 'remotes', 'plumber',

      #-- Cloud
      'googledrive', 'googlesheets4', 'googleCloudStorageR', 'bigrquery', 'aws.s3',

      #-- Modelling
      'caret', 'foreach', 'doParallel', 'smbinning', 'Information', 'xgboost'
    )

    #-- Exclude packages from installation
    if (!is.null(exclude)) {
      v_core_packages <- v_core_packages[!(v_core_packages %in% exclude_from_core)]
    }

    #-- Install packages
    install.packages(v_core_packages)
  }


  #----- Additional Packages
  if (install_additional_packages == T) {
    v_additional_packages <- c(
      #-- Base
      'data.table', 'readxl', 'writexl',

      #-- Modelling
      'ROCR', 'woeBinning', 'separationplot', 'MASS', 'car', 'dummies',
      'tm', 'corpus', 'hunspell',
      # 'unbalanced',

      #-- Other
      'datapasta', 'extrafont',

      #-- Geo-spatial
      'sf', 'h3jsr',

      #-- Visualisation
      'plotly', 'lemon', 'flexdashboard', 'DiagrammeR', 'kableExtra'
    )

    #-- Install packages
    install.packages(v_additional_packages)

    #-- Special installations
    remotes::install_github('rstudio/rmarkdown')
  }

}
