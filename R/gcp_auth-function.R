#' @export
gcp_auth <- function(path = NULL) {

  #-- Load Packages
  library(googledrive)
  library(googlesheets4)
  library(bigrquery)
  library(googleCloudStorageR)

  #-- Service Account
  google_service_account <- path

  #-- Authenticate
  drive_auth(path = google_service_account)
  gs4_auth(path = google_service_account)
  bigrquery::bq_auth(path = google_service_account)
  googleCloudStorageR::gcs_auth(json_file = google_service_account)

}
