#' Authenticate with Google Cloud Platform services
#'
#' @description
#' Authenticates with multiple Google Cloud Platform services using a service account key file.
#' Sets up authentication for Google Drive, Google Sheets, BigQuery, and Google Cloud Storage.
#'
#' @param path Path to the Google service account JSON key file
#'
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
