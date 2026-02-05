#' Authenticate with Google Cloud Platform services
#'
#' @description
#' Authenticates with multiple Google Cloud Platform services using a service account key file.
#' Sets up authentication for Google Drive, Google Sheets, BigQuery, and Google Cloud Storage.
#'
#' @param path Path to the Google service account JSON key file
#' @param auth_arrow Whether to set up Arrow authentication via environment variable (default: TRUE)
#'
#' @importFrom bigrquery bq_auth
#' @importFrom googleCloudStorageR gcs_auth
#' @importFrom googledrive drive_auth
#' @importFrom googlesheets4 gs4_auth
#' @export
gcp_auth <- function(path = NULL, auth_arrow = T) {

  #-- Service Account
  google_service_account <- path

  #-- Authenticate
  drive_auth(path = google_service_account)
  gs4_auth(path = google_service_account)
  bigrquery::bq_auth(path = google_service_account)
  googleCloudStorageR::gcs_auth(json_file = google_service_account)

  #-- Authenticate Arrow
  if (auth_arrow == T) {
    arrow_temp_creds_file <- tempfile(fileext = ".json")
    writeLines(google_service_account, arrow_temp_creds_file)
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = arrow_temp_creds_file)
  }

}
