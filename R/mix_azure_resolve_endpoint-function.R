#' Resolve Azure Storage Endpoint
#'
#' @param endpoint Full Azure storage endpoint URL
#' @param storage_key Azure storage account key (optional)
#' @param token Azure authentication token object (optional)
#'
#' @return An AzureStor storage endpoint object
#' @keywords internal
#' @noRd
mix_azure_resolve_endpoint <- function(endpoint, storage_key = NULL, token = NULL) {
  if (!is.null(storage_key)) {
    if (!is.character(storage_key) || length(storage_key) != 1L) {
      stop('storage_key must be a length-1 character string or NULL')
    }
    if (!is.na(storage_key) && nzchar(storage_key)) {
      return(AzureStor::storage_endpoint(endpoint = endpoint, key = storage_key))
    }
  }

  if (!is.null(token)) {
    return(AzureStor::storage_endpoint(endpoint = endpoint, token = token))
  }

  resolved_token <- mix_azure_get_token()
  AzureStor::storage_endpoint(endpoint = endpoint, token = resolved_token)
}
