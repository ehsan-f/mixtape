#' Get Azure authentication token
#'
#' @description
#' Resolves an Azure authentication token for use with AzureStor functions.
#' First attempts managed identity authentication (suitable for Azure resources
#' like Container Apps with managed identity assigned). If `AZURE_CLIENT_ID` is
#' set to a non-empty value, it is forwarded to the managed identity request to
#' disambiguate between multiple attached identities. If managed identity fails,
#' falls back to interactive authentication using the authorization code flow.
#'
#' @param resource Azure resource URL for which to request the token (default: 'https://storage.azure.com/')
#' @param tenant_id Azure tenant ID. If not supplied, reads from AZURE_TENANT_ID environment variable.
#' @param app_id Azure application ID (client ID). If not supplied, reads from AZURE_CLIENT_ID environment variable.
#' @param is_interactive (For testing only) whether the session is interactive. Defaults to interactive().
#'
#' @return An AzureAuth token object (suitable for use with AzureStor::storage_endpoint(..., token = ...))
#'
#' @details
#' This function attempts two authentication methods in sequence:
#' \enumerate{
#'   \item Managed Identity: For Azure Container Apps/App Service identity protocol
#'         (detected via `IDENTITY_ENDPOINT` + `IDENTITY_HEADER`), directly calls
#'         the local identity endpoint using the required `X-IDENTITY-HEADER`,
#'         then wraps the access token in `AzureAuth::AzureManualToken`.
#'         Otherwise, falls back to `AzureAuth::get_managed_token()` (classic IMDS path).
#'         When `AZURE_CLIENT_ID` is set to a non-empty value, it is passed as
#'         `client_id` to disambiguate between multiple attached identities.
#'   \item Interactive (fallback): If managed identity fails, falls back to
#'         AzureAuth::get_azure_token() with authorization code flow for local development.
#'         AzureAuth caches the token, so subsequent calls in the same or later sessions
#'         reuse the cached token rather than prompting again.
#' }
#'
#' @export
mix_azure_get_token <- function(resource = 'https://storage.azure.com/',
                                tenant_id = NULL,
                                app_id = NULL,
                                is_interactive = interactive()) {

  if (!requireNamespace('AzureAuth', quietly = TRUE)) {
    stop(
      'The AzureAuth package is required for token-based authentication but is not installed. ',
      'Install it with: install.packages("AzureAuth")'
    )
  }

  managed_identity_error <- NULL

  # Try managed identity first (does not require tenant_id/app_id)
  token <- tryCatch(
    {
      if (is_interactive) message('Attempting managed identity authentication...')
      identity_endpoint <- Sys.getenv('IDENTITY_ENDPOINT', unset = NA_character_)
      identity_header <- Sys.getenv('IDENTITY_HEADER', unset = NA_character_)
      mi_client_id <- Sys.getenv('AZURE_CLIENT_ID', unset = NA_character_)

      if (!is.na(identity_endpoint) && nzchar(identity_endpoint) &&
          !is.na(identity_header) && nzchar(identity_header)) {
        request <- httr2::request(identity_endpoint) |>
          httr2::req_url_query(
            resource = resource,
            `api-version` = '2019-08-01'
          ) |>
          httr2::req_headers(`X-IDENTITY-HEADER` = identity_header)

        if (!is.na(mi_client_id) && nzchar(mi_client_id)) {
          request <- httr2::req_url_query(request, client_id = mi_client_id)
        }

        response <- httr2::req_perform(request)
        status <- httr2::resp_status(response)
        response_body <- tryCatch(
          httr2::resp_body_string(response),
          error = function(e) stop('Managed identity token response body could not be read: ', conditionMessage(e))
        )

        if (status != 200) {
          stop('Managed identity token request failed with status ', status, ': ', response_body)
        }

        token_response <- tryCatch(
          jsonlite::fromJSON(response_body),
          error = function(...) list()
        )
        access_token <- token_response$access_token
        token_type <- token_response$token_type
        token_tenant <- token_response$tenant
        if (is.null(access_token) || !nzchar(access_token)) {
          stop('Managed identity token response did not include access_token')
        }
        if (is.null(token_type) || !nzchar(token_type)) {
          token_type <- 'Bearer'
        }
        if (is.null(token_tenant) || !nzchar(token_tenant)) {
          token_tenant <- NULL
        }

        AzureAuth::AzureManualToken$new(
          access_token,
          type = token_type,
          tenant = token_tenant,
          resource = resource
        )
      } else {
        if (!is.na(mi_client_id) && nzchar(mi_client_id)) {
          AzureAuth::get_managed_token(resource, token_args = list(client_id = mi_client_id))
        } else {
          AzureAuth::get_managed_token(resource)
        }
      }
    },
    error = function(e) {
      managed_identity_error <<- conditionMessage(e)
      if (is_interactive) message('Managed identity authentication failed: ', managed_identity_error)
      NULL
    }
  )

  # If managed identity failed, fall back to interactive auth
  if (is.null(token)) {
    if (!is_interactive) {
      stop(
        'Managed identity authentication failed and session is non-interactive. ',
        'Details: ', managed_identity_error, '. ',
        'Pass a storage_key or token to the calling function, or ensure the environment has a managed identity assigned.'
      )
    }

    # Resolve tenant_id from environment if not supplied
    if (is.null(tenant_id)) {
      tenant_id <- Sys.getenv('AZURE_TENANT_ID', unset = NA_character_)
    }
    if (is.na(tenant_id) || !nzchar(tenant_id)) {
      stop('tenant_id must be supplied or AZURE_TENANT_ID environment variable must be set')
    }

    # Resolve app_id from environment if not supplied
    if (is.null(app_id)) {
      app_id <- Sys.getenv('AZURE_CLIENT_ID', unset = NA_character_)
    }
    if (is.na(app_id) || !nzchar(app_id)) {
      stop('app_id must be supplied or AZURE_CLIENT_ID environment variable must be set')
    }

    message('Attempting interactive authentication (authorization code flow)...')
    token <- tryCatch(
      AzureAuth::get_azure_token(
        resource = resource,
        tenant = tenant_id,
        app = app_id,
        auth_type = 'authorization_code'
      ),
      error = function(e) {
        stop('Both managed identity and interactive authentication failed. ',
             'Last error: ', conditionMessage(e))
      }
    )
  }

  return(token)
}
