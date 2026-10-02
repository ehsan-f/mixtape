test_that("managed identity succeeds returns token directly", {
  skip_if_not_installed("AzureAuth")
  managed_token_called <- FALSE
  interactive_fallback_called <- FALSE

  local_mocked_bindings(
    get_managed_token = function(...) {
      managed_token_called <<- TRUE
      structure(list(token = "managed-identity-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    get_azure_token = function(...) {
      interactive_fallback_called <<- TRUE
      stop("Should not be called if managed identity succeeds")
    },
    .package = "AzureAuth"
  )

  result <- mix_azure_get_token()

  expect_true(managed_token_called, info = "get_managed_token should be called")
  expect_false(interactive_fallback_called, info = "get_azure_token should NOT be called if managed identity succeeds")
  expect_equal(class(result), "AzureToken")
})

test_that("managed identity passes client_id when AZURE_CLIENT_ID is set", {
  skip_if_not_installed("AzureAuth")
  withr::local_envvar(AZURE_CLIENT_ID = "managed-identity-client-id")

  local_mocked_bindings(
    get_managed_token = function(resource, token_args = NULL, ...) {
      expect_equal(resource, "https://storage.azure.com/")
      expect_equal(token_args, list(client_id = "managed-identity-client-id"))
      structure(list(token = "managed-identity-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  result <- mix_azure_get_token()

  expect_equal(class(result), "AzureToken")
})

test_that("managed identity does not pass client_id when AZURE_CLIENT_ID is empty", {
  skip_if_not_installed("AzureAuth")
  withr::local_envvar(AZURE_CLIENT_ID = "")

  local_mocked_bindings(
    get_managed_token = function(resource, token_args = NULL, ...) {
      expect_equal(resource, "https://storage.azure.com/")
      expect_null(token_args)
      structure(list(token = "managed-identity-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  result <- mix_azure_get_token()

  expect_equal(class(result), "AzureToken")
})

test_that("container apps identity endpoint path uses X-IDENTITY-HEADER and manual token", {
  skip_if_not_installed("AzureAuth")
  withr::local_envvar(
    IDENTITY_ENDPOINT = "http://127.0.0.1/metadata/identity/oauth2/token",
    IDENTITY_HEADER = "container-apps-secret",
    AZURE_CLIENT_ID = "managed-identity-client-id"
  )

  managed_token_called <- FALSE
  captured_request <- NULL
  captured_manual_token <- NULL

  local_mocked_bindings(
    get_managed_token = function(...) {
      managed_token_called <<- TRUE
      stop("Should not be called in Container Apps identity-endpoint path")
    },
    AzureManualToken = list(
      new = function(token, type = NULL, tenant = NULL, resource = NULL) {
        captured_manual_token <<- list(
          token = token,
          type = type,
          tenant = tenant,
          resource = resource
        )
        structure(list(token = token, resource = resource), class = "AzureToken")
      }
    ),
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    request = function(url) {
      list(url = url, query = list(), headers = list())
    },
    req_url_query = function(req, ...) {
      req$query <- c(req$query, list(...))
      req
    },
    req_headers = function(req, ...) {
      req$headers <- c(req$headers, list(...))
      req
    },
    req_perform = function(req) {
      captured_request <<- req
      structure(list(), class = "mock_httr2_response")
    },
    resp_status = function(resp) {
      200L
    },
    resp_body_string = function(resp, ...) {
      '{"access_token":"manual-managed-identity-token","token_type":"Bearer","tenant":"tenant-id"}'
    },
    .package = "httr2"
  )

  result <- mix_azure_get_token()

  expect_false(managed_token_called, info = "get_managed_token should be bypassed in Container Apps path")
  expect_equal(captured_request$url, "http://127.0.0.1/metadata/identity/oauth2/token")
  expect_equal(captured_request$query$resource, "https://storage.azure.com/")
  expect_equal(captured_request$query$`api-version`, "2019-08-01")
  expect_equal(captured_request$query$client_id, "managed-identity-client-id")
  expect_equal(captured_request$headers$`X-IDENTITY-HEADER`, "container-apps-secret")
  expect_equal(captured_manual_token$token, "manual-managed-identity-token")
  expect_equal(captured_manual_token$type, "Bearer")
  expect_equal(captured_manual_token$tenant, "tenant-id")
  expect_equal(captured_manual_token$resource, "https://storage.azure.com/")
  expect_equal(result$token, "manual-managed-identity-token")
  expect_equal(result$resource, "https://storage.azure.com/")
})

test_that("container apps path is skipped when IDENTITY_HEADER is missing", {
  skip_if_not_installed("AzureAuth")
  withr::local_envvar(
    IDENTITY_ENDPOINT = "http://127.0.0.1/metadata/identity/oauth2/token",
    IDENTITY_HEADER = NA_character_,
    AZURE_CLIENT_ID = "managed-identity-client-id"
  )

  managed_token_called <- FALSE

  local_mocked_bindings(
    get_managed_token = function(resource, token_args = NULL, ...) {
      managed_token_called <<- TRUE
      expect_equal(resource, "https://storage.azure.com/")
      expect_equal(token_args, list(client_id = "managed-identity-client-id"))
      structure(list(token = "managed-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    request = function(...) {
      stop("Container Apps identity-endpoint path should not execute when header is missing")
    },
    .package = "httr2"
  )

  result <- mix_azure_get_token()

  expect_true(managed_token_called)
  expect_equal(class(result), "AzureToken")
})

test_that("managed identity fails, interactive session, params supplied", {
  skip_if_not_installed("AzureAuth")
  local_mocked_bindings(
    get_managed_token = function(...) {
      stop("Managed identity failed")
    },
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    get_azure_token = function(resource, tenant, app, auth_type, ...) {
      # Verify correct params were passed
      expect_equal(resource, "https://storage.azure.com/")
      expect_equal(tenant, "my-tenant-id")
      expect_equal(app, "my-app-id")
      expect_equal(auth_type, "authorization_code")
      structure(list(token = "interactive-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  result <- mix_azure_get_token(tenant_id = "my-tenant-id", app_id = "my-app-id", is_interactive = TRUE)

  expect_equal(class(result), "AzureToken")
})

test_that("managed identity fails, interactive, tenant_id/app_id from environment", {
  skip_if_not_installed("AzureAuth")
  withr::local_envvar(
    AZURE_TENANT_ID = "env-tenant-id",
    AZURE_CLIENT_ID = "env-app-id"
  )

  local_mocked_bindings(
    get_managed_token = function(...) {
      stop("Managed identity failed")
    },
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    get_azure_token = function(resource, tenant, app, auth_type, ...) {
      # Verify environment variables were resolved
      expect_equal(tenant, "env-tenant-id")
      expect_equal(app, "env-app-id")
      structure(list(token = "interactive-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  result <- mix_azure_get_token(is_interactive = TRUE)

  expect_equal(class(result), "AzureToken")
})

test_that("managed identity fails, interactive, tenant_id missing errors", {
  skip_if_not_installed("AzureAuth")
  # Ensure environment variable is unset
  withr::local_envvar(AZURE_TENANT_ID = NA_character_)

  local_mocked_bindings(
    get_managed_token = function(...) {
      stop("Managed identity failed")
    },
    .package = "AzureAuth"
  )

  expect_error(
    mix_azure_get_token(is_interactive = TRUE),
    "tenant_id must be supplied or AZURE_TENANT_ID environment variable must be set"
  )
})

test_that("managed identity fails, non-interactive session errors immediately", {
  skip_if_not_installed("AzureAuth")
  managed_token_call_count <- 0
  interactive_fallback_call_count <- 0

  local_mocked_bindings(
    get_managed_token = function(...) {
      managed_token_call_count <<- managed_token_call_count + 1
      stop("Managed identity failed")
    },
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    get_azure_token = function(...) {
      interactive_fallback_call_count <<- interactive_fallback_call_count + 1
      stop("Should not be called in non-interactive session")
    },
    .package = "AzureAuth"
  )

  expect_error(
    mix_azure_get_token(is_interactive = FALSE),
    "Managed identity authentication failed and session is non-interactive"
  )

  expect_equal(managed_token_call_count, 1, info = "get_managed_token should be called exactly once")
  expect_equal(interactive_fallback_call_count, 0, info = "get_azure_token should NOT be called in non-interactive session")
})

test_that("both managed identity and interactive fallback fail", {
  skip_if_not_installed("AzureAuth")
  local_mocked_bindings(
    get_managed_token = function(...) {
      stop("Managed identity service unavailable")
    },
    .package = "AzureAuth"
  )

  local_mocked_bindings(
    get_azure_token = function(...) {
      stop("Browser authentication failed: user denied")
    },
    .package = "AzureAuth"
  )

  expect_error(
    mix_azure_get_token(tenant_id = "tenant-id", app_id = "app-id", is_interactive = TRUE),
    "Both managed identity and interactive authentication failed"
  )
})

test_that("managed identity succeeds even when env vars unset (Bug 1 regression)", {
  skip_if_not_installed("AzureAuth")
  # This tests the specific regression: function should NOT error if env vars unset
  # when managed identity succeeds
  withr::local_envvar(
    AZURE_TENANT_ID = NA_character_,
    AZURE_CLIENT_ID = NA_character_
  )

  local_mocked_bindings(
    get_managed_token = function(resource, token_args = NULL, ...) {
      expect_equal(resource, "https://storage.azure.com/")
      expect_null(token_args)
      structure(list(token = "managed-token"), class = "AzureToken")
    },
    .package = "AzureAuth"
  )

  # Should not error even though env vars are unset
  result <- mix_azure_get_token()

  expect_equal(class(result), "AzureToken")
})
