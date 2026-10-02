test_that("valid storage_key uses key-based auth", {
  local_mocked_bindings(
    storage_endpoint = function(...) {
      # Capture arguments to verify key is passed, not token
      args <- list(...)
      expect_equal(args$endpoint, "https://example.blob.core.windows.net/")
      expect_equal(args$key, "my-storage-key")
      expect_null(args$token)
      "mocked-endpoint"
    },
    .package = "AzureStor"
  )

  result <- mixtape:::mix_azure_resolve_endpoint(
    endpoint = "https://example.blob.core.windows.net/",
    storage_key = "my-storage-key"
  )

  expect_equal(result, "mocked-endpoint")
})

test_that("empty string storage_key falls through to token path", {
  token_called <- FALSE
  get_token_called <- FALSE

  local_mocked_bindings(
    storage_endpoint = function(...) {
      args <- list(...)
      token_called <<- TRUE
      # Should be called with token, not key
      expect_null(args$key)
      expect_equal(args$token, "resolved-token")
      "mocked-endpoint"
    },
    .package = "AzureStor"
  )

  local_mocked_bindings(
    mix_azure_get_token = function(...) {
      get_token_called <<- TRUE
      "resolved-token"
    },
    .package = "mixtape"
  )

  result <- mixtape:::mix_azure_resolve_endpoint(
    endpoint = "https://example.blob.core.windows.net/",
    storage_key = ""
  )

  expect_true(token_called, info = "storage_endpoint should be called with token")
  expect_true(get_token_called, info = "mix_azure_get_token should be called for empty key")
  expect_equal(result, "mocked-endpoint")
})

test_that("NA storage_key falls through to token path", {
  token_called <- FALSE
  get_token_called <- FALSE

  local_mocked_bindings(
    storage_endpoint = function(...) {
      args <- list(...)
      token_called <<- TRUE
      expect_null(args$key)
      expect_equal(args$token, "resolved-token")
      "mocked-endpoint"
    },
    .package = "AzureStor"
  )

  local_mocked_bindings(
    mix_azure_get_token = function(...) {
      get_token_called <<- TRUE
      "resolved-token"
    },
    .package = "mixtape"
  )

  result <- mixtape:::mix_azure_resolve_endpoint(
    endpoint = "https://example.blob.core.windows.net/",
    storage_key = NA_character_
  )

  expect_true(token_called, info = "storage_endpoint should be called with token")
  expect_true(get_token_called, info = "mix_azure_get_token should be called for NA key")
  expect_equal(result, "mocked-endpoint")
})

test_that("NULL storage_key with valid token uses token-based auth", {
  local_mocked_bindings(
    storage_endpoint = function(...) {
      args <- list(...)
      expect_null(args$key)
      expect_equal(args$token, "my-token")
      "mocked-endpoint"
    },
    .package = "AzureStor"
  )

  result <- mixtape:::mix_azure_resolve_endpoint(
    endpoint = "https://example.blob.core.windows.net/",
    storage_key = NULL,
    token = "my-token"
  )

  expect_equal(result, "mocked-endpoint")
})

test_that("storage_key takes priority over token when both supplied", {
  local_mocked_bindings(
    storage_endpoint = function(...) {
      args <- list(...)
      # Should use key, not token
      expect_equal(args$key, "my-key")
      expect_null(args$token)
      "mocked-endpoint"
    },
    .package = "AzureStor"
  )

  result <- mixtape:::mix_azure_resolve_endpoint(
    endpoint = "https://example.blob.core.windows.net/",
    storage_key = "my-key",
    token = "my-token"
  )

  expect_equal(result, "mocked-endpoint")
})

test_that("neither storage_key nor token triggers auto-resolve", {
  get_token_called <- FALSE

  local_mocked_bindings(
    storage_endpoint = function(...) {
      args <- list(...)
      expect_null(args$key)
      expect_equal(args$token, "auto-resolved-token")
      "mocked-endpoint"
    },
    .package = "AzureStor"
  )

  local_mocked_bindings(
    mix_azure_get_token = function(...) {
      get_token_called <<- TRUE
      "auto-resolved-token"
    },
    .package = "mixtape"
  )

  result <- mixtape:::mix_azure_resolve_endpoint(
    endpoint = "https://example.blob.core.windows.net/"
  )

  expect_true(get_token_called, info = "mix_azure_get_token should be called when no key/token")
  expect_equal(result, "mocked-endpoint")
})

test_that("invalid storage_key type raises error", {
  expect_error(
    mixtape:::mix_azure_resolve_endpoint(
      endpoint = "https://example.blob.core.windows.net/",
      storage_key = 12345
    ),
    "storage_key must be a length-1 character string or NULL"
  )
})

test_that("storage_key with length > 1 raises error", {
  expect_error(
    mixtape:::mix_azure_resolve_endpoint(
      endpoint = "https://example.blob.core.windows.net/",
      storage_key = c("key1", "key2")
    ),
    "storage_key must be a length-1 character string or NULL"
  )
})
