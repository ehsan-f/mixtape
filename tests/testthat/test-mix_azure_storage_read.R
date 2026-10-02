# Unit tests for mix_azure_storage_read() — the storage function the pipeline
# API actually calls at startup (plumber-api/run.R) to load reference data:
#   * 5 macro parquet files (multi-file listing, combined)
#   * 3 tag-mapping JSON files (listing + json parsing, return_list = FALSE)
# All Azure network calls are mocked; parquet/json parsing is exercised for
# real against in-memory bytes. Auth resolution itself is covered separately
# in test-mix_azure_resolve_endpoint.R / test-mix_azure_get_token.R.

# --- helpers ----------------------------------------------------------------

make_parquet_raw <- function(df) {
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf), add = TRUE)
  arrow::write_parquet(df, tf)
  readBin(tf, what = "raw", n = file.size(tf))
}

make_json_raw <- function(df) {
  charToRaw(as.character(jsonlite::toJSON(df, dataframe = "rows")))
}

# Mocks the three AzureStor calls plus the internal auth resolution in one go.
# `files` is the container listing; `downloads` maps file name -> raw bytes.
# Returns an environment recording what was requested, for assertions.
mock_azure_storage <- function(files, downloads, env = parent.frame()) {
  recorded <- new.env(parent = emptyenv())
  recorded$resolve_args <- NULL
  recorded$downloaded <- character(0)

  local_mocked_bindings(
    mix_azure_resolve_endpoint = function(endpoint, storage_key = NULL, token = NULL) {
      recorded$resolve_args <- list(
        endpoint = endpoint, storage_key = storage_key, token = token
      )
      "mock-storage-account"
    },
    list_storage_containers = function(...) list(refdata = "mock-container"),
    list_storage_files = function(...) data.frame(name = files, stringsAsFactors = FALSE),
    storage_download = function(container, src, dest = NULL) {
      recorded$downloaded <- c(recorded$downloaded, src)
      downloads[[src]]
    },
    .env = env
  )

  recorded
}

# --- input validation -------------------------------------------------------

test_that("non-logical single_file is rejected", {
  expect_error(
    mix_azure_storage_read(
      storage_account_name = "acct", container_name = "refdata",
      prefix = "x.parquet", single_file = "yes"
    ),
    "single_file"
  )
})

test_that("non-logical return_list is rejected", {
  expect_error(
    mix_azure_storage_read(
      storage_account_name = "acct", container_name = "refdata",
      prefix = "x.parquet", return_list = 1
    ),
    "return_list"
  )
})

# --- endpoint construction & auth pass-through ------------------------------

test_that("adls (the API default) builds a dfs endpoint; blob builds a blob endpoint", {
  df <- data.frame(month = "2024-01", cpi = 3.1)
  recorded <- mock_azure_storage(
    files = "macro/cpi.parquet",
    downloads = list("macro/cpi.parquet" = make_parquet_raw(df))
  )

  suppressMessages(mix_azure_storage_read(
    storage_account_name = "myaccount", container_name = "refdata",
    prefix = "macro", storage_type = "adls"
  ))
  expect_equal(recorded$resolve_args$endpoint, "https://myaccount.dfs.core.windows.net")

  suppressMessages(mix_azure_storage_read(
    storage_account_name = "myaccount", container_name = "refdata",
    prefix = "macro", storage_type = "blob"
  ))
  expect_equal(recorded$resolve_args$endpoint, "https://myaccount.blob.core.windows.net")
})

test_that("storage_key and token are passed through to auth resolution untouched", {
  df <- data.frame(month = "2024-01", cpi = 3.1)
  recorded <- mock_azure_storage(
    files = "macro/cpi.parquet",
    downloads = list("macro/cpi.parquet" = make_parquet_raw(df))
  )

  # Local/production shape: key set, token NULL (run.R passes exactly one)
  suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro", storage_key = "the-key", token = NULL
  ))
  expect_equal(recorded$resolve_args$storage_key, "the-key")
  expect_null(recorded$resolve_args$token)

  # CI shape: key NULL, OIDC token set
  suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro", storage_key = NULL, token = "the-ci-token"
  ))
  expect_null(recorded$resolve_args$storage_key)
  expect_equal(recorded$resolve_args$token, "the-ci-token")
})

# --- multi-file parquet (the macro reference-data path) ---------------------

test_that("multi-file parquet read filters by extension (case-insensitive) and row-binds", {
  df_a <- data.frame(month = c("2024-01", "2024-02"), cpi = c(3.1, 3.0))
  df_b <- data.frame(month = c("2024-03", "2024-04"), cpi = c(2.9, 2.8))

  recorded <- mock_azure_storage(
    files = c("macro/cpi_a.parquet", "macro/cpi_b.PARQUET",
              "macro/notes.txt", "macro/legacy.csv"),
    downloads = list(
      "macro/cpi_a.parquet" = make_parquet_raw(df_a),
      "macro/cpi_b.PARQUET" = make_parquet_raw(df_b)
    )
  )

  result <- suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro", object_format = "parquet"
  ))

  expect_setequal(recorded$downloaded, c("macro/cpi_a.parquet", "macro/cpi_b.PARQUET"))
  expect_equal(nrow(result), 4L)
  expect_equal(sort(result$month), c("2024-01", "2024-02", "2024-03", "2024-04"))
})

test_that("regex_pattern narrows the file listing", {
  df <- data.frame(month = "2024-01", value = 1)
  recorded <- mock_azure_storage(
    files = c("macro/cpi.parquet", "macro/gdp.parquet"),
    downloads = list("macro/cpi.parquet" = make_parquet_raw(df))
  )

  result <- suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro", regex_pattern = "cpi"
  ))

  expect_equal(recorded$downloaded, "macro/cpi.parquet")
  expect_equal(nrow(result), 1L)
})

test_that("n_files caps how many files are read", {
  df <- data.frame(month = "2024-01", value = 1)
  files <- c("macro/a.parquet", "macro/b.parquet", "macro/c.parquet")
  recorded <- mock_azure_storage(
    files = files,
    downloads = setNames(
      lapply(files, function(f) make_parquet_raw(df)),
      files
    )
  )

  result <- suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro", n_files = 2
  ))

  expect_length(recorded$downloaded, 2L)
  expect_equal(nrow(result), 2L)
})

test_that("no matching files raises a clear error naming format and prefix", {
  mock_azure_storage(
    files = c("macro/notes.txt", "macro/readme.md"),
    downloads = list()
  )

  expect_error(
    suppressMessages(mix_azure_storage_read(
      storage_account_name = "acct", container_name = "refdata",
      prefix = "macro", object_format = "parquet"
    )),
    "No parquet files found at: macro"
  )
})

test_that("return_list = TRUE returns one data frame per file, uncombined", {
  df_a <- data.frame(month = "2024-01", value = 1)
  df_b <- data.frame(month = "2024-02", value = 2)
  mock_azure_storage(
    files = c("macro/a.parquet", "macro/b.parquet"),
    downloads = list(
      "macro/a.parquet" = make_parquet_raw(df_a),
      "macro/b.parquet" = make_parquet_raw(df_b)
    )
  )

  result <- suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro", return_list = TRUE
  ))

  expect_type(result, "list")
  expect_length(result, 2L)
  expect_equal(as.data.frame(result[[1]]), df_a)
  expect_equal(as.data.frame(result[[2]]), df_b)
})

# --- single_file mode -------------------------------------------------------

test_that("single_file = TRUE downloads the prefix directly without listing", {
  df <- data.frame(month = "2024-01", value = 1)
  recorded <- new.env(parent = emptyenv())
  recorded$downloaded <- character(0)

  local_mocked_bindings(
    mix_azure_resolve_endpoint = function(...) "mock-storage-account",
    list_storage_containers = function(...) list(refdata = "mock-container"),
    list_storage_files = function(...) stop("listing must not be called in single_file mode"),
    storage_download = function(container, src, dest = NULL) {
      recorded$downloaded <- c(recorded$downloaded, src)
      make_parquet_raw(df)
    }
  )

  result <- suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "macro/exact-file.parquet", single_file = TRUE
  ))

  expect_equal(recorded$downloaded, "macro/exact-file.parquet")
  expect_equal(as.data.frame(result), df)
})

# --- json (the tag reference-data path) --------------------------------------

test_that("json format parses plain .json and gzipped .json.gz files", {
  df_tags <- data.frame(
    client_tag_final = c("groceries", "gambling"),
    serene_tag_final = c("essential_spend", "gambling"),
    stringsAsFactors = FALSE
  )
  df_more <- data.frame(
    client_tag_final = "rent",
    serene_tag_final = "housing",
    stringsAsFactors = FALSE
  )

  recorded <- mock_azure_storage(
    files = c("tags/client_tags_mapped.json", "tags/extra_tags.json.gz",
              "tags/ignore.parquet"),
    downloads = list(
      "tags/client_tags_mapped.json" = make_json_raw(df_tags),
      "tags/extra_tags.json.gz" = memCompress(make_json_raw(df_more), type = "gzip")
    )
  )

  result <- suppressMessages(mix_azure_storage_read(
    storage_account_name = "acct", container_name = "refdata",
    prefix = "tags", object_format = "json", return_list = FALSE
  ))

  expect_setequal(
    recorded$downloaded,
    c("tags/client_tags_mapped.json", "tags/extra_tags.json.gz")
  )
  expect_equal(nrow(result), 3L)
  expect_setequal(result$client_tag_final, c("groceries", "gambling", "rent"))
  expect_setequal(names(result), c("client_tag_final", "serene_tag_final"))
})
