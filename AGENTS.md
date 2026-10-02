# AGENTS.md

Guidance for AI agents and contributors working on code in this repository.

## Package Overview

`mixtape` is an R package that enhances end-to-end data science operations. It provides utility functions for data cleaning, cloud storage operations (GCS, BigQuery, Azure, Databricks, Google Drive), ML modelling helpers, visualisation themes and plots, and automation tools.

`mixtape.serene` (`Serene-Data-Science/mixtape.serene`) has the same functions plus Serene-specific transaction tagging (`serene_*`). Every function in this package is kept identical in both, along with its documentation and tests, so a change to any function here should be copied there too (and vice versa). The exception is the geo functions (`mix_azure_geo_read()`, `mix_azure_geo_write()`, `mix_point_to_h3()`), which were removed from `mixtape.serene` because the `sf` package is problematic to install in Docker; they exist only here.

## Development Commands

### Package Development
```bash
# Update NAMESPACE and documentation
Rscript -e "devtools::document()"

# Install package in development mode
Rscript -e "devtools::install()"

# Load package for testing
Rscript -e "devtools::load_all()"

# Check package integrity
Rscript -e "devtools::check()"
```

### Tests
```bash
# Run the whole test suite
Rscript -e "devtools::test()"

# Run one test file
Rscript -e "testthat::test_file('tests/testthat/test-pretty.R')"
```

Tests use `testthat` (3rd edition) and live in `tests/testthat/`, one `test-<topic>.R` file per source file (e.g. `test-infix.R` for `infix-functions.R`). They also run in CI on every push and pull request to `master` (`.github/workflows/unit-tests.yml`). When adding or changing a function in a tested file, add or update its tests. When a test fails after a change, check whether the behaviour change was intended before updating the test.

Coverage so far: the small, self-contained functions (the infix operators, `pretty_*()`, `elapsed_*()`, `if_na()` / `if_null()`, `substr_*()`, `is_date()`, `mix_mode()`, `time_key()`, `set_integer_to_numeric()`, `nested_tbl_build()` / `nested_tbl_extract()`) and the Azure token/endpoint/storage-read logic, which mocks all Azure calls so no credentials are needed. Wrapper functions around modelling packages (e.g. `bin()` / `auto_bin()`) and the other cloud-service functions are not tested.

### Git Operations
The default branch is `master`. `git-commit.R` holds the R-side workflow (`gert::git_add()` / `git_commit()` / `git_push()`) plus one-time credential and clone setup. From a shell:
```bash
git add .
git commit -m "Your commit message"
git push origin master
```

## Architecture

### Function Categories

**Cloud Storage & Data Access**
- `mix_gcs_*`: Google Cloud Storage (object read/upload, Arrow dataset read/write, code execution)
- `mix_bq_to_gcs()`, `mix_gcs_to_bq()`, `gcp_auth()`: BigQuery/GCP
- `mix_azure_storage_*` (`read`, `write`, `list`), `mix_azure_geo_read()` / `mix_azure_geo_write()`: Azure Storage. Each accepts a `storage_key`, a `token`, or neither (automatic resolution)
- `mix_azure_get_token()`: Azure token acquisition (Managed Identity, with interactive fallback); the internal `mix_azure_resolve_endpoint()` picks between a storage key, an explicit token and automatic resolution. Needs the `AzureAuth` package (in `Suggests`) for the token paths
- `mix_databricks_read()`, `mix_gdr_read()` (Google Drive), `copy_table()`

**Code Execution & Automation**
- `mix_code_execution()`, `mix_batch_code_execution()`: Execute code remotely / in batches
- `mix_cluster_make()`, `mix_cluster_stop()`: Parallel cluster management
- `mix_load_packages()`, `mix_r_setup()`: Session setup

**Data Processing**
- `bin()`, `auto_bin()`, `as_nlevels()`: Binning utilities
- `clean()`: Data cleaning
- `set_integer_to_numeric()`: Type helper
- `nested_tbl_build()` / `nested_tbl_extract()`: Pack a named list of tables into a one-row nested tibble (for nested parquet output) and get a table back out
- `dts()`, `time_key()`, `is_date()`, `elapsed_days()` / `_weeks()` / `_months()` / `_years()`: Dates and times
- `mix_mode()`, `mix_point_to_h3()`

**Modelling**
- `mix_ml_feature_selection()`, `mix_ml_tunez()`, `mix_ml_model_metrics()`, `mix_ml_shap_prob_bands()`
- `mix_train_test_split()`, `mix_train_index()`, `train_index()`: Train/test splits
- `mix_apply_tiles()`

**Visualisation**
- `mix_theme()`: Consistent ggplot2 theme
- `mix_palette()`, `mix_palette_gg()`, `mix_palette_light()`, `mix_palette_cb_jp()`: Colour palettes
- `roc_plot()`, `pr_roc_plot()`, `lift_chart()`: Model performance plots

**Utility Functions**
- `%><%`, `%>=<%`, `%limit%`, `%bracket_min%`, `%bracket_max%` etc.: Custom infix operators
- `if_na()`, `if_null()`: Null/NA handling
- `pretty_num()`, `pretty_perc()`, `pretty_curr()`: Number formatting
- `substr_left()`, `substr_right()`, `current_file_location()`

### Code Conventions

- Follow the R style guide shipped with this package: [`inst/R-STYLE-GUIDE.md`](inst/R-STYLE-GUIDE.md). It applies both to this package and to projects that use it. Once installed, find it with `system.file('R-STYLE-GUIDE.md', package = 'mixtape')`.
- Functions use roxygen2 documentation with `@description`, `@param`, `@export` tags.
- Dependencies are declared with roxygen `@importFrom` tags (and listed under `Imports` in `DESCRIPTION`), not loaded with `library()` inside functions. The exceptions are `mix_load_packages()`, whose job is attaching packages, and `mix_point_to_h3()`.
- Function names use `snake_case`, with a `mix_` prefix for core functionality.
- Each function is in its own file, `functionname-function.R`; small related groups share a `*-functions.R` file (e.g. `infix-functions.R`, `elapsed-functions.R`).
- Default parameters use `T` / `F`, not `TRUE` / `FALSE`.

### File Structure

- `R/`: All function definitions
- `man/`: Auto-generated documentation files (do not edit manually)
- `NAMESPACE`: Auto-generated exports and imports (managed by roxygen2)
- `DESCRIPTION`: Package metadata and dependencies
- `inst/R-STYLE-GUIDE.md`: R code style guide, installed with the package
- `tests/testthat/`: Unit tests
- `.github/workflows/unit-tests.yml`: Runs the tests in CI
- `git-commit.R`: Development workflow scripts
