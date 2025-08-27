# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`mixtape` is an R package that enhances end-to-end data science operations. It provides utility functions for data cleaning, cloud storage operations (GCS, Azure), visualization themes, statistical functions, and automation tools.

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

### Git Operations
The `git-commit.R` file contains helpful scripts for version control:
```bash
# Stage and commit changes
git add .
git commit -m "Your commit message"

# Push to remote
git push origin master
```

## Architecture

### Function Categories

**Cloud Storage Operations**
- `mix_gcs_*` functions: Google Cloud Storage operations (read, write, data transfer)
- `mix_azure_storage_*` functions: Azure Storage operations 
- `mix_bq_to_gcs()`, `mix_gcs_to_bq()`: BigQuery integration

**Code Execution & Automation**
- `mix_code_execution()`: Execute code remotely
- `mix_batch_code_execution()`: Batch code execution
- `mix_cluster_*` functions: Cluster management

**Data Processing & Analysis**
- `bin()`, `auto_bin()`: Data binning utilities
- `clean()`: Data cleaning operations
- `dts()`: Date/time string operations
- `train_index()`: Training/testing data splits

**Visualization**
- `mix_theme()`: Consistent ggplot2 theme
- `mix_palette*`: Color palette functions
- `tile_plot()`, `roc_plot()`: Specialized plot functions

**Utility Functions**  
- `%><%`, `%>=<%` etc.: Custom infix operators
- `if_na()`, `if_null()`: Null/NA handling
- `pretty_*` functions: Number formatting
- `substr_left()`, `substr_right()`: String manipulation

### Code Conventions

- Functions use roxygen2 documentation with `@description`, `@param`, `@export` tags
- Function names use snake_case with `mix_` prefix for core functionality
- Each function is in its own file: `functionname-function.R`
- Package dependencies are loaded at function level with `library()` calls
- Default parameters use single letters: `F` instead of `FALSE`

### File Structure

- `R/`: All function definitions
- `man/`: Auto-generated documentation files (do not edit manually)
- `NAMESPACE`: Auto-generated exports (managed by roxygen2)
- `DESCRIPTION`: Package metadata and dependencies
- `git-commit.R`: Development workflow scripts