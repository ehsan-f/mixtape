% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mix_code_execution-function.R
\name{mix_code_execution}
\alias{mix_code_execution}
\title{Execute R script with error handling and status reporting}
\usage{
mix_code_execution(
  script_path = NULL,
  script_url = NULL,
  google_sheet_id = NULL,
  sheet_name = "R_Code_Logs",
  script_prefix = NULL,
  ...
)
}
\arguments{
\item{script_path}{Path to the R script file to execute}

\item{script_url}{URL of the R script to execute (alternative to script_path)}

\item{google_sheet_id}{ID of Google Sheet to log execution details (optional)}

\item{sheet_name}{Name of the sheet in the Google Sheet for logging (default: 'R_Code_Logs')}

\item{script_prefix}{Prefix to add to the script name in logs (optional)}

\item{...}{Additional arguments}
}
\description{
Executes an R script with error handling and provides status reporting. It can source scripts from a local path
or from a URL, and can optionally log execution details to a Google Sheet.
}
