% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mix_batch_code_execution-function.R
\name{mix_batch_code_execution}
\alias{mix_batch_code_execution}
\title{Execute multiple R scripts in a directory}
\usage{
mix_batch_code_execution(directory, run_manual_scripts = NA)
}
\arguments{
\item{directory}{Directory containing R scripts to execute}

\item{run_manual_scripts}{JSON array of script names to run (optional, default: NA)}
}
\description{
Executes multiple R scripts in a specified directory.
Optionally allows selection of specific scripts to run.
}
