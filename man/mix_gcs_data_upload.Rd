% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mix_gcs_data_upload-function.R
\name{mix_gcs_data_upload}
\alias{mix_gcs_data_upload}
\title{Upload data to Google Cloud Storage}
\usage{
mix_gcs_data_upload(
  project,
  df,
  object_name,
  object_name_wildcard_length = 5,
  bucket,
  folder = NULL,
  max_object_size_mb = 50,
  object_format = "parquet"
)
}
\arguments{
\item{project}{Google Cloud project ID}

\item{df}{Data frame to upload}

\item{object_name}{Base name for the output files}

\item{object_name_wildcard_length}{Length of the wildcard part in object names (default: 5)}

\item{bucket}{Name of the Google Cloud Storage bucket}

\item{folder}{Folder path within the bucket (optional)}

\item{max_object_size_mb}{Maximum size of each output file in MB (default: 50)}

\item{object_format}{Format for the output files ('parquet', 'csv') (default: 'parquet')}
}
\description{
Uploads a data frame to Google Cloud Storage with optional batching for large datasets.
Supports different file formats and handles error conditions.
}
