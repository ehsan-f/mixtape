#' Export BigQuery table to Google Cloud Storage
#'
#' @description
#' Exports data from a BigQuery table or external table to Google Cloud Storage.
#' Supports different output formats and handles error conditions.
#'
#' @param project Google Cloud project ID
#' @param dataset BigQuery dataset name (optional if using a fully qualified table)
#' @param table BigQuery table name
#' @param external_table Whether the source is an external table (default: FALSE)
#' @param external_server External server connection string if external_table is TRUE
#' @param bucket Name of the destination Google Cloud Storage bucket
#' @param folder Folder path within the bucket (optional)
#' @param object_format Format for the exported files (default: 'parquet')
#'
#' @importFrom bigrquery bq_project_query
#' @importFrom dplyr if_else
#' @export
mix_bq_to_gcs <- function(project,
                          dataset = NULL,
                          table,
                          external_table = F,
                          external_server = NULL,
                          bucket,
                          folder = NULL,
                          # destination_file_name,
                          object_format = 'parquet') {

  #-- Start Time
  v_start_time <- Sys.time()

  #-- Set vars
  folder <- if_else(is.null(folder), '', paste0(folder, '/'))
  v_uri <- paste0(bucket, '/', folder, table, '*.', object_format)
  v_bq_table <- if_else(is.null(dataset), table, paste0(dataset, '.', table))

  #-- Start Process Info
  message('Table: ', v_bq_table)

  #-- Create query
  if (external_table == T) {

    v_query <- paste0(
      "EXPORT DATA
        OPTIONS (
          uri = 'gs://", v_uri,"',
          format = '", object_format,"',
          overwrite = true)
        AS (
          SELECT
          *
          FROM EXTERNAL_QUERY('", external_server,"', 'SELECT * FROM ", v_bq_table, "')
        )"
    )

  } else {

    v_query <- paste0(
      "EXPORT DATA
        OPTIONS (
          uri = 'gs://", v_uri,"',
          format = '", object_format,"',
          overwrite = true)
        AS (
          SELECT
          *
          FROM ", v_bq_table, "
          ORDER BY 1 DESC
        )"
    )

  }

  #-- Push BQ data to GCS
  tryCatch(

    expr = {
      bigrquery::bq_project_query(
        x = project,
        query = v_query
      )
    },

    error = function(e) {
      message('An error was detected.')

      if (!is.null(e) &
          grepl(pattern = "no applicable method for 'as_bq_table' applied to an object of|must be a string, list, or", x = e)) {

        message('Error not fatal, process can continue.')

      } else {

        stop(e)

      }
    }
  )

  #-- End Time
  v_end_time <- Sys.time()

  #-- Process Info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

}
