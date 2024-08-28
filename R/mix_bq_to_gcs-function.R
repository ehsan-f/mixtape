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

  #-- Packages
  library(googleCloudStorageR)
  library(bigrquery)
  library(dplyr)

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
