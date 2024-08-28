#' @export
mix_gcs_to_bq <- function(project,
                          bucket,
                          folder = NULL,
                          object_name,
                          dataset,
                          table,
                          partition_index = NULL,
                          cluster_index = NULL,
                          use_staging = 0,
                          staging_dataset,
                          object_format = 'parquet') {

  #-- Start Time
  v_start_time <- Sys.time()

  #-- Start Process Info
  message('Object: ', object_name)

  #-- Packages
  library(googleCloudStorageR)
  library(bigrquery)
  library(dplyr)
  library(purrr)

  #-- Variables
  v_staging <- max(if_else(!is.null(cluster_index) | !is.null(partition_index), 1, 0), use_staging)
  v_bq_table <- paste0(dataset, '.', table)
  v_staging_bq_table <- paste0(staging_dataset, '.', table)


  #----- Push Data into GCS
  #-- Set vars
  v_folder <- if_else(is.null(folder), '', paste0(folder, '/'))
  v_uri <- paste0(bucket, '/', v_folder, object_name, '*.', object_format)

  #-- Create query
  v_query_push_to_gcs <- paste0(

    "LOAD DATA OVERWRITE ", if_else(v_staging == 1, v_staging_bq_table, v_bq_table),
    "
    ",
    "FROM FILES (
         format = '", object_format, "',
         uris = ['gs://", v_uri, "'])"
  )

  #-- Run query
  tryCatch(

    expr = {
      bigrquery::bq_project_query(
        x = project,
        query = v_query_push_to_gcs
      )
    },

    error = function(e) {
      message(e)
      message('An error was detected.')

      if (!is.null(e) &
          grepl(pattern = "no applicable method for 'as_bq_table' applied to an object of", x = e)) {

        message('Error not fatal, process can continue.')

      } else {

        stop(e)

      }
    }
  )

  #----- Parition / Cluster Table
  if (v_staging == 1) {
    #-- Set vars
    #- Get var info from BQ
    v_staging_bq_table_fields <- bq_table_fields(x = paste0(project, '.', v_staging_bq_table))
    ls_staging_bq_fields <- NULL
    for (i in 1:length(v_staging_bq_table_fields)) {
      ls_staging_bq_fields[[i]] <- tibble(name = v_staging_bq_table_fields[[i]]$name,
                                          type = v_staging_bq_table_fields[[i]]$type)
    }
    ds_staging_bq_fields <- ls_staging_bq_fields %>% list_rbind()

    #- Partition / cluster query segments
    v_partition <- if_else(is.null(partition_index),
                           '',
                           if_else(ds_staging_bq_fields$type[ds_staging_bq_fields$name == partition_index] == 'DATE',
                                   paste0('PARTITION BY ', partition_index),
                                   paste0('PARTITION BY DATE(', partition_index, ')')))
    v_cluster <- if_else(is.null(cluster_index), '', paste0('CLUSTER BY ', cluster_index))

    #-- Create query
    v_query_pt_cl <- paste0(
      "CREATE OR REPLACE TABLE ", v_bq_table,
      "
    ",
    v_partition,
    "
    ",
    v_cluster,
    "
    ",
    "AS (",
    "
    ",
    "SELECT ",
    if_else(!is.null(cluster_index),
            paste0("CAST(", cluster_index, " AS INT64) AS ", cluster_index, ","),
            ""),
    paste0("* EXCEPT (", cluster_index,")"),
    "
    ",
    "FROM ", v_staging_bq_table,
    ")"
    )

    #-- Run query
    tryCatch(

      expr = {
        bigrquery::bq_project_query(
          x = project,
          query = v_query_pt_cl
        )
      },

      error = function(e) {
        message(e)
        message('An error was detected.')

        if (!is.null(e) &
            grepl(pattern = "no applicable method for 'as_bq_table' applied to an object of", x = e)) {

          message('Error not fatal, process can continue.')

        } else {

          #-- Remove staging table
          bigrquery::bq_project_query(
            x = project,
            query = paste0("DROP TABLE ", v_staging_bq_table)
          )

          stop(e)

        }
      }
    )

    #-- Remove staging table
    bigrquery::bq_project_query(
      x = project,
      query = paste0("DROP TABLE ", v_staging_bq_table)
    )
  }

  #-- End Time
  v_end_time <- Sys.time()

  #-- Process Info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

}
