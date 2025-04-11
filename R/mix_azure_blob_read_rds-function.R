#' Read a single RDS file from Azure Blob Storage
#'
#' @description
#' Downloads and reads an RDS file from Azure Blob Storage.
#' Handles authentication and error conditions.
#'
#' @param storage_account_name Name of the Azure storage account
#' @param container_name Name of the container in the storage account
#' @param file_path Path to the RDS file (must include .rds extension)
#' @param storage_sas Shared access signature for Azure authentication
#'
#' @export
mix_azure_blob_read_rds <- function(storage_account_name,
                                    container_name,
                                    file_path,
                                    storage_sas) {
  #-- Start time
  v_start_time <- Sys.time()

  #-- Start process info
  message('Reading RDS file: ', file_path)

  #-- Required packages
  library(AzureStor)

  #-- Authentication - use blob endpoint instead of dfs
  v_blob_end_point <- sprintf('https://%s.blob.core.windows.net', storage_account_name)
  v_blob_storage_account <- storage_endpoint(endpoint = v_blob_end_point,
                                             sas = storage_sas)

  #-- Create temporary file
  temp_file <- tempfile(fileext = ".rds")

  #-- Download and process the file
  tryCatch(
    expr = {
      # Create the full URL to the blob
      v_data_path <- sprintf(
        'https://%s.blob.core.windows.net/%s/%s?%s',
        storage_account_name, container_name, file_path, storage_sas
      )

      # Download to temp file
      download.file(url = v_data_path, destfile = temp_file, mode = "wb", quiet = TRUE)

      # Read the RDS file
      model_object <- readRDS(temp_file)

      # Remove temp file
      file.remove(temp_file)
      message('Downloaded file deleted.')
    },
    error = function(e) {
      # Remove temp file in case of error
      if (file.exists(temp_file)) {
        file.remove(temp_file)
        message('Downloaded file deleted.')
      }

      # Output error message
      stop("Error reading RDS file: ", e$message)
    }
  )

  #-- End time
  v_end_time <- Sys.time()

  #-- Process info
  v_time_taken <- difftime(v_end_time, v_start_time, units='mins')
  message('Time taken: ', round(as.numeric(v_time_taken), 3), ' mins')

  #-- Output
  return(model_object)
}
