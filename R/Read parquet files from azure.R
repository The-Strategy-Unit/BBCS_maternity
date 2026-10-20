## Azure setup -----------------------------------------------------------------
# set path to lake mart
container <- Sys.getenv("container")
lake <- Sys.getenv("lake")
folder <- Sys.getenv("folder")
path <- glue::glue("abfss://{container}@{lake}{folder}")

# set up access settings 
# get shared access signature auth for data
# nb, follow any authentication directions in the console.
vault_nm <- Sys.getenv("azure_key_vault")
vault <- tryCatch(
  AzureKeyVault::key_vault(vault_nm),
  error = function(e) {
    cli::cli_abort(
      "Failed to access Azure Key Vault {.val {vault_nm}}.
        Check your authentication and network connection. \n{e$message}"
    )
  }
)

# read from lake 
sas_nm <- glue::glue("alp-{container}-StrategyUnit-r")
# get the SAS for the relevant data, r = read access, w = write access
az_sas <- tryCatch(
  vault$secrets$get(sas_nm),
  error = function(e) {
    cli::cli_abort(
      "Could not retrieve SAS token {.val {sas_nm}} from Key Vault.
        Please ensure the secret exists and you have access. \n{e$message}"
    )
  }
)

if (is.null(az_sas$value) || nchar(az_sas$value) == 0) {
  cli::cli_abort(
    "SAS token {.val {sas_name}} is empty.
      Please check the Key Vault secret configuration."
  )
}

# set connection string
az_endpoint <- AzureStor::storage_endpoint(
  glue::glue("https://{lake}"),
  sas = az_sas$value
)

# set the storage container
az_container <- AzureStor::storage_container(az_endpoint, container)

# list files in the project folder
az_files <- AzureStor::list_adls_files(az_container, folder, recursive = TRUE)

## Function to read data from Azure----------------------------------------------

# Adapted from 
# https://github.com/The-Strategy-Unit/nnhip_index_and_scorecard/blob/main/R/utils.R
# on 27/04/2026.

#' Get a Strategy Unit LakeMart Parquet File
#'
#' @description
#' Retrieves and consolidates all indvidual `part-*` files from a Parquet
#' dataset stored in the Strategy Unit LakeMart area of UDAL, returning the
#' combined data as a single tibble.
#'
#' @details
#' This function relies on several environment variables to construct the
#' connection path and authenticate against Azure Data Lake Storage (ADLS).
#' These must be available in the R session before calling the function.
#'
#' @section Required environment variables:
#' The following environment variables must be set (e.g. via `.Renviron`,
#' `Sys.setenv()`, or your project settings):
#'
#' - **`container`** - the ADLS container name
#' - **`lake`** - the ADLS account name including domain name
#' - **`folder`** - the folder path within the container where the Parquet
#' files reside.
#' - **`azure_key_vault`** - The name of the Azure Key Vault containing SAS
#' tokens.
#'
#' The function expects a read-access SAS token stored in Key Vault under the
#' name: `"alp-{container}-StrategyUnit-r"`.
#'
#' @section How file matching works:
#' The `str_file_pattern` argument is used to filter file names returned from
#' ADLS. It should match part of the Parquet dataset path, for example:
#' ```
#' "matching_referrals.parquet/part"
#' ```
#' All matching Parquet files are downloaded and read using
#' `arrow::read_parquet()`, then combined row-wise into a single tibble.
#'
#' @section Error handling:
#' The function performs several validation checks and will abort with a clear
#' diagnostic message if:
#' - required environment variables are missing or empty,
#' - the Azure Key Vault cannot be accessed,
#' - the expected SAS token is not found in Key Vault,
#' - no files match the supplied `str_file_pattern`
#'
#' These checks are intended to make configuration issues easier to diagnose.
#'
#' @param str_file_pattern String. A substring used to identify the Parquet dataset within the Strategy Unit LakeMart folder structure.
#'
#' @returns A tibble containing the combined data from all matching Parquet part files.
#'
#' @examples
#' \dontrun{
#' df <- get_su_lakemart_parquet_file("matching_referrals.parquet/part")
#' }
get_su_lakemart_parquet_file <- function(str_file_pattern = "", az_files, az_container) {
  # ensure a file pattern was supplied
  if (nchar(str_file_pattern) == 0) {
    cli::cli_abort(
      "{.var str_file_pattern} must be a string matching part of a parquet file, e.g. {.val matching_referrals.parquet/part}"
    )
  }
  
  # identify parquet files in the matching_variables folder
  requested_file <-
    az_files |>
    tibble::as_tibble() |>
    dplyr::filter(stringr::str_detect(
      string = name,
      pattern = str_file_pattern
    )) |>
    dplyr::pull(name)
  
  if (length(requested_file) == 0) {
    cli::cli_abort(
      "No files found matching pattern {.val {str_file_pattern}} in folder {.val {folder}}."
    )
  }
  
  # read all parquet files to a single df
  df_return <-
    purrr::map_dfr(
      .x = requested_file,
      .f = \(.x) {
        AzureStor::storage_download(
          container = az_container,
          src = .x,
          dest = NULL
        ) |>
          arrow::read_parquet()
      }
    )
  
  # return the df
  return(df_return)
}



## Get data from Azure -----------------------------------------------------

final_cohort_2326<- get_su_lakemart_parquet_file(
  "final_cohort_2326.parquet/part",
  az_files,
  az_container
)|>    janitor::clean_names()

total_births_by_LA_2326<- get_su_lakemart_parquet_file(
  "total_births_by_LA_2326.parquet/part",
  az_files,
  az_container
)|>    janitor::clean_names()

trust_matrix_data<- get_su_lakemart_parquet_file(
  "trust_matrix.parquet/part",
  az_files,
  az_container
)|>    janitor::clean_names()

icb_matrix_data<- get_su_lakemart_parquet_file(
  "icb_matrix.parquet/part",
  az_files,
  az_container
)|>    janitor::clean_names()

demographics<- get_su_lakemart_parquet_file(
  "demographics.parquet/part",
  az_files,
  az_container
)|>    janitor::clean_names()


