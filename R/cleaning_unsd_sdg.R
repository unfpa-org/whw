# This script defines routines for cleaning data from the UNSD SDG API.


#' Combine SDG indicators from a multi-sheet Excel file
#'
#' This function combines SDG indicators from a multi-sheet Excel file into a
#' single tibble. In so doing, it select columns of interest, removes duplicates
#' and and renames some of those in line with the agreed standard.
#'
#' @param filepath A path to the file produced by download_sgd_series_data.
#' @return A combined tibble of SDG indicators.
#' @seealso [data4whw::download_sgd_series_data()] that downloads the indicators.
#' @examples
#' combine_sdg_series_data("../data/tmp/unsd_indicators.xlsx")
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
combine_sdg_series_data <- function(filepath, verbose = TRUE) {
  sheet.names <- openxlsx::getSheetNames(filepath)
  series.data <- list()

  # mapping of new names to old ones
  to.rename <- list(
    "indicator_id" = "SeriesCode",
    "indicator_definition" = "SeriesDescription",
    "country" = "GeoAreaName",
    "year" = "TimePeriod",
    "value" = "Value",
    "source_details" = "Source"
  )

  # processing each series code one by one
  for (name in sheet.names) {
    df.sheet <- openxlsx::read.xlsx(xlsxFile = filepath, sheet = name)
    df.sheet %<>% mutate(
      # forcing strings to be able to combine the tibbles
      dplyr::across(dplyr::everything(), as.character)
    )
    series.data[[name]] <- df.sheet
  }

  # combining all the indicators into a single table
  df.series <- bind_rows(series.data)
  cols.before <- colnames(df.series)
  df.series %<>% dplyr::select(
    dplyr::one_of(as.character(to.rename)) | dplyr::starts_with("[")
  ) %>%
    dplyr::distinct() # drop fully duplicated rows, if any
  cols.after <- colnames(df.series)
  df.series %<>% dplyr::rename(!!!to.rename)
  if (isTRUE(verbose)) {
    cols <- as.character(dplyr::setdiff(cols.before, cols.after))
    cat("The following columns were removed:", cols, sep = "\n")
  }

  return(df.series)
}

#' Preprocess SDG indicators in line with the template.
#'
#' Preprocessess a combined dataframe of SDG indicators in line with
#' the template. In particular, renames the columns, removes redundant
#' disaggregation, streamlines data types and adds relevant columns. It
#' also runs sanity checks to validate the output.
#'
#' @param df.series A tibble of indicator data as returned by `combine_sdg_series_data`.
#' @return A cleaned tibble of SDG indicators.
#' @seealso [data4whw::combine_sdg_series_data()] that combines the indicators.
#' @examples
#' clean_sdg_series_data(df.series)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
clean_sdg_series_data <- function(df.series) {

  # list of available disaggregation columns
  to.rename <- list(
    # mapping to the template names
    "units" = "[Units]",
    "nature" = "[Nature]",
    "reporting_type" = "[Reporting.Type]",
    "age" = "[Age]",
    "sex" = "[Sex]",
    "residence" = "[Location]",
    "education" = "[Education.level]",
    "wealth" = "[Quantile]",
    "grounds_of_discrimination" = "[Grounds.of.discrimination]"
    # ignored columns
    #"" = "[Activity]",
    #"" = "[Observation.Status]",
    #"" = "[Freq]",
    #"" = "[Name.of.non-communicable.disease]",
  )

  df.output <- df.series %>%
    # filtering out redundant groups
    dplyr::filter( # for SL_ISV_IFEM
      `[Activity]` == "TOTAL" | is.na(`[Activity]`)
    ) %>%
    # renaming columns and removing redundant disaggregations
    dplyr::rename(!!!to.rename) %>%
    dplyr::select(-dplyr::starts_with("[")) %>%
    dplyr::mutate(
      source = "SDG",
      year = as.integer(year)
    )

  validate_output_sdg(df.output)
  return(df.output)
}

#' Validate output dataframe using sanity checks.
#'
#' An internal function to validate the data produces within `clean_sdg_series_data`.
#' Currently only checks if duplicates result from data processing. If so,
#' issues a warning.
#'
#' @param df.output A tibble of indicator data within `clean_sdg_series_data`.
#' @return Nothing
#' @seealso [data4whw::clean_sdg_series_data()] that calls this function internally.
#' @importFrom magrittr %>%
validate_output_sdg <- function(df.output) {
  indicators <- df.output %>% pull(indicator_id) %>% unique()

  for (ind_id in indicators) {
    # if any relevant dimension was excluded, there will be non-unique rows
    df.temp <- df.output %>%
      filter(indicator_id == ind_id) %>%
      select(-value)
    n_duplicates = nrow(df.temp) - nrow(distinct(df.temp))
    if (n_duplicates > 0) {
      warning(ind_id, " has ", n_duplicates, " a relevant dimension might have been exluded\n")
    }
  }
}
