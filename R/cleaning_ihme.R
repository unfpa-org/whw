# This script defines routines for cleaning data from IHME


#' Combine IHME indicators from multiple CSV files
#'
#' This function combines IHME indicators from multiple CSV files into a
#' single tibble.
#'
#' @param filepath A list that maps file names to indicator names contained
#' therein.
#' @return A combined tibble of IHME indicators.
#' @seealso [data4whw::combine_sdg_series_data()] that combines SDG data.
#' @examples
#' combine_ihme_indicators(list("myfile.csv" = "indicator name"))
#' @export
combine_ihme_indicators <- function(file.list, verbose = FALSE) {
  df.list <- list()
  for (name in names(file.list)) {
    path <- file.path("..", "data", "raw", "ihme", name)
    df.indicator <- readr::read_csv(path, show_col_types = FALSE) %>%
      dplyr::mutate(indicator_definition = file.list[[name]])
    df.list[[name]] = df.indicator

    if (isTRUE(verbose)) {
      cat(name)
      cat(names(df.indicator), "\n\n")
    }
  }

  df.ihme <- bind_rows(df.list)
  return(df.ihme)
}

#' Preprocess IHME indicators in line with the template.
#'
#' Preprocessess a combined dataframe of IHME indicators in line with
#' the template. In particular, renames the columns, removes redundant
#' disaggregation, streamlines data types and adds relevant columns. It
#' also runs sanity checks to validate the output.
#'
#' @param df.ihme A tibble of indicator data as returned by `combine_ihme_indicators`.
#' @param country2iso A list mapping country names to iso-alpha-3 codes.
#' @return A cleaned tibble of IHME indicators.
#' @seealso [data4whw::combine_ihme_indicators()] that combines IHME indicators.
#' @examples
#' combine_ihme_indicators(df.ihme, country2iso)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
clean_ihme_indicators <- function(df.ihme, country2iso) {
  to.remove <- c("measure_name", "cause_name", "rei_name", "upper", "lower")
  to.rename <- list(
    "country" = "location_name",
    "iso3code" = "iso3code",
    "indicator_definition" = "indicator_definition",
    "year" = "year",
    "source" = "source",
    "source_details" = "source_details",
    "source_id" = "source_id",
    "units" = "metric_name",
    "sex" = "sex_name",
    "age" = "age_name",
    "value" = "val",
    "nature" = "nature"
  )

  df.ihme %<>%
    #dplyr::select(-matches("_id"), -dplyr::all_of(to.remove)) %>%
    dplyr::mutate(
      iso3code = dplyr::recode(location_name, !!!country2iso),
      source = "IHME",
      source_details = "IHME",
      source_id = NA,
      nature = "E"
    ) %>%
    dplyr::rename(!!!to.rename) %>%
    dplyr::select(dplyr::all_of(names(to.rename))) %>%
    dplyr::mutate(
      sex = dplyr::recode(
        sex,
        "Both" = "_T",
        "Female" = "FEMALE",
        "Male" = "MALE"
      ),
      age = dplyr::recode(
        age,
        "All ages" = "_T"
      ),
      age = stringr::str_replace(age, " years", ""),
      units = dplyr::recode(
        units,
        "Percent" = "PERCENT",
        "Rate" = "RATE"
      )
    )

  #validate_output_ihme(df.ihme)
  return(df.ihme)
}

#' Validate output dataframe using sanity checks.
#'
#' An internal function to validate the data produces within `clean_ihme_indicators`.
#' Currently only checks if duplicates result from data processing. If so,
#' issues a warning.
#'
#' @param df.output A tibble of indicator data within `clean_ihme_indicators`.
#' @return Nothing
#' @seealso [data4whw::clean_ihme_indicators()] that calls this function internally.
#' @importFrom magrittr %>%
validate_output_ihme <- function(df.output) {
  indentifiers <- c("country", "indicator_definition", "year", "sex", "age")
  n.unique.rows <- df.output %>%
    select(all_of(c(indentifiers, "units"))) %>%
    distinct() %>%
    nrow()
  non.iso <- df.output %>%
    select(country, iso3code) %>%
    distinct() %>%
    filter(str_length(iso3code) > 3) %>%
    pull(iso3code)

  if (n.unique.rows != nrow(df.output)) {
    warning(nrow(df.output) - nrow, " duplicates have been found.")
  } else if (length(non.iso) > 0) {
    warning(str_c(non.iso, collapse = ", "), " have not been mapped to iso.")
  } else {
    cat("All good.")
  }
}
