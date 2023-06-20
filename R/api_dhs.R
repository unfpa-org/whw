#' This script contains functions to retrieve data and metadata via DHS API.
#'
#' \code{fetch_data} retrieves DHS API data and returns a dataframe with arguments to specify countries, years, indicators, tag, and level of disaggregation.
#' @details Set DHS API key with \code{\link{set_api_key}} and return fields with \code{\link{set_return_fields}}.
#' @param countries Specify countries by DHS country code. Use \code{fetch_countries} for listing of DHS country codes.
#' @param years Specify survey year(s)
#' @param indicators Specify indicators by indicator_id. Use \code{fetch_indicators} for indicator details including indicator_ids.
#' @param tag Specify tag. Use \code{fetch_tags} for details. NOTE: only 1 tag can be specified per call.
#' @param breakdown_level Specify level of data disaggregation. Options include "national" (default), "subnational", "background", and "all".
#' @param add_geometry Set to "true" for inclusion of polygon coordinates.
#' @seealso \code{\link{fetch_countries}}, \code{\link{fetch_indicators}}
#' @return A dataframe with arguments to specify countries, years, indicators, tag, and level of disaggregation.
#' @examples
#' # Return all immunization tagged data for Ghana and Kenya from 2010 to 2016
#' fetch_data(countries = c("GH", "KE"), years = 2010:2016, tag = 32, breakdown_level = "all")
#'
#' # Return all national-level data for "Assistance during delivery from a skilled provider" and
#' "Treatment of diarrhea: Taken to a health facility"
#' fetch_data(indicators = c("RH_DELA_C_SKP", "CH_DIAT_C_ADV"), breakdown_level = "national")
#'
#' # Return all 2016 data with geometry for the tag "SDGs"
#' fetch_data(years = 2016, tag = 80, add_geometry = "true")
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
fetch_data <- function(countries = NULL, years = NULL, indicators = NULL, tag = NULL, breakdown_level = NULL, add_geometry = NULL) {

  # check args
  attempt::stop_if_all(c(countries, years, indicators, tag, breakdown_level), is.null, "Please specify at least one argument")
  attempt::stop_if(tag, ~ length(.x) > 1, "Only one tag may be specified per call")

  # collapse args with multiple inputs
  countries  <- stringr::str_c(countries, collapse = ",")
  years      <- stringr::str_c(years, collapse = ",")
  indicators <- stringr::str_c(indicators, collapse = ",")
  tag        <- stringr::str_c(tag, collapse = ",")

  # optional args
  returnFields <- dplyr::if_else(is.null(getOption("return_fields")), "", getOption("return_fields"))    # from set_return_fields()
  apiKey       <- dplyr::if_else(is.null(getOption("api_key")), "", getOption("api_key"))                # from set_api_key()
  perpage      <- dplyr::if_else(is.null(getOption("api_key")), "1000", "5000")                          # 1000 max [unauthenticated]; 5000 max [authenticated]

  # build args list
  args <- purrr::compact(
    list(
      countryIds = countries,
      surveyYear = years,
      indicatorIds = indicators,
      tagIds = tag,
      breakdown = breakdown_level,
      returnGeometry = add_geometry,
      returnFields = returnFields,
      apiKey = apiKey,
      perpage = perpage
    )
  )

  # dhs api
  base_url <- "http://api.dhsprogram.com/rest/dhs/data?"

  # api call
  res <- httr::GET(base_url, query = args)
  url <- res[["url"]]

  # grab total pages
  n_pgs <- jsonlite::fromJSON(rawToChar(res[["content"]]))[["TotalPages"]]

  # total page message
  if (n_pgs == 0) {
    stop("No data available for these parameters", call. = FALSE)
  } else if (n_pgs == 1) {
    message(stringr::str_c(n_pgs," page to extract"))
  } else {
    message(stringr::str_c(n_pgs," pages to extract"))
  }

  # fetch data
  extract_json <- function(pages) {
    # call status
    message("Retrieving page ", pages)

    # api call
    args <- c(args, page = pages)
    res <- httr::GET(base_url, query = args)

    jsonlite::fromJSON(rawToChar(res[["content"]]))[["Data"]] %>%
      # coerce inconsistent return field types
      dplyr::mutate_at(dplyr::vars(dplyr::contains("SurveyYearLabel")), dplyr::funs(as.character)) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::matches("CI|Denominator")), dplyr::funs(as.numeric))
  }

  # output
  df <- purrr::map_df(seq_len(n_pgs), extract_json) %>%
    dplyr::as_data_frame() %>%
    dplyr::rename_all(snakecase::to_snake_case) %>%
    dplyr::na_if(., "")

  return(list(df = df, url = url))
}


#' Fetch country listing
#'
#' \code{fetch_countries} returns a dataframe of all countries with available DHS survey data and their associated DHS and ISO codes.
#' @seealso \code{\link{fetch_data}}
#' @importFrom magrittr %>%
#' @export
fetch_countries <- function() {
  jsonlite::fromJSON("https://api.dhsprogram.com/rest/dhs/countries?returnFields=CountryName,DHS_CountryCode,iso3_CountryCode") %>%
    .$Data %>%
    dplyr::rename_all(snakecase::to_snake_case) %>%
    dplyr::as_data_frame()
}


# Select indicators and start extraction
#indicator_list_w <- c("RH_PAHC_W_PR1", "AH_HINS_W_NON","HA_STIS_W_STI","AN_NUTS_W_NRM") # women indicators
#indicator_list_m <- c("AH_HINS_M_NON","HA_STIS_M_STI") # men indicators
#dhs_df <- fetch_data(indicators = c(indicator_list_w, indicator_list_m), breakdown_level = "all")
#
#dhs_raw <- dhs_df$df # Raw data
#country_list <- fetch_countries() # Country list with DHS code
