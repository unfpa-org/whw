# This script defines routines for interacting with GHO API.


#' Make a GET request to GHO API
#'
#' A generic function to make a GET request to WHO's Global Health Observatory API.
#' See the [GHO OData API documentation](https://www.who.int/data/gho/info/gho-odata-api) for a
#' list of available commands and indicators.
#'
#' @param command An API request to submit. Can be an indicator ID.
#' @return Contents of the response (parsed).
#' @examples
#' get_gho_api("WHOSIS_000001")
#' get_gho_api("cci2030")
#' @importFrom magrittr %>%
#' @export
get_gho_api <- function(indicator) {
  # indicators of interest are "WHOSIS_000001", "cci2030"
  # "SpatialDim" is iso3
  # "NumericValue is value
  # "TimeDim": year
  # "Dim1Type":"SEX"
  # "Dim1":"FMLE" or "MLE" or "BTSX"

  endpoint <- "https://ghoapi.azureedge.net/api/"
  params <- "?$filter=SpatialDimType eq 'COUNTRY'"
  link <- paste(endpoint, indicator, params, sep = "") %>% URLencode()
  response <- httr::GET(link) %>% httr::content(as = "parsed")
  df.indicator <- dplyr::bind_rows(response$value)
  return(df.indicator)
}

