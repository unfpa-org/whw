# This script defines routines for interacting with UNICEF REST API Service.

#' Make a GET request to UNICEF API
#'
#' See the [UNICEF REST Web Service](https://sdmx.data.unicef.org/webservice/data.html) for a
#' list of available commands and indicators.
#'
#' @param command An API request to submit. Can be an indicator ID.
#' @return Contents of the response (parsed).
#' @examples
#' get_unicef_api(NUTRITION,1.0, list("country" = "", "indicator" = "NT_ANT_HAZ_NE2", "sex" = "", "age" = "", "wealth" = "", "residence" = "", "education" = "", "head_of_house" = ""))
#' @export
get_unicef <- function(dataflow, parameters) {
  # retrieves data from UNICEF SDMX data warehouse

  endpoint <- "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data"
  params <- stringr::str_flatten(parameters, collapse = ".")
  link <- sprintf("%s/%s/%s?format=csv", endpoint, dataflow, params)
  df.unicef <- read.csv(link)

  return(df.unicef)
}
