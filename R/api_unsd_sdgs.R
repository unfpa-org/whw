# This script defines routines for interacting with UNSD SDG API.


#' Make a GET request to UNSD SDG API
#'
#' A generic function to make a GET request to United Nations Statistics Division SDG API.
#' See the [SDGs API documentation](https://unstats.un.org/sdgapi/swagger/) for a
#' list of available commands.
#'
#' @param command An API request to submit.
#' @return Contents of the response (parsed).
#' @seealso [data4whw::post_sdg_api()] which makes a POST request and accepts a `body` parameter.
#' @examples
#' get_sdg_api("/v1/sdg/Indicator/List")
#' get_sdg_api("/v1/sdg/Goal/List")
#' @importFrom magrittr %>%
#' @export
get_sdg_api <- function(command) {
  endpoint <- "https://unstats.un.org/sdgapi"
  link <- paste(endpoint, command, sep = "")
  response <- httr::GET(link) %>% httr::content(as = "parsed")
  return(response)
}

#' Make a POST request to UNSD SDG API
#'
#' A generic function to make a POST request to United Nations Statistics Division SDG API.
#' See the [SDGs API documentation](https://unstats.un.org/sdgapi/swagger/) for a
#' list of available commands.
#'
#' @param command An API request to submit.
#' @param body A body of the POST request.
#' @return Contents of the parsed response converted into a tibble.
#' @seealso [data4whw::get_sdg_api()] which makes a GET request returnes a response (parsed).
#' @examples
#' post_sdg_api("/v1/sdg/Series/DataCSV", list(seriesCodes = c("VC_DSR_MORT"), areaCodes = c(528)))
#' @importFrom magrittr %>%
#' @export
post_sdg_api <- function(command, body) {
  endpoint <- "https://unstats.un.org/sdgapi"
  link <- paste(endpoint, command, sep = "")
  response <- httr::POST(link, body = body) %>%
    httr::content(as = "text", type = "text/csv", encoding = "UTF-8") %>%
    readr::read_csv(show_col_types = FALSE)
  return(response)
}

#' Extract a list of all available SDG indicators.
#'
#' This function is used to retrieve a list of all indicators available via UNSD SDGs API
#' in a tabular format.
#'
#' @return A list of indicators and their metadata converted into a tibble.
#' @examples
#' exctract_series_list()
#' @importFrom magrittr %>%
#' @export
exctract_sdg_series_list <- function() {
  indicators <- get_sdg_api("/v1/sdg/Indicator/List")
  series.list <- list()

  # processing indicators one by one
  for (i in 1:length(indicators)) {
    indicator <- indicators[[i]]

    # some indicators have empty series
    if (rlang::is_empty(indicator$series)) {
      next
    }

    # processing each series within an indicator
    for (j in 1:length(indicator$series)) {
      series <- indicator$series[[j]]
      idx <- length(series.list) + 1
      series.list[[idx]] <- unlist(series)
    }
  }

  # converting a list of all series to a tibble
  df.series <- series.list %>%
    purrr::transpose() %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(cols = dplyr::everything())

  return(df.series)
}

#' Download SDG indicators to an Excel file.
#'
#' Given a list of indicators, writes each indicator into a separate sheet of
#' an Excel file in a specified filepath.
#'
#' @param codes A character vector of series codes.
#' @param filepath A full path including a filename and extension.
#' @param verbose Set to TRUE to print the dimensions of each series table.
#' @return Contents of the parsed response converted into a tibble.
#' @seealso [data4whw::exctract_series_list()] which returns all the available series together with their codes.
#' @examples
#' download_sgd_series_data(c("SI_POV_NAHC", "SI_COV_CHLD"), "temp.xlsx")
#' @importFrom magrittr %>%
#' @export
download_sgd_series_data <- function(codes, filepath, verbose = FALSE) {
  series.data <- list()

  # processing each series code one by one
  for (code in codes) {
    series.data[[code]] <- post_sdg_api(
      command = "/v1/sdg/Series/DataCSV",
      body = list(seriesCodes = c(code))
    )

    # printing dimensions of the data
    if (isTRUE(verbose)) {
      series.data[[code]] %>% dim() %>% cat(code, ., "\n")
    }
  }

  # saving all the dataframes at once
  openxlsx::write.xlsx(x = series.data, file = filepath)
}
