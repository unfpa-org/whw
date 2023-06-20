# This script defines routines for interacting with miscellaneous APIs, e.g.
# the World Bank, UNPD, UNESCO


get_wb_api <- function(command) {
  endpoint <- "http://api.worldbank.org/v2/"
  link <- paste(endpoint, command, "?format=json", sep = "")
  response <- httr::GET(link) %>% httr::content(as = "parsed")
  return(response)
}

get_wb_indicators <-function(indicators, source_code, folder.path = "../data/tmp") {
  # https://datahelpdesk.worldbank.org/knowledgebase/articles/898581-api-basic-call-structures
  # Note: A maximum of 60 indicators can be used.
  # A maximum of 1,500 characters are allowed between two back-slashes (/).
  # A maximum of 4,000 characters are allowed in the entire URL.

  # setting directories
  archive.path <- file.path(folder.path, "tmp.zip")
  unzip.path <- file.path(folder.path, "wb")

  # creating a link
  endpoint <- "http://api.worldbank.org/v2/"
  indicators <- stringr::str_c(indicators, collapse = ";")
  command <- paste("country/all/indicator/", indicators, sep = "")
  link <- paste(endpoint, command, "?source=", source_code, "&downloadformat=csv", sep = "")

  # downloading the archive, unzipping and reading the table
  download.file(url = link, destfile = archive.path, mode = "wb")
  unzip(zipfile = archive.path, exdir = unzip.path)
  filename <- list.files(unzip.path, pattern = "^API_*")
  filepath <- file.path(unzip.path, filename)
  df.wb <- readr::read_csv(filepath, skip = 3) # skipping extra header rows

  # cleaning up by removing temporary files and directories
  unlink(c(archive.path, unzip.path), recursive = T)
  return(df.wb)
}

get_unpd_indicators <- function(indicators) {
  #link <- "https://population.un.org/dataportalapi/api/v1/indicators?sort=id&format=csv"
  #df.indicators <- read_delim(link, delim = "|", skip = 1)
  #df.indicators %>% filter(ShortName %in% c("DEMMod", "E0")) %>% select(Id, Name)


  # getting all country location ids
  link <- "https://population.un.org/dataportalapi/api/v1/locations?sort=id&format=csv"
  df_locations <- readr::read_delim(link, delim = "|", skip = 1)
  locations <- df_locations %>%
    #dplyr::filter(LocationType == "Country") %>%
    drop_na(Longitude, Latitude) %>% # LocationType was removed, regions seem to have no geodata
    dplyr::pull(Id) %>%
    stringr::str_c(collapse = ",")

  endpoint <- "https://population.un.org/dataportalapi/api/v1"
  link <- paste(
    endpoint,
    "/data/indicators/",
    stringr::str_c(indicators, collapse = ","),
    "/locations/",
    locations,
    "/start/1950/end/2030?format=csv",
    sep = ""
  )

  df.unpd <- readr::read_delim(link, delim = "|", skip = 1)
  return(df.unpd)
}

get_unesco_indicators <- function(indicators, folder.path = "../data/tmp") {
  # SDG Global and Thematic Indicators
  # https://apiportal.uis.unesco.org/bdds

  # setting directories
  archive.path <- file.path(folder.path, "tmp.zip")
  unzip.path <- file.path(folder.path, "unesco")


  link <- "https://apimgmtstzgjpfeq2u763lag.blob.core.windows.net/content/MediaLibrary/bdds/SDG.zip"
  download.file(url = link, destfile = archive.path)
  unzip(zipfile = archive.path, overwrite = T, exdir = unzip.path)

  #df.country <- readr::read_csv("../data/tmp/unesco/SDG_COUNTRY.csv")

  # data file in the archive
  filepath <- file.path(unzip.path, "SDG_DATA_NATIONAL.csv")
  df.national <- readr::read_csv(filepath) %>%
    dplyr::filter(INDICATOR_ID %in% indicators)

  filepath <- file.path(unzip.path, "SDG_LABEL.csv")
  df.labels <- read_csv(filepath)
  df.national <- left_join(
    x = df.national,
    y = df.labels,
    by = "INDICATOR_ID"
  )

  filepath <- file.path(unzip.path, "SDG_METADATA.csv")
  df.sources <- read_csv(filepath) %>%
    filter(TYPE == "Source") %>%
    select(-TYPE)
  df.national <- left_join(
    x = df.national,
    y = df.sources,
    by = c("INDICATOR_ID", "COUNTRY_ID", "YEAR")
  )

  # cleaning up by removing temporary files and directories
  unlink(c(archive.path, unzip.path), recursive = T)
  return(df.national)
}
