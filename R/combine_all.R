# This script defines function for combining and harmonising all sources

#' Combine cleaned indicator data from each source into a single table.
#'
#' This function combined cleaned data from SDG, IHME, DHS and other sources
#' into a single dataframe.
#'
#' @param paths A character vector of file paths to cleaned datasets.
#' @return A combined dataframe of data from all sources.
#' @export
combine_indicators <- function(paths) {
  source.list <- list()

  for (path in paths) {
    file.extension <- tools::file_ext(path)

    if (file.extension == "xlsx") {
      source.list[[path]] <- openxlsx::read.xlsx(path) %>%
        mutate(
          year = as.integer(year), # may be stored as characters
          value = as.character(value) # may not be coersible to numeric, e.g. "<5"
        )
    } else if (file.extension == "csv") {
      source.list[[path]] <- readr::read_csv(path) %>%
        mutate(
          year = as.integer(year),
          value = as.character(value)
        )
    } else {
      cat("Unknown extension for ", path)
    }

  }
  df.sources <- dplyr::bind_rows(source.list)
  # TODO: add validation
  return(df.sources)
}
