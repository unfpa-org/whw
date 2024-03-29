% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_unsd_sdgs.R
\name{download_sgd_series_data}
\alias{download_sgd_series_data}
\title{Download SDG indicators to an Excel file.}
\usage{
download_sgd_series_data(codes, filepath, verbose = FALSE)
}
\arguments{
\item{codes}{A character vector of series codes.}

\item{filepath}{A full path including a filename and extension.}

\item{verbose}{Set to TRUE to print the dimensions of each series table.}
}
\value{
Contents of the parsed response converted into a tibble.
}
\description{
Given a list of indicators, writes each indicator into a separate sheet of
an Excel file in a specified filepath.
}
\examples{
download_sgd_series_data(c("SI_POV_NAHC", "SI_COV_CHLD"), "temp.xlsx")
}
\seealso{
\code{\link[=exctract_series_list]{exctract_series_list()}} which returns all the available series together with their codes.
}
