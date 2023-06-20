# This script defines routines for cleaning data from UNICEF


#' Rename columns, recode values in UNICEF indicators
#'
#' This function cleans UNICEF indicators by renaming columns and recoding values.
#'
#' @param df.unicef An input dataframe of combined UNICEF indicators
#' therein.
#' @return A cleaned tibble of UNICEF indicators.
#' @seealso [data4whw::get_unicef()] that retrieves UNICEF data.
#' @examples
#' clean_unicef(df.unicef)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
clean_unicef <- function(df.unicef) {
  to.rename <- list(
    "iso3code" = "REF_AREA",
    "country" = "Country",
    "indicator_id" = "INDICATOR",
    "indicator_definition" = "Indicator",
    "age" = "AGE",
    "wealth" = "WEALTH_QUINTILE",
    "residence" = "RESIDENCE",
    "education" = "MATERNAL_EDU_LVL",
    "year" = "TIME_PERIOD",
    "sex" = "Sex",
    "source_details" = "DATA_SOURCE",
    "observation_status" = "Observation.Status",
    "units" = "UNIT_MEASURE",
    "value" = "OBS_VALUE"
  )

  df.unicef <- df.unicef %>% dplyr::rename(!!!to.rename) %>%
    dplyr::select(names(to.rename)) %>%
    dplyr::mutate(
      education = dplyr::case_when(
        education == "_T" ~ "_T",
        education == "AGG_0_1" ~ "NO_PRI_EDU",
        education == "AGG_2_3" ~ "SEC_EDU",
        education == "AGG_3S_H" ~ "SEC_HIGH_EDU",
        education == "AGG_5T8" ~ "HIGH_EDU",
        education == "ISCED11_1" ~ "PRI_EDU",
        education == "ISCED11A_01" ~ "NO_EDU"
      ),
      nature = dplyr::case_when(
        observation_status == "Reported" ~ "C",
        observation_status == "Reanalysed" ~ "E",
        observation_status == "Adjusted" ~ "CA",
        observation_status == "Estimated value" ~ "E",
        observation_status == "External Reanalysis" ~ "E",
        observation_status == "Low reliability" ~ "U"
      ),
      indicator_definition = dplyr::case_when(
        indicator_definition == "Antenatal care 4+ visits - percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider" ~ "Antenatal care 4+ visits",
        indicator_definition == "Height-for-age <-2 SD (stunting)" ~ "Prevalence of stunting among children under 5 years of age",
        TRUE ~ indicator_definition
      ),
      source = "UNICEF",
      units = "PERCENT"
  )

  # denoting the latest data point before removing month-day information from year
  df.unicef <- df.unicef %>%
    dplyr::mutate(
      date = dplyr::if_else(stringr::str_length(year) == 4, sprintf("%s-01-01", year), year),
      year = stringr::str_extract(year, "^\\d{4}") %>% as.numeric()
    ) %>%
    dplyr::group_by(indicator_id, iso3code, age, wealth, residence, education, sex, year) %>%
    dplyr::mutate(latest_data_trend = date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-date)

  return(df.unicef)
}
