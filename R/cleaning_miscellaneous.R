#' This script contains functions to retrieve and reshape data from WPP, WWBI, JME, UNICEF
#'


clean_wb_indicators <- function(df.wb) {

  to.rename <- list(
    iso3code = "Country Code",
    indicator_definition = "Indicator Name",
    indicator_id = "Indicator Code"
  )

  # remove junk columns starting with triple dots
  df.wb %>%
    dplyr::select(-c("Country Name"), -matches("\\.\\.\\.")) %>%
    tidyr::pivot_longer(
      cols = dplyr::one_of(as.character(1920:2022)),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::rename(!!!to.rename) %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(
      source = "WB",
      nature = "E",
      source_details = case_when(
        indicator_definition == "Account (% age 15+)" ~ "Global Findex Database (World Bank)",
        indicator_definition == "Account, female (% age 15+)" ~ "Global Findex Database (World Bank)",
        indicator_definition == "Account, male (% age 15+)" ~ "Global Findex Database (World Bank)",
        indicator_definition == "Female to male wage ratio in the public sector (using mean)" ~ "Worldwide Bureaucracy Indicators (World Bank)",
        indicator_definition == "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)" ~ "ILOSTAT database"
      ),
      units = case_when(
        indicator_definition == "Account (% age 15+)" ~ "PERCENT",
        indicator_definition == "Account, female (% age 15+)" ~ "PERCENT",
        indicator_definition == "Account, male (% age 15+)" ~ "PERCENT",
        indicator_definition == "Female to male wage ratio in the public sector (using mean)" ~ "RATIO",
        indicator_definition == "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)" ~ "RATIO")
    )
}

clean_unpd_indicators <- function(df.unpd) {
  to.rename <- list(
    iso3code = "Iso3",
    indicator_id = "IndicatorId",
    indicator_definition = "Indicator",
    year = "TimeLabel",
    source_details = "Source",
    age = "AgeLabel",
    sex = "Sex",
    marital_status = "Category",
    value = "Value"
  )
  df.unpd %<>% dplyr::filter(Variant == "Median") %>%
    dplyr::rename(!!!to.rename) %>%
    dplyr::select(all_of(names(to.rename))) %>%
    dplyr::mutate(
      source = "UNPD",
      nature = "E",
      units = dplyr::case_when(
        indicator_definition == "Demand for family planning satisfied by any modern method (Percent)" ~ "PERCENT",
        indicator_definition == "Life expectancy at birth" ~ "YEARS")
    )

  # columns, except for value, need to uniquely identify each row
  n.unique.rows <- df.unpd %>%
    dplyr::select(-value) %>%
    dplyr::distinct() %>%
    nrow()

  stopifnot(n.unique.rows == nrow(df.unpd))

  return(df.unpd)
}

clean_unesco_indicators <- function(df.unesco) {
  to.rename <- list(
    iso3code = "COUNTRY_ID",
    indicator_id = "INDICATOR_ID",
    indicator_definition = "INDICATOR_LABEL_EN",
    year = "YEAR",
    source_details = "METADATA",
    value = "VALUE"
  )
  df.unesco %<>% dplyr::rename(!!!to.rename) %>%
    dplyr::select(names(to.rename)) %>%
    dplyr::mutate(
      source = "UNESCO",
      nature = "C",
      units = "RATIO"
    )
  return(df.unesco)
}

clean_who <- function(df.who) {
  to.select <- c(
    "Country Code", "Year", "Sex", "Age Group",
    "Age-standardized death rate per 100 000 standard population",
    "Death rate per 100 000 population"
  )

  to.rename <- list(
    "iso3code" = "Country Code",
    "year" = "Year",
    "sex" = "Sex",
    "age" = "Age Group"
  )
  df.who %>%
    dplyr::select(dplyr::all_of(to.select)) %>%
    tidyr::pivot_longer(
      cols = to.select[5:6],
      names_to = "indicator_definition",
      values_to = "value",
      values_drop_na = T
    ) %>%
    dplyr::rename(!!!to.rename) %>%
    dplyr::mutate(
      age = stringr::str_replace_all(age, "\\[|\\]", ""),
      age = dplyr::if_else(age == "All", "_T", age),
      sex = dplyr::if_else(sex == "All", "_T", sex),
      sex = dplyr::if_else(sex == "Female", "FEMALE", sex),
      sex = dplyr::if_else(sex == "Male", "Male", sex),
      source = "WHO",
      source_details = "WHO Mortality Database",
      nature = "E",
      units = dplyr::case_when(
        indicator_definition == "Age-standardized death rate per 100 000 standard population" ~ "PER_100000_POP",
        indicator_definition == "Death rate per 100 000 population" ~ "PER_100000_POP"
      ),
      indicator_definition = dplyr::case_when(
        indicator_definition == "Age-standardized death rate per 100 000 standard population" ~ "Age-standardized death rate per 100 000 standard population: Breast cancer",
        indicator_definition == "Death rate per 100 000 population" ~ "Death rate per 100 000 population: Breast cancer"
      )
    )

}

clean_gho <- function(df.gho) {
  to.rename <- list(
    "indicator_id" = "IndicatorCode",
    "iso3code" = "SpatialDim",
    "sex" = "Dim1",
    "year" = "TimeDimensionValue",
    "value" = "NumericValue"
  )
  df.gho %>%
    dplyr::rename(!!!to.rename) %>%
    dplyr::select(dplyr::all_of(names(to.rename))) %>%
    dplyr::mutate(
      sex = dplyr::recode(
        sex,
        "BTSX" = "_T",
        "FMLE" = "FEMALE",
        "MLE" = "MALE"
      ),
      age = "_T",
      source = "WHO",
      source_details = "Global Health Observatory (WHO)",
      source_id = "NCD_HYP_DIAGNOSIS_A",
      units = "PERCENT",
      reporting_type = "G",
      nature = "E",
      residence = "_T",
      wealth = "_T",
      indicator_definition = "Prevalence of hypertension among adults aged 30-79"
    )
}

combine_miscellaneous_indicators <- function(path) {
  df.list <- list()

  for (filename in list.files(path)) {
    if (!stringr::str_ends(filename, ".xlsx")) {
      next
    }

    filepath <- file.path(path, filename)
    df.temp <- openxlsx::read.xlsx(xlsxFile = filepath)
    df.list[[filename]] <- df.temp %>% mutate(
      year = as.numeric(year),
      indicator_id = as.character(indicator_id)
    )
    #file.remove(filepath)
  }

  df.combined <- bind_rows(df.list)
  return(df.combined)
}

clean_whr <- function(df.whr) {
  df.whr %>%
    rename(value = `Happiness score`) %>%
    select(iso3code, value) %>%
    mutate(
      indicator_id = NA,
      indicator_definition = "Happiness score - SWB (Subjective Well-being)",
      source = "Gallup World Poll",
      nature = "E",
      year = 2022,
      units = "SCORE"
    )
}
