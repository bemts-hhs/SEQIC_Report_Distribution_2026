###_____________________________________________________________________________
### Loading data for the distribution of SEQIC reports in 2026
### All data for these reports will be 2021 - 2025 with 2025 being the 'current'
### year. Date of data download/processing from the Iowa Trauma Registry:
### [DATE] for 2025 data, for 2021-2024 data - [DATE]
###_____________________________________________________________________________

# Secure creds and path
path_2020 <- Sys.getenv("FILE_PATH_2021")
path_2021 <- Sys.getenv("FILE_PATH_2022")
path_2022 <- Sys.getenv("FILE_PATH_2023")
path_2023 <- Sys.getenv("FILE_PATH_2024")
path_2024 <- Sys.getenv("FILE_PATH_2025")

# population files environment variables ----
iowa_county_pops_path <- Sys.getenv("IOWA_COUNTY_POPS")
iowa_county_age_pops_path <- Sys.getenv("IOWA_COUNTY_AGE_POPS")
us_standard_pops_path <- Sys.getenv("US_STANDARD_POPS")
iowa_state_age_pops_path <- Sys.getenv("IOWA_STATE_POPS")
iowa_counties_districts_path <- Sys.getenv("iowa_counties_districts")

# load files from source
trauma_2021 <-
  readr::read_csv(
    path_2021
  )
trauma_2022 <-
  readr::read_csv(
    path_2022
  )
trauma_2023 <-
  readr::read_csv(
    path_2023
  )
trauma_2024 <-
  readr::read_csv(
    path_2024
  )
trauma_2025 <-
  readr::read_csv(
    path_2025
  )

# Get data into one file for all years for reporting across years
# Although identical processing workflows were followed for all files, the zip
# codes seem to get formatted differently between years, fix those
trauma_2021_2025 <- dplyr::bind_rows(
  trauma_2021 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2022 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2023 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2024 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2025 |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("_zip$|_fips$"),
        ~ as.character(.)
      ),
      Level = ifelse(
        Facility_Name ==
          "Southeast Iowa Regional Medical Center, West Burlington",
        "III",
        Level
      ),
      Level_I_II = ifelse(
        Facility_Name ==
          "Southeast Iowa Regional Medical Center, West Burlington",
        "Level III",
        Level_I_II
      )
    )
) |>
  dplyr::mutate(
    Level_I_II = ifelse(
      `Current Facility Name` ==
        "UnityPoint Health - Trinity Regional Medical Center" &
        ED_Acute_Care_Admission_Date < as.Date("2018-04-01"),
      "Level III",
      ifelse(
        `Current Facility Name` == "Mahaska Health" &
          ED_Acute_Care_Admission_Date < as.Date("2018-07-01"),
        "Level III",
        ifelse(
          `Current Facility Name` == "CHI Health Mercy Council Bluffs" &
            ED_Acute_Care_Admission_Date < as.Date("2018-10-01"),
          "Level IV",
          ifelse(
            `Current Facility Name` ==
              "UnityPoint Health - Marshalltown Hospital" &
              ED_Acute_Care_Admission_Date < as.Date("2018-12-01"),
            "Level III",
            ifelse(
              `Current Facility Name` ==
                "UnityPoint Health - Grinnell Regional Medical Center" &
                ED_Acute_Care_Admission_Date < as.Date("2019-01-01"),
              "Level III",
              ifelse(
                `Current Facility Name` ==
                  "MercyOne North Iowa Medical Center" &
                  ED_Acute_Care_Admission_Date < as.Date("2020-07-01"),
                "Level I & II",
                ifelse(
                  `Current Facility Name` == "Pella Regional Health Center" &
                    ED_Acute_Care_Admission_Date < as.Date("2020-08-01"),
                  "Level III",
                  ifelse(
                    `Current Facility Name` ==
                      "Genesis Medical Center, Davenport" &
                      ED_Acute_Care_Admission_Date < as.Date("2021-10-01"),
                    "Level I & II",
                    ifelse(
                      `Current Facility Name` ==
                        "Southeast Iowa Regional Medical Center, West Burlington" &
                        ED_Acute_Care_Admission_Date < as.Date("2021-12-01"),
                      "Level III",
                      ifelse(
                        `Current Facility Name` ==
                          "Southeast Iowa Regional Medical Center, West Burlington" &
                          ED_Acute_Care_Admission_Date >= as.Date("2024-12-01"),
                        "Level III",
                        ifelse(
                          `Current Facility Name` ==
                            "Southeast Iowa Regional Medical Center, West Burlington" &
                            ED_Acute_Care_Admission_Date >=
                              as.Date("2021-12-01") &
                            ED_Acute_Care_Admission_Date <
                              as.Date("2024-12-01"),
                          "Level IV",
                          ifelse(
                            `Current Facility Name` ==
                              "UnityPoint Health - St. Luke's Hospital, Sioux CIty" &
                              ED_Acute_Care_Admission_Date <
                                as.Date("2024-10-01"),
                            "Level I & II",
                            Level_I_II
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ) |>
  dplyr::mutate(
    Injury_County = stringr::str_to_title(Injury_County),
    Injury_County = dplyr::if_else(
      grepl(pattern = "o'b", x = Injury_County, ignore.case = TRUE),
      "O'Brien",
      Injury_County
    )
  ) |>
  dplyr::mutate(
    Age_Group = dplyr::case_when(
      Patient_Age_Years < 5 ~ "0-4",
      Patient_Age_Years >= 5 & Patient_Age_Years < 10 ~ "5-9",
      Patient_Age_Years >= 10 & Patient_Age_Years < 15 ~ "10-14",
      Patient_Age_Years >= 15 & Patient_Age_Years < 20 ~ "15-19",
      Patient_Age_Years >= 20 & Patient_Age_Years < 25 ~ "20-24",
      Patient_Age_Years >= 25 & Patient_Age_Years < 30 ~ "25-29",
      Patient_Age_Years >= 30 & Patient_Age_Years < 35 ~ "30-34",
      Patient_Age_Years >= 35 & Patient_Age_Years < 40 ~ "35-39",
      Patient_Age_Years >= 40 & Patient_Age_Years < 45 ~ "40-44",
      Patient_Age_Years >= 45 & Patient_Age_Years < 50 ~ "45-49",
      Patient_Age_Years >= 50 & Patient_Age_Years < 55 ~ "50-54",
      Patient_Age_Years >= 55 & Patient_Age_Years < 60 ~ "55-59",
      Patient_Age_Years >= 60 & Patient_Age_Years < 65 ~ "60-64",
      Patient_Age_Years >= 65 & Patient_Age_Years < 70 ~ "65-69",
      Patient_Age_Years >= 70 & Patient_Age_Years < 75 ~ "70-74",
      Patient_Age_Years >= 75 & Patient_Age_Years < 80 ~ "75-79",
      Patient_Age_Years >= 80 & Patient_Age_Years < 85 ~ "80-84",
      Patient_Age_Years >= 85 ~ "85+",
      TRUE ~ "Missing",
      .default = "Missing"
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+",
        "Missing"
      )
    ),
    .after = Age_Range
  )

###_____________________________________________________________________________
# census bureau standard pops 2020-2025 census ----
# documentation here:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/
###_____________________________________________________________________________

# 2020-2025 Census Bureau County population data ----
# get years for each county population
county_pops_all <- readr::read_csv(
  file = iowa_county_pops_path
)

# get columns of interest for the county-level population data ----
county_pops_select <- county_pops_all |>
  dplyr::filter(STATE == "19", COUNTY != "000") |>
  dplyr::select(County = CTYNAME, tidyselect::matches("popestimate\\d{4}$")) |>
  tidyr::pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "County_Population"
  ) |>
  dplyr::mutate(
    Year = stringr::str_remove(string = Year, pattern = "POPESTIMATE"),
    Year = forcats::as_factor(as.numeric(Year)),
    County = stringr::str_squish(stringr::str_remove_all(
      County,
      pattern = "\\sCounty"
    )),
    County = stringr::str_to_title(County),
    County = dplyr::if_else(
      grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
      "O'Brien",
      County
    )
  )

# Iowa county pops by age group ----

# ingest data
age_group_pops <- readr::read_csv(iowa_county_age_pops_path)

# 2020-2025 data Iowa county population data by age group
age_group_pops_final <- age_group_pops |>
  dplyr::select(
    CTYNAME,
    YEAR,
    tidyselect::matches(
      "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
      ignore.case = TRUE
    ),
    POPESTIMATE
  ) |>

  # Year #1 here is the base year at 4/1/2020
  dplyr::filter(YEAR != 1) |> # 7/1/2020 - 7/1/2024 pop estimates

  # Add 2018 to each year so that 2 == 2020, 3 == 2021, 4 == 2022, 5 == 2023,
  # and 6 == 2024, 7 == 2025, which is the intended meaning
  dplyr::mutate(YEAR = 2018 + YEAR) |>
  dplyr::rename(Year = YEAR) |>
  tidyr::pivot_longer(
    cols = AGE04_TOT:AGE85PLUS_TOT,
    names_to = "Age_Group",
    values_to = "County_Age_Population"
  ) |>
  dplyr::mutate(
    Age_Group = stringr::str_extract(Age_Group, pattern = "\\d+"),
    Age_Group = dplyr::if_else(
      Age_Group == "85",
      "85+",
      dplyr::if_else(
        nchar(Age_Group) == 2,
        paste0(
          stringr::str_sub(Age_Group, 1, 1),
          "-",
          stringr::str_sub(Age_Group, 2, 2)
        ),
        dplyr::if_else(
          nchar(Age_Group) == 4,
          paste0(
            stringr::str_sub(Age_Group, 1, 2),
            "-",
            stringr::str_sub(Age_Group, 3, 4)
          ),
          "Missing"
        )
      )
    ),
    CTYNAME = stringr::str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
  ) |>
  dplyr::rename(County = CTYNAME, County_Population = POPESTIMATE) |>
  dplyr::relocate(County_Population, .after = County_Age_Population)

###_____________________________________________________________________________
# age group populations for Iowa at the state (not county) level ----
# work with the sc-est[year]-agesex-civ.csv files for this via
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/state/asrh/
###_____________________________________________________________________________

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

# Iowa age groups at the state level for 2018-2022
# clean Iowa age group populations
# these are NOT standard populations
us_state_age_pops <- readr::read_csv(file = iowa_state_age_pops_path)

# get Iowa state age populations
state_age_group_pops <- us_state_age_pops |>

  # Iowa == State #19, and SEX == 0 is for totals not sex-specific
  dplyr::filter(STATE == 19, SEX == 0) |>
  dplyr::select(
    -c(SUMLEV, REGION, DIVISION, STATE, NAME, SEX, ESTBASE2020_CIV)
  ) |>
  tidyr::pivot_longer(
    cols = -AGE,
    names_to = "Year",
    values_to = "Population"
  ) |>
  dplyr::mutate(
    Year = as.numeric(stringr::str_extract(string = Year, pattern = "\\d+")),
    Age_Group = dplyr::if_else(
      AGE < 5,
      "0-4",
      dplyr::if_else(
        AGE >= 5 & AGE < 10,
        "5-9",
        dplyr::if_else(
          AGE >= 10 & AGE < 15,
          "10-14",
          dplyr::if_else(
            AGE >= 15 & AGE < 20,
            "15-19",
            dplyr::if_else(
              AGE >= 20 & AGE < 25,
              "20-24",
              dplyr::if_else(
                AGE >= 25 & AGE < 30,
                "25-29",
                dplyr::if_else(
                  AGE >= 30 & AGE < 35,
                  "30-34",
                  dplyr::if_else(
                    AGE >= 35 & AGE < 40,
                    "35-39",
                    dplyr::if_else(
                      AGE >= 40 & AGE < 45,
                      "40-44",
                      dplyr::if_else(
                        AGE >= 45 & AGE < 50,
                        "45-49",
                        dplyr::if_else(
                          AGE >= 50 & AGE < 55,
                          "50-54",
                          dplyr::if_else(
                            AGE >= 55 & AGE < 60,
                            "55-59",
                            dplyr::if_else(
                              AGE >= 60 & AGE < 65,
                              "60-64",
                              dplyr::if_else(
                                AGE >= 65 & AGE < 70,
                                "65-69",
                                dplyr::if_else(
                                  AGE >= 70 & AGE < 75,
                                  "70-74",
                                  dplyr::if_else(
                                    AGE >= 75 & AGE < 80,
                                    "75-79",
                                    dplyr::if_else(
                                      AGE >= 80 & AGE < 85,
                                      "80-84",
                                      dplyr::if_else(
                                        AGE >= 85 & AGE < 999,
                                        "85+",
                                        dplyr::if_else(
                                          AGE >= 999,
                                          "Total",
                                          "Error",
                                          missing = "Error"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+",
        "Total",
        "Error"
      )
    )
  ) |>
  dplyr::select(Year, Age_Group, Population) |>
  dplyr::summarize(
    State_Population = sum(Population, na.rm = TRUE),
    .by = c(Year, Age_Group)
  )

# standard US populations
us_age_pops <- readr::read_tsv(
  file = "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
  col_names = FALSE
)

# clean US standard populations
us_age_pops_clean <- us_age_pops |>
  dplyr::filter(grepl(pattern = "^204", x = X1)) |>
  dplyr::mutate(
    Age_Group = stringr::str_sub(X1, start = 4, end = 6),
    Population = stringr::str_sub(X1, start = 7, end = 14),
    Population = as.numeric(stringr::str_remove(Population, pattern = "^0")),
    Weight = round(Population / sum(Population), digits = 6)
  ) |>
  dplyr::select(-X1) |>
  dplyr::mutate(
    Age_Group = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85+"
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+"
      )
    )
  ) |>
  dplyr::rename(US_Population = Population)
