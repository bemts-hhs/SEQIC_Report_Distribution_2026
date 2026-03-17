###_____________________________________________________________________________
### Loading data for the distribution of SEQIC reports in 2026
### All data for these reports will be 2021 - 2025 with 2025 being the 'current'
### year. Date of data download/processing from the Iowa Trauma Registry:
### [DATE] for 2025 data, for 2021-2024 data - [DATE]
###_____________________________________________________________________________

# Secure creds and path ----
path_2021 <- Sys.getenv("FILE_PATH_2021")
path_2022 <- Sys.getenv("FILE_PATH_2022")
path_2023 <- Sys.getenv("FILE_PATH_2023")
path_2024 <- Sys.getenv("FILE_PATH_2024")
path_2025 <- Sys.getenv("FILE_PATH_2025")

# Open connection to location file ----
iowa_counties_districts_path <- Sys.getenv("iowa_counties_districts")

###_____________________________________________________________________________
# Get location data into memory for calculations
###_____________________________________________________________________________

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

###_____________________________________________________________________________
# Trauma data
###_____________________________________________________________________________

# load files from source ----
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

# Get data into one file for all years for reporting across years ----
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
