###_____________________________________________________________________________
# Estimate age adjusted rates of injury prevalence resulting in inpatient hospitalization ----
# based on the trauma registry
###_____________________________________________________________________________

# Estimate Counts ----

## total trauma cases ----
trauma_cases_years <- trauma_2020_2024 |>
  injury_case_count(Year, descriptive_stats = TRUE)

## check overall trauma case counts with injury location of Iowa ----
trauma_years_iowa <- trauma_2020_2024 |>
  dplyr::filter(
    grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
    !is.na(Injury_County),
    !Injury_County %in%
      c(
        "Not Applicable",
        "Not Known",
        "Not Known / Not Recorded",
        "Not Known/Not Recorded",
        "Rock Island"
      )
  ) |>
  injury_case_count(Year) # close to the annual trauma report, but is a different file if not filtering out non-Iowa incidents 

# get injury counts and case counts by year, county, and age group ----

  ## injuries ----
  injury_counts <- trauma_2020_2024 |>
    dplyr::filter(
      grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
      !is.na(Injury_County),
      !Injury_County %in%
        c(
          "Not Applicable",
          "Not Known",
          "Not Known / Not Recorded",
          "Not Known/Not Recorded",
          "Rock Island"
        )
    ) |>
    # it seems that registrars may enter incident_state as the state the patient is from
    # observed Iowa counties paired with other states, which is improbable that it was meant as the county of another state
    injury_incident_count(Year, Injury_County, Age_Group) |> # as the states in this dataset seem to have a very, very good spelling match with Iowa county names, so assumption is county and zip code are better sources
    tidyr::complete(Year, Injury_County, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, Injury_County, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::left_join(
      age_group_pops_final,
      by = c("Injury_County" = "County", "Age_Group", "Year")
    ) |>
    dplyr::rename(Count = n) |>
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(us_age_pops_clean, by = "Age_Group") |>
    dplyr::mutate(Rate_Type = "Injury_Count")

  ## cases ----
  # often mentioned as 'incidents'
  case_counts <- trauma_2020_2024 |>
    injury_case_count(Year, County, Age_Group) |>
    tidyr::complete(Year, County, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, County, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::left_join(
      age_group_pops_final,
      by = c("County", "Age_Group", "Year")
    ) |>
    dplyr::rename(Count = n) |>
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(us_age_pops_clean, by = "Age_Group") |>
    dplyr::mutate(Rate_Type = "Case_Count")

# get injury counts and case counts by year and age group ----

  ## injury counts by age group -----
  iowa_injury_counts_age <- trauma_2020_2024 |>
    dplyr::filter(
      grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
      !is.na(Injury_County),
      !Injury_County %in%
        c(
          "Not Applicable",
          "Not Known",
          "Not Known / Not Recorded",
          "Not Known/Not Recorded",
          "Rock Island"
        )
    ) |>
    # it seems that registrars may enter incident_state as the state the patient is from
    # observed Iowa counties paired with other states, which is improbable that it was meant as the county of another state
    # as the states in this dataset seem to have a very, very good spelling match with Iowa county names, so assumption is county and zip code are better sources
    injury_incident_count(Year, Age_Group) |>
    tidyr::complete(Year, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
    dplyr::left_join(
      us_age_pops_clean,
      by = "Age_Group"
    ) |>
    dplyr::rename(Count = n)

  ## get case counts ----

  iowa_case_counts_age <- trauma_2020_2024 |>
    injury_case_count(Year, Age_Group) |>
    tidyr::complete(Year, Age_Group, fill = list(n = 0L)) |>
    dplyr::arrange(Year, Age_Group) |> # returns 99 counties, and more accurate counts
    dplyr::filter(Age_Group != "Missing") |>
    dplyr::left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
    dplyr::left_join(
      us_age_pops_clean,
      by = "Age_Group"
    ) |>
    dplyr::rename(Count = n)

## check for missingness ----
# if this check produces many missing age_groups
# go back to the trauma_data_final file and
# review the DOB / Incident dates to see what is driving
# the missingness
trauma_counts_na <- injury_counts |>
  dplyr::filter(dplyr::if_any(tidyselect::everything(), ~ is.na(.)))

### check the trauma_data_final file for missing DOB / Incident_Date ----
trauma_data_na <- trauma_2020_2024 |>
  dplyr::filter(dplyr::if_any(c(Patient_DOB, Incident_Date), ~ is.na(.)))

### run this to get a big picture of where the NAs are at ----
trauma_2020_2024 |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = T) |>
  dplyr::count(Year, Age_Group) |>
  dplyr::filter(Age_Group == "Missing") |>
  print(n = Inf)

###_____________________________________________________________________________
# Trauma data age adjustments ----
###_____________________________________________________________________________

## rates summarized by year and county ----

  ### injuries ----
  injury_rates <- injury_counts |>
    calc_age_adjusted_rate(
      count = Count,
      local_population = County_Age_Population,
      standard_population_weight = Weight,
      .by = c("Year", "Injury_County", "Rate_Type")
    ) |>
    dplyr::mutate(
      pretty_label = ifelse(
        Year %in% c(2020, 2023, 2024),
        traumar::pretty_number(
          x = Age_Adjusted_Rate,
          n_decimal = 2
        ),
        ""
      ),
      .by = Injury_County
    ) |>
    dplyr::left_join(location_data, by = c("Injury_County" = "County"))

  ### cases / incidents ----
  case_rates <- case_counts |>
    calc_age_adjusted_rate(
      count = Count,
      local_population = County_Age_Population,
      standard_population_weight = Weight,
      .by = c("Year", "County", "Rate_Type")
    ) |>
    dplyr::mutate(
      pretty_label = ifelse(
        Year %in% c(2020, 2023, 2024),
        traumar::pretty_number(
          x = Age_Adjusted_Rate,
          n_decimal = 2
        ),
        ""
      ),
      .by = County
    ) |>
    dplyr::left_join(location_data, by = "County")

  ### union the by year and county age adjusted rates ----
  injury_case_rates <- dplyr::bind_rows(
    injury_rates |> dplyr::rename(County = Injury_County),
    case_rates
  )

###_____________________________________________________________________________
# rates at the state level ----
###_____________________________________________________________________________

  ## injuries ----
  iowa_injury_rate <- iowa_injury_counts_age |>
    calc_age_adjusted_rate(
      count = Count,
      local_population = State_Population,
      standard_population_weight = Weight,
      .by = "Year",
      rate = 100000
    ) |>
    dplyr::mutate(Rate_Category = "Injury_Count", .before = Year)

  ## cases / incidents ----
  iowa_case_rate <- iowa_case_counts_age |>
    calc_age_adjusted_rate(
      count = Count,
      local_population = State_Population,
      standard_population_weight = Weight,
      .by = "Year",
      rate = 100000
    ) |>
    dplyr::mutate(Rate_Category = "Case_Count", .before = Year)

  ## union the state rates ----
  iowa_injury_case_rates <- dplyr::bind_rows(iowa_injury_rate, iowa_case_rate)

# EXPORTS ----

## county-level estimates ----

### injury and case rates at the county level ---- 
export_state_data(x = injury_case_rates, output_dir_root = paste0(state_output_folder, "rates"), subfolder = "")

## state-level estimates ----

### injury and case rates at the state level ----
export_state_data(x = iowa_injury_case_rates, output_dir_root = paste0(state_output_folder, "rates"), subfolder = "")
