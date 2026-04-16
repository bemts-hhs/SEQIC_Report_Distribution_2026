###_____________________________________________________________________________
### Produce the Risk Adjusted Mortality Metrics for Each Trauma Center
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2026.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

# Clean the data and prepare for TRISS analyses ----
trauma_2021_2025_clean <- trauma_2021_2025 |>
  dplyr::filter(!is.na(Probability_of_Survival_Calc)) |>
  dplyr::mutate(Alive = ifelse(Death, 0, 1)) |>
  dplyr::group_by(Year) |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::ungroup()

# Facility Level Performance Metrics ----

# given that traumar does not provide a grouping structure for the
# traumar::trauma_performance() function, we will use tidyr::nest() to
# do the grouping

## Facility Level W, M, and Z Scores ----

### Facility Level iterate over groups - Year and facility ----
trauma_performance_result_years <- trauma_2021_2025_clean |>
  tidyr::nest(data = -c(Year, `Current Facility Name`)) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Year)

### Facility Level iterate over trauma type groups ----
### facility, mechanism of injury do not add year as a stratum given that some
### counts will be very small in groups
trauma_performance_result_mech <- trauma_2021_2025_clean |>
  tidyr::nest(data = -c(`Current Facility Name`, Trauma_Type)) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Trauma_Type) |>
  dplyr::relocate(Trauma_Type, .after = `Current Facility Name`) |>
  dplyr::mutate(
    Trauma_Type = ifelse(is.na(Trauma_Type), "Missing", Trauma_Type)
  )

### Facility Level iterate over TTA groups ----
### facility, mechanism of injury do not add year as a stratum given that some
### counts will be very small in groups
trauma_performance_result_tta <- trauma_2021_2025_clean |>
  tidyr::nest(
    data = -c(`Current Facility Name`, Trauma_Team_Activated)
  ) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Trauma_Team_Activated) |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Current Facility Name`)

## Facility Level Relative Mortality Metric ----
rm_summary_results <- trauma_2021_2025_clean |>
  dplyr::group_by(Year, `Current Facility Name`) |>
  dplyr::group_split() |>
  purrr::map(
    .x = _,
    ~ dynamic_rm_bin_summary(
      data = .x,
      Ps_col = Probability_of_Survival_Calc,
      outcome_col = Alive,
      group_vars = c("Year", "Current Facility Name"),
      n_samples = 100,
      bootstrap_ci = TRUE,
      seed = 10232015
    )
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(!is.na(`Current Facility Name`)) |>
  dplyr::arrange(`Current Facility Name`, Year, bin_number)

# Calculate metrics at the state level ----
## State Level W, M, and Z scores ----
### State Level iterate over groups - Year and facility ----
trauma_performance_result_years_state <- trauma_2021_2025_clean |>
  tidyr::nest(data = -Year) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(Year)

### State Level iterate over trauma type groups ----
### facility, mechanism of injury do not add year as a stratum given that some
### counts will be very small in groups
trauma_performance_result_mech_state <- trauma_2021_2025_clean |>
  tidyr::nest(data = -Trauma_Type) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(Trauma_Type) |>
  dplyr::mutate(
    Trauma_Type = ifelse(is.na(Trauma_Type), "Missing", Trauma_Type)
  )

### State Level iterate over TTA groups ----
### facility, mechanism of injury do not add year as a stratum given that some
### counts will be very small in groups
trauma_performance_result_tta_state <- trauma_2021_2025_clean |>
  tidyr::nest(
    data = -Trauma_Team_Activated
  ) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(Trauma_Team_Activated)

## State Level Relative Mortality Metric ----
rm_summary_results_state <- trauma_2021_2025_clean |>
  tidyr::nest(data = -Year) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ dynamic_rm_bin_summary(
        data = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Alive,
        group_vars = NULL,
        n_samples = 1000,
        bootstrap_ci = TRUE,
        seed = 10232015
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(Year, bin_number)

# EXPORT ----

## Facility Level Exports ----

### Export Facility Level W, M, and Z scores grouped by year ----
export_seqic_data(
  agency_names = unique(trauma_2025$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = trauma_performance_result_years,
  indicator = "_wmz_years"
)

### Export Facility Level W, M, and Z scores grouped by mechanism of injury ----
export_seqic_data(
  agency_names = unique(trauma_2025$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = trauma_performance_result_mech,
  indicator = "_wmz_mechanism"
)

### Export Facility Level W, M, and Z scores grouped by TTA ----
export_seqic_data(
  agency_names = unique(trauma_2025$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = trauma_performance_result_tta,
  indicator = "_wmz_tta"
)

### Export Facility Level RMM statistics by year ----
export_seqic_data(
  agency_names = unique(trauma_2025$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = rm_summary_results,
  indicator = "_rmm_years"
)

## export performance by year and region at the state level ----

### STATE LEVEL W, M, and Z scores grouped by year ----
export_state_data(
  x = trauma_performance_result_years_state,
  subfolder = "rmm_wmz"
)

### STATE LEVEL W, M, and Z scores grouped by mechanism of injury ----
export_state_data(
  x = trauma_performance_result_mech_state,
  subfolder = "rmm_wmz"
)

### STATE LEVEL W, M, and Z scores grouped by whether a TTA was called ----
export_state_data(
  x = trauma_performance_result_tta_state,
  subfolder = "rmm_wmz"
)

### STATE LEVEL RMM statistics by year ----
export_state_data(
  x = rm_summary_results_state,
  subfolder = "rmm_wmz"
)
