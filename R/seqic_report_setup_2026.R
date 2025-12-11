# IOWA SEQIC REPORT PREP 2024 ----------------------------------------------
# This script prepares for the analyses using the `traumar` package v1.2.0
# For the shapefiles, it is assumed that the files are downloaded from
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php using the year 2024
# and then utilizing the Counties (and equivalent), States (and equivalent), and
# Zip Code Tabulation Areas (ZCTAS) options in the dropdown dialogue to download
# the files manually and put them in the directory used here.

## PACKAGES -------------------------------------------------------------------

## CRAN versions ================================================================

# install these packages if not already
# renv::install(
#   c(
#     "tidyverse",
#     "traumar",
#     "devtools",
#     "remotes",
#     "janitor",
#     "naniar",
#     "devtools",
#     "renv",
#     "roxygen2",
#     "roxygen2md"
#   )
# )

# load packages ----
# library(tidyverse)
# library(traumar)
# library(devtools)
# library(remotes)
# library(traumar)
# library(naniar)
# library(devtools)
# library(renv)
# library(roxygen2)
# library(roxygen2md)
# library(blastula)
# library(emayili)
# library(Microsoft365R)

# Handy Functions --------------------------------------------------------------

# CALCULATION FACILITIES =====================================================

# Apply rm_bin_summary with dynamic parameters based on sample size
#
# This function:
# - Accepts a dataset with propensity scores and binary outcomes.
# - Checks for insufficient data (n < 2) and returns a 1-row tibble with
#   pre-defined structure filled with NA_real_ values.
# - Otherwise, determines the number of observations in the dataset and
#   sets thresholds and divisors dynamically:
#     - ≤100 rows: thresholds (0.8, 0.9), divisors = 1
#     - 101–200 rows: thresholds (0.9, 0.99), divisors = 1
#     - >200 rows: thresholds (0.9, 0.99), divisors = 3
# - Applies `traumar::rm_bin_summary()` with the appropriate parameters,
#   using `{{ }}` to support unquoted column names.
# - Returns a tibble summarizing binary outcome performance across propensity bins,
#   with optional bootstrap-based confidence intervals.
#
# Used to flexibly summarize performance metrics across variable sample sizes,
# including edge-case handling for very small cohorts.
dynamic_rm_bin_summary <- function(
  data,
  Ps_col,
  outcome_col,
  group_vars,
  n_samples,
  bootstrap_ci,
  seed
) {
  # Count observations
  n_obs <- nrow(data)

  # safeguard against n_obs < 2
  if (n_obs < 2) {
    # if n_obs < 2, produce a tibble 1 x 19 with all NA_real_ values for that
    # facility and year
    results <- tibble::tibble(
      bin_number = NA_real_,
      TA_b = NA_real_,
      TD_b = NA_real_,
      N_b = NA_real_,
      EM_b = NA_real_,
      AntiS_b = NA_real_,
      AntiM_b = NA_real_,
      bin_start = NA_real_,
      bin_end = NA_real_,
      midpoint = NA_real_,
      R_b = NA_real_,
      population_RMM_LL = NA_real_,
      population_RMM = NA_real_,
      population_RMM_UL = NA_real_,
      population_CI = NA_real_,
      bootstrap_RMM_LL = NA_real_,
      bootstrap_RMM = NA_real_,
      bootstrap_RMM_UL = NA_real_,
      bootstrap_CI = NA_real_
    )

    # Return that fully missing result
    return(results)
  }

  # Define dynamic parameters based on sample size
  params <- if (n_obs <= 100) {
    list(Divisor1 = 1, Divisor2 = 1, Threshold_1 = 0.8, Threshold_2 = 0.9)
  } else if (n_obs <= 200) {
    list(Divisor1 = 1, Divisor2 = 1, Threshold_1 = 0.9, Threshold_2 = 0.99)
  } else {
    list(Divisor1 = 3, Divisor2 = 3, Threshold_1 = 0.9, Threshold_2 = 0.99)
  }

  # Call rm_bin_summary with dynamic thresholds/divisors
  results <- traumar::rm_bin_summary(
    data = data,
    Ps_col = {{ Ps_col }},
    outcome_col = {{ outcome_col }},
    group_vars = group_vars,
    n_samples = n_samples,
    Divisor1 = params$Divisor1,
    Divisor2 = params$Divisor2,
    Threshold_1 = params$Threshold_1,
    Threshold_2 = params$Threshold_2,
    bootstrap_ci = bootstrap_ci,
    seed = seed
  )

  # Return the results as a tibble
  return(results)
}

###_____________________________________________________________________________
### Helper to summarize reinjury counts after filtering for reinjuries (n > 1)
### Input:
###   - df: tibble with counts column `n` (injury counts per patient per group)
###   - grouping_vars: character vector for grouping variable names (for .by)
### Output:
###   - tibble summarizing min, max, mode, quartiles, and median of reinjury counts
###_____________________________________________________________________________
summarize_reinjury_stats <- function(df, grouping_vars = grouping_vars) {
  df |>
    dplyr::filter(n > 1) |> # retain only reinjuries (counts > 1)
    dplyr::summarize(
      Min_Reinjury = min(n, na.rm = TRUE),
      Max_Reinjury = max(n, na.rm = TRUE),
      Mode_Reinjury = stat_mode(n, na.rm = TRUE),
      Q25_Reinjury = stats::quantile(n, probs = 0.25, na.rm = TRUE, type = 7),
      Median_Reinjury = stats::quantile(n, probs = 0.5, na.rm = TRUE, type = 7),
      Q75_Reinjury = stats::quantile(n, probs = 0.75, na.rm = TRUE, type = 7),
      .by = dplyr::all_of(grouping_vars)
    )
}

###_____________________________________________________________________________
### Helper function to compute period-to-period change in counts
### Adds three columns:
###   - change: raw numeric difference in count (n - lag(n))
###   - prop_change: proportional change ((n - lag(n)) / lag(n))
###   - prop_label: formatted percent label using traumar::pretty_percent()
### This assumes that the input data is already grouped and counted, with a column `n`
###_____________________________________________________________________________
add_change_metrics <- function(df) {
  df |>
    dplyr::mutate(
      # Compute raw numeric change from previous row
      change = n - dplyr::lag(n),

      # Compute proportional change (as a fraction)
      prop_change = (n - dplyr::lag(n)) / dplyr::lag(n),

      # Generate a human-readable percent label (e.g., "14.2%")
      # Use NA_character_ if proportional change is NA (e.g., first row)
      prop_label = ifelse(
        !is.na(prop_change),
        traumar::pretty_percent(prop_change, n_decimal = 2),
        NA_character_
      )
    )
}

###_____________________________________________________________________________
### Custom function to get unique count of injury cases in Patient Registry
### Equivalent to counting unique inpatient visits based on Unique_Incident_ID
### Supports dynamic grouping via tidy evaluation (bare column names in ...)
###_____________________________________________________________________________
injury_case_count <- function(df, ..., descriptive_stats = FALSE) {
  # Capture grouping variables as symbols and convert to character strings for .by and joins
  grouping_syms <- rlang::ensyms(...)
  grouping_vars <- sapply(grouping_syms, rlang::as_string)

  # Create temporary dataset with unique incident rows
  # Unique incident defined by Unique_Incident_ID (one row per inpatient visit)
  temp <- df |>
    dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE)

  if (!descriptive_stats) {
    # Simple count of unique cases by user-defined grouping variables
    out <- temp |>
      dplyr::count(...)

    cli::cli_alert_success(
      "Returning the count(s) of total unique inpatient injury cases."
    )

    return(out)
  }

  # When descriptive_stats = TRUE, add change metrics (numeric and percent change)
  out <- temp |>
    dplyr::count(...) |>
    add_change_metrics()

  cli::cli_alert_success(
    "Returning the count(s) of total unique inpatient injury cases and descriptive statistics."
  )

  return(out)
}

###_____________________________________________________________________________
### Custom function to get unique count of injuries in Patient Registry
### A true estimation of the number of incidents (not encounters or records)
###_____________________________________________________________________________
injury_incident_count <- function(df, ..., descriptive_stats = FALSE) {
  # Capture grouping variables from bare column names (e.g., Year, County)
  grouping_syms <- rlang::ensyms(...) # capture as symbols for tidy eval
  grouping_vars <- sapply(grouping_syms, rlang::as_string) # convert to character for .by and join

  # Create a temporary object with one row per unique injury event
  # A unique event is defined as a unique combination of Incident_Date and Unique_Patient_ID
  # This removes duplicated encounters for the same incident
  temp <- df |>
    dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

  if (!descriptive_stats) {
    # Return a simple count of injury events by grouping variable(s)
    out <- temp |>
      dplyr::count(!!!grouping_syms)

    cli::cli_alert_success(
      "Returning the count(s) of total unique injury events leading to a trauma center visit."
    )

    return(out)
  }

  # When descriptive_stats = TRUE, include additional summary statistics
  # This captures reinjury characteristics for patients who appear >1 time per group

  # Step 1: Identify reinjury patterns (count > 1 per group)
  stat <- temp |>
    dplyr::filter(!is.na(Unique_Patient_ID)) |>
    dplyr::count(!!!grouping_syms, Unique_Patient_ID) |>
    summarize_reinjury_stats(grouping_vars = grouping_vars)

  # Step 2: Count events and calculate change metrics
  out <- temp |>
    dplyr::count(!!!grouping_syms) |>
    add_change_metrics() |>
    dplyr::left_join(stat, by = grouping_vars) # <- this fixes the join

  cli::cli_alert_success(
    "Returning the count(s) of total unique injury events leading to a trauma center visit and descriptive statistics."
  )

  return(out)
}

###_____________________________________________________________________________
# Function to calculate age-adjusted EMS run rates by county and BH district
# This function computes directly standardized (age-adjusted) rates using
# pre-aggregated data that includes event counts, local population estimates,
# and standard population weights.
#
# Assumptions:
#   • The input data has already been grouped (e.g., by County and Age Group).
#   • {{ count }}, {{ local_population }}, and {{ standard_population_weight }}
#     are scalar fields within these grouped rows.
#   • The user provides any necessary grouping variables via ... to summarize
#     results at the desired geographic or demographic resolution.
#
# This function returns both crude and age-adjusted rates per specified
# multiplier (default is per 100,000 population).
#
# Inputs:
#   • data — a grouped or ungrouped data.frame or tibble with input variables
#   • count — unquoted column name representing the event count
#   • local_population — unquoted column name for the local population (e.g., county-age)
#   • standard_population_weight — unquoted column name for the standard weight (proportional)
#   • ... — grouping variables to aggregate final rates (e.g., County, District)
#   • rate — numeric value for scaling rates (default: 100,000)
#
# Output:
#   • A tibble with Count, Crude_Rate, and Age_Adjusted_Rate per grouping
###_____________________________________________________________________________

calc_age_adjusted_rate <- function(
  data, # input tibble or data.frame with grouped or stratified rows
  count, # unquoted column name for observed event count (e.g., EMS runs)
  local_population, # unquoted column name for stratum-specific local population
  standard_population_weight, # unquoted column name for proportional weight from standard population
  .by = NULL, # grouping variables for aggregating final rates (e.g., 'County', 'District')
  rate = 100000 # rate multiplier (e.g., per 1000, 10000, or 100000)
) {
  # Step 1: Calculate age-specific crude rate and weighted contribution
  rate_data <- data |>
    dplyr::mutate(
      crude_rate = ({{ count }} / {{ local_population }}) * rate, # rate within each age group
      weighted_rate = crude_rate * {{ standard_population_weight }} # contribution to adjusted rate
    )

  # Step 2: Aggregate to final grouping level
  rate_summary <- rate_data |>
    dplyr::summarize(
      Count = sum({{ count }}, na.rm = TRUE), # total count of events
      Crude_Rate = sum({{ count }}, na.rm = TRUE) /
        sum({{ local_population }}, na.rm = TRUE) *
        rate, # overall crude rate
      Age_Adjusted_Rate = sum(weighted_rate, na.rm = TRUE), # final adjusted rate
      .by = tidyselect::all_of({{ .by }}) # group by user-supplied variables (e.g., County)
    )

  return(rate_summary)
}

# DATA MANIPULATION FACILITIES ===============================================

###_____________________________________________________________________________
# Standard output from traumar is in wide format, with separate columns for each
# SEQIC indicator's numerator, denominator, and calculated value (seqic_*).
#
# The following metadata and helper functions facilitate transforming that wide
# output into a long, tidy format that is easier to analyze, visualize, and
# interpret.
#
# Recommended workflow:
# 1. Compute indicators using traumar::seqic_indicator_*() functions.
# 2. Pass the resulting wide-format data to reshape_seqic_indicators()
#    to pivot it into long format with separate rows per indicator.
# 3. Use match_seqic_indicator() to append indicator names and goals
#    by matching on the indicator code.
# 4. Compute comparison service area and trauma facility verification level
# results
# 5. Use format_seqic_comparison() to clean up service area and comparable trauma
# facility verification results
# 6. Join the service area and comparable verified trauma center data to the
# agency-level data to provide a type of benchmark
###_____________________________________________________________________________

# seqic_indicator_names
# A reference data frame containing SEQIC indicator metadata used for mapping,
# labeling, and evaluation of performance metrics. It includes:
#
# Columns:
# - Indicator: Character code identifying each SEQIC indicator (e.g., "1a", "9b_all").
# - Indicator_Name: Descriptive name of the indicator, suitable for labeling or reporting.
# - Goal: Numeric target threshold (if applicable) representing the performance benchmark
#         for the indicator. Values are `NA_real_` where no explicit goal is defined.
#
# This dataset supports automated indicator classification and goal evaluation within
# trauma and EMS quality assurance workflows.
{
  seqic_indicator_names <- data.frame(
    Indicator = c(
      "1a",
      "1b",
      "1c",
      "1d",
      "1e",
      "1f",
      "2",
      "3",
      "4a",
      "4b",
      "5a",
      "5b",
      "5c",
      "5d",
      "6",
      "7",
      "8_all",
      "8_risk",
      "9a_all",
      "9b_all",
      "9c_all",
      "9d_all",
      "9e_all",
      "9f_all",
      "9a_activations",
      "9b_activations",
      "9c_activations",
      "9d_activations",
      "9e_activations",
      "9f_activations",
      "9a_risk",
      "9b_risk",
      "9c_risk",
      "9d_risk",
      "9e_risk",
      "9f_risk",
      "9a_activations_risk",
      "9b_activations_risk",
      "9c_activations_risk",
      "9d_activations_risk",
      "9e_activations_risk",
      "9f_activations_risk",
      "10a",
      "10b",
      "10c",
      "11",
      "12",
      "13"
    ),
    Indicator_Name = c(
      "Indicator 1a - Trauma Surgeon Responding Within 15 Minutes",
      "Indicator 1b - Trauma Surgeon Responding Within 30 Minutes",
      "Indicator 1c - Trauma Surgeon Response Time Unknown",
      "Indicator 1d - Physician Responding Within 5 Minutes",
      "Indicator 1e - Physician Responding Within 20 Minutes",
      "Indicator 1f - Physician Response Time Unknown",
      "Indicator 2 - Injury Time Blank",
      "Indicator 3 - Probability of Survival Calculated",
      "Indicator 4a - Deceased Trauma Patient Autopsied",
      "Indicator 4b - No Autopsy on Death with LOS > 72 Hours",
      "Indicator 5a - Blood ETOH Measured",
      "Indicator 5b - Blood ETOH Positive",
      "Indicator 5c - Drug Screen Completed",
      "Indicator 5d - Drug Screen Positive",
      "Indicator 6 - GCS Less Than 9 And Arrived to Definitive Care > 3 Hours from Injury",
      "Indicator 7 - Patients to Definitive Care > 3 Hours",
      "Indicator 8 - Overall Survival Rate",
      "Indicator 8 - Survival Rate by Risk Group",
      "Indicator 9a - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 9a By TTA - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b By TTA - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c By TTA - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d By TTA - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e By TTA - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f By TTA - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 9a By Risk Group - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b By Risk Group - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c By Risk Group - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d By Risk Group - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e By Risk Group - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f By Risk Group - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 9a By TTA and Risk Group - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b By TTA and Risk Group - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c By TTA and Risk Group - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d By TTA and Risk Group - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e By TTA and Risk Group - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f By TTA and Risk Group - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 10a - Major Trauma With Limited-to-No TTA at Definitive Care (Cribari)",
      "Indicator 10b - Minor Trauma With the Highest Level TTA at Definitive Care",
      "Indicator 10c - Major Trauma With Limited-to-No TTA at Definitive Care (Modified Cribari)",
      "Indicator 11 - ISS < 9 With ED LOS < 24 Hours Among Patients Transferred to Definitive Care",
      "Indicator 12 - Incidents Submitted Within 60 Days of Patient Discharge",
      "Indicator 13 - Incidents with Validity Score > 84"
    ),
    Goal = c(
      0.80,
      0.80,
      0,
      0.80,
      1,
      0,
      0.25,
      0.90,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      0,
      0,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      0.05,
      0.35,
      NA_real_,
      NA_real_,
      0.80,
      0.90
    )
  )
}

# Reshape SEQIC indicator columns from wide to long format
#
# Expects input data where each indicator (e.g., 1, 1a, 2, 8, etc.) has its own
# set of columns, such as:
#   numerator_8, denominator_8, seqic_8, lower_ci_8, upper_ci_8, etc.
#
# This function:
# - Optionally renames columns dynamically using user-defined regex patterns. Uses
# `pivot_longer()` with a regex pattern to split wide-format indicator data into
# long format.
# - Extracts both the value type (numerator, denominator, seqic, lower_ci,
# upper_ci) and the indicator code (e.g., "8", "8_all") into separate
# components.
# - Uses `.value` in `names_to` to spread the value types into columns.
# - Renames `seqic` to `performance` and standardizes `lower_ci` and `upper_ci` to
# `lower ci` and `upper ci` for readability.
#
# Optional renaming parameters (`rename_cols`, `column_pattern`,
# `match_pattern`, `replace_with`) allow users to dynamically standardize
# indicator suffixes (e.g., converting "_8" to "_8_all").
#
# Returns a long-format tibble with columns: indicator, numerator, denominator,
# performance, lower ci, and upper ci, along with any grouping variables like
# Year, Service Area, etc.

reshape_seqic_indicators <- function(
  data,
  rename_cols = FALSE, # Whether to apply dynamic renaming to columns
  column_pattern = NULL, # Regex pattern to match columns of interest
  match_pattern = NULL, # Regex pattern to match suffixes needing renaming
  replace_with = NULL # Replacement string to apply if renaming is enabled
) {
  # If dynamic renaming is requested but pattern/replacement is missing, throw error
  if (rename_cols) {
    if (
      is.null(match_pattern) || is.null(replace_with) || is.null(column_pattern)
    ) {
      rlang::abort(
        "If `rename_cols = TRUE`, both `match_pattern` and `replace_with` must be provided."
      )
    }

    # Apply regex replacement to any column names matching lower/upper CI patterns
    data <- data |>
      dplyr::rename_with(
        .cols = tidyselect::matches(column_pattern), # Select only CI-related columns
        ~ stringr::str_replace_all(
          .,
          pattern = match_pattern,
          replacement = replace_with
        )
      )
  }

  # Reshape wide-format indicator columns to long format using `pivot_longer()`
  data |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c(
        "numerator_",
        "denominator_",
        "seqic_",
        "lower_ci",
        "upper_ci"
      )),
      names_to = c(".value", "indicator"), # Use `.value` to keep value type columns separate
      names_pattern = "(numerator|denominator|seqic|lower_ci|upper_ci)_(\\d+[a-z]?_?\\w*_?\\w*)"
      # This regex splits names into the value type (e.g., numerator) and the indicator (e.g., 8_all)
    ) |>
    dplyr::rename(
      performance = seqic, # Rename for clarity
      `lower ci` = lower_ci,
      `upper ci` = upper_ci
    )
}


# match_seqic_indicator()
# This function searches for the first matching SEQIC indicator code
# (as defined in `seqic_indicator_names`) within a specified text column.
# It appends three new columns to the input dataframe:
#   - matched_indicator: the first matching indicator code (e.g., "9b_all")
#   - Indicator_Name: the corresponding descriptive name of the indicator
#   - Goal: the numeric target threshold associated with the indicator
#
# Usage:
#   match_seqic_indicator(data, col)
#   - data: a dataframe containing a column with text to search
#   - col: unquoted name of the column to search (uses tidy evaluation)
#
# Note: Only the first match per row is returned. If no match is found,
# all new columns will be NA for that row.
match_seqic_indicator <- function(
  data,
  col,
  indicator_df = seqic_indicator_names,
  performance_col
) {
  # Build regex pattern to match indicators exactly (as whole words)
  pattern <- stringr::str_c(indicator_df$Indicator, collapse = "|")

  data |>
    dplyr::mutate(
      name = dplyr::case_when(
        !is.na({{ col }}) ~
          indicator_df$Indicator_Name[
            match({{ col }}, indicator_df$Indicator)
          ],
        TRUE ~ NA_character_
      ),
      goal = dplyr::case_when(
        !is.na({{ col }}) ~
          indicator_df$Goal[
            match({{ col }}, indicator_df$Indicator)
          ],
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::relocate(name, .after = {{ col }}) |>
    dplyr::relocate(goal, .after = {{ performance_col }})
}

# --- format_seqic_comparison ---
# Generalized function to format SEQIC indicator results for use as
# comparison benchmarks (region or trauma facility level).
#
# Arguments:
# - data: A tibble of wide-format SEQIC indicator results.
# - type: A string specifying the benchmark type, either "region" or "level".
#
# Output:
# A long-format tibble with indicator metadata and a single column renamed to
# reflect the comparison type:
# - "comparison region performance" or
# - "comparison trauma facility performance"
#
# Usage in workflow:
# 1. Calculate comparison data grouped by region or trauma level.
# 2. Use this function to prepare those data for joining with agency-level outputs.

format_seqic_comparison <- function(
  data,
  type = c("region", "level"),
  rename_cols = FALSE, # Whether to apply dynamic renaming to columns
  column_pattern = NULL, # Regex pattern to match columns of interest
  match_pattern = NULL, # Regex pattern to match suffixes needing renaming
  replace_with = NULL # Replacement string to apply if renaming is enabled
) {
  type <- rlang::arg_match(type)

  # Determine appropriate name
  perf_col <- dplyr::case_when(
    type == "region" ~ "comparison region performance",
    type == "level" ~ "comparison trauma facility performance"
  )

  data |>
    reshape_seqic_indicators(
      rename_cols = rename_cols, # Whether to apply dynamic renaming to columns
      column_pattern = column_pattern, # Regex pattern to match columns of interest
      match_pattern = match_pattern, # Regex pattern to match suffixes needing renaming
      replace_with = replace_with # Replacement string to apply if renaming is enabled
    ) |>
    match_seqic_indicator(col = indicator, performance_col = performance) |>
    dplyr::select(-c(name, numerator, denominator, goal)) |>
    dplyr::rename(!!perf_col := performance)
}

# Join comparison benchmarks to SEQIC results and format for reporting
#
# This function merges SEQIC performance results with comparison benchmarks
# at both the state level (`data_level`) and the regional level (`data_region`).
#
# It performs the following operations:
# - Joins `data` to `data_level` by Year, Level I/II designation, and indicator code.
# - Joins `data` to `data_region` by Year, Service Area, and indicator code.
# - Applies `pretty_percent()` formatting from the traumar package to all relevant
#   percentage-based columns, rounding to 2 decimal places and preserving `NA` values.
# - Standardizes all column names to lowercase.
# - Renames `level_i_ii` to `level` for consistency in output.
#
# Expected inputs:
# - `data`: Long-format SEQIC results with columns like Year, Level_I_II, Service Area, and indicator.
# - `data_level`: State-level comparison data, matched on Year, Level_I_II, and indicator.
# - `data_region`: Region-level comparison data, matched on Year, Service Area, and indicator.
#
# Returns a formatted tibble with performance values and comparisons ready for tabular report output.
join_comparison_data <- function(data, data_level, data_region) {
  data |>
    dplyr::left_join(
      data_level,
      by = dplyr::join_by(Year, Level_I_II, indicator)
    ) |>
    dplyr::left_join(
      data_region,
      by = dplyr::join_by(Year, `Service Area`, indicator)
    ) |>
    dplyr::mutate(dplyr::across(
      c(
        performance,
        goal,
        `lower ci`,
        `upper ci`,
        `comparison trauma facility performance`,
        `comparison region performance`
      ),
      ~ ifelse(
        is.na(.),
        NA_real_,
        traumar::pretty_percent(variable = ., n_decimal = 2)
      )
    )) |>
    dplyr::rename_with(~ tolower(.)) |>
    dplyr::rename(level = level_i_ii)
}

# DATA EXPORT FACILITIES =====================================================

###_____________________________________________________________________________
# send_facility_emails()
#
# Sends individualized emails with attachments to facilities listed in an XLSX
# file. Each row in the input file should contain the trauma program manager
# (tpm), email address, facility name (tcf), verification, and file folder name.
#
# For each recipient, the function:
# - Locates the folder of files using a secure base path plus the file folder name
# - Attaches all files found in that folder plus a common guide file
# - Sends an HTML email with a consistent, styled message via Microsoft365R Outlook
# - Logs success/failure for each email to a CSV file
#
# Environment variables used:
#   TPM_CONTACTS - path to XLSX file with contact info
#   FACILITY_PATH - root directory for facility-specific report folders
#   GUIDE_PATH - path to a common guide PDF attached to all emails
#
# Input:
#   recipient_file - path to the XLSX contact file (default: Sys.getenv("TPM_CONTACTS"))
#   base_path     - root folder path for reports (default: Sys.getenv("FACILITY_PATH"))
#   log_path      - path to write the email send log (default: "email_log.csv")
#   guide_path    - path to common guide file (default: Sys.getenv("GUIDE_PATH"))
#
# Requirements:
#   Packages: blastula, Microsoft365R, dplyr, readxl, glue, fs, purrr, tibble, cli
#
# Notes:
#   - Uses purrr::pwalk to iterate rowwise, preserving individual TPM and facility info
#   - HTML email enforces Consolas monospace font, 14px font size, line height 1.4
#   - Bold formatting for sender name and position included in email body
#   - All paths and emails handled securely without hardcoding
###_____________________________________________________________________________
send_facility_emails <- function(
  recipient_file = Sys.getenv("TPM_CONTACTS"), # Path to XLSX file with recipients
  base_path = Sys.getenv("FACILITY_PATH"), # Root folder for facility report files
  log_path = "email_log.csv", # File path for email send log CSV
  guide_path = Sys.getenv("GUIDE_PATH") # Path to a common guide PDF attachment
) {
  # Authenticate and get Microsoft 365 Outlook client for email sending
  outl <- Microsoft365R::get_business_outlook()

  # Read recipient details from Excel file, expecting columns: tpm, email, tcf, file
  recipients <- readxl::read_excel(recipient_file)

  # Initialize an empty tibble to log each email's send attempt and outcome
  log <- tibble::tibble(
    timestamp = as.POSIXct(character()), # Time of email processing attempt
    facility = character(), # Facility name (tcf)
    email = character(), # Recipient email address
    status = character(), # Send status: sent, skipped, failed
    message = character() # Additional status message or error
  )

  # Iterate rowwise over recipient data with purrr::pwalk (parallel walk)
  purrr::pwalk(
    list(
      tpm = recipients$tpm, # Trauma Program Manager name
      email = recipients$email, # Recipient email address
      tcf = recipients$tcf, # Trauma Center Facility name
      file = recipients$file # Facility folder name for attachments
    ),
    function(tpm, email, tcf, file) {
      # Record current timestamp for this email operation
      timestamp <- Sys.time()

      # Inform console of current email processing (for progress tracking)
      cli::cli_inform("Processing email: {email} | file: {file}")

      # Check if the file/folder name is missing or empty; skip if so
      if (is.na(file) || trimws(file) == "") {
        log <<- dplyr::add_row(
          log,
          tibble::tibble(
            timestamp,
            facility = tcf,
            email,
            status = "skipped",
            message = "Missing file location"
          )
        )
        return() # Skip to next iteration
      }

      # Construct full path to the facility folder using base path + file folder
      facility_path <- fs::path(base_path, file)

      # Check if the folder exists; log failure and skip if it does not
      if (!fs::dir_exists(facility_path)) {
        log <<- dplyr::add_row(
          log,
          tibble::tibble(
            timestamp,
            facility = tcf,
            email,
            status = "failed",
            message = "Directory not found"
          )
        )
        return() # Skip to next iteration
      }

      # List all files within the facility folder for attachment
      attachments <- fs::dir_ls(facility_path, type = "file")
      # Combine facility attachments with the guide file to attach both
      all_attachments <- c(attachments, guide_path)

      # Compose the HTML email body with inline CSS for Consolas font and styling
      body_text <- glue::glue(
        '
<div style="font-family: Consolas, monospace; font-size: 14px; line-height: 1.4;">
  <p>Dear {tpm},</p>

  <p>
    Attached is your facility’s (<strong>{tcf}</strong>) risk-adjusted mortality metrics and SEQIC (System Evaluation and Quality Improvement Committee) performance report for the previous five years (including the target reporting year). The attached files include your center’s performance across all 13 SEQIC indicators and are intended to support your local quality improvement efforts.
  </p>

  <p>
    Along with the statistical files, I have also attached a guide to help you navigate the reports. Please use this guide, as it will do a good job of answering most of the questions you will have up front about the statistics.
  </p>

  <p>
    If you have questions about any metrics, methodology, or would like technical assistance interpreting the results, please don’t hesitate to contact us.
  </p>

  <p>Thank you for your continued commitment to trauma system improvement in Iowa.</p>

  <p>
    <strong>Nicolas Foss, Ed.D., MS<br>
    Epidemiologist</strong><br>
    Bureau of Emergency Medical and Trauma Services<br>
    Bureau of Health Statistics<br>
    321 E 12th St., Des Moines, IA 50319<br>
    515-985-9627 mobile<br>
    nicolas.foss@hhs.iowa.gov<br>
    <a href="https://hhs.iowa.gov/public-health/emergency-medical-services-trauma">https://hhs.iowa.gov/public-health/emergency-medical-services-trauma</a><br>
    <a href="https://hhs.iowa.gov/public-health/health-statistics">https://hhs.iowa.gov/public-health/health-statistics</a>
  </p>
</div>
'
      )

      # Create blastula email object with HTML body and simple footer
      body <- blastula::compose_email(
        body = blastula::html(body_text),
        footer = blastula::md("This email was sent via Microsoft365R.")
      )

      # Use tryCatch to handle any errors during email creation and sending
      tryCatch(
        {
          # Create the email object specifying recipient, subject, and body
          em <- outl$create_email(
            body = body,
            subject = glue::glue(
              "CONFIDENTIAL - 2024 Risk Adjusted Mortality Metrics and SEQIC Reports for {tcf}"
            ),
            to = email
          )

          # Add all attachments (facility reports + guide) to the email
          purrr::walk(all_attachments, em$add_attachment)

          # Send the email through Outlook client
          em$send()

          # Log successful send status with "OK" message
          log <<- dplyr::add_row(
            log,
            tibble::tibble(
              timestamp,
              facility = tcf,
              email,
              status = "sent",
              message = "OK"
            )
          )
        },
        error = function(e) {
          # Log any error encountered during email processing with the error message
          log <<- dplyr::add_row(
            log,
            tibble::tibble(
              timestamp,
              facility = tcf,
              email,
              status = "failed",
              message = e$message
            )
          )
        }
      )
    }
  )

  # Write the complete log of all email attempts and results to CSV
  readr::write_csv(log, fs::path(base_path, log_path))
}

### state output folder
state_output_folder <- Sys.getenv("STATE_OUTPUT_FILE_PATH")


# export_state_data
# Export State Data to CSV
#
# This function exports a given data frame or tibble to a CSV file. The file is saved in a dynamically generated
# directory based on the provided root directory and subfolder name. The file name is constructed using the name
# of the input data frame or tibble.
#
# Parameters:
# - x: A data frame or tibble containing the data to be exported.
#
# - output_dir_root: A string specifying the root directory where the output file will be saved. Defaults to the
#   value of the "STATE_OUTPUT_FILE_PATH" environment variable.
#
# - subfolder: A string specifying the subfolder name to be used in the output file path.
#
# Side Effects:
# - Creates the output directory if it does not already exist.
#
# - Writes the data frame or tibble to a CSV file in the specified directory.
#
# Output:
# - Returns `invisible(NULL)`; the primary function is the side-effect of file export.
#
# Workflow Context:
# - Use this function to export state-level data to CSV files for further analysis or reporting.
#
export_state_data <- function(
  x,
  output_dir_root = Sys.getenv("STATE_OUTPUT_FILE_PATH"),
  subfolder
) {
  # Validate input
  # Check if 'x' is a tibble or data frame. If not, abort the function with an error message.
  if (!tibble::is_tibble(x) && !is.data.frame(x)) {
    rlang::abort("`seqic_results` must be a data.frame or tibble.")
  }

  # Get the name of 'x' as a character string
  # This will be used to dynamically create the file name.
  x_name <- deparse(substitute(x))

  # Create dynamic output path
  # Construct the output directory path using the provided root directory.
  output_path <- fs::path(output_dir_root)
  # Create the directory if it does not exist.
  fs::dir_create(output_path)

  # Build file path and write CSV
  # Construct the full file path by combining the output directory, subfolder, and the name of 'x' with a ".csv" extension.
  file_path <- fs::path(
    output_path,
    subfolder,
    paste0(x_name, ".csv")
  )

  # Write the data frame or tibble 'x' to the constructed file path in CSV format.
  readr::write_csv(x = x, file = file_path)
}

# export_seqic_data
# Export SEQIC Indicator Results to CSV by Agency
#
# This function iterates over a vector of agency names and exports SEQIC
# indicator results (from a precomputed results tibble) to individual
# CSV files for each agency. Files are saved in dynamically generated
# subdirectories based on agency name.
#
# Parameters:
# - agency_names: A character vector of agency or facility names. Each name must
#   match values in the `Facility_Name` column of `seqic_results`.
#
# - seqic_results: A data frame or tibble of SEQIC indicator results that
#   includes a `Facility_Name` column used to filter results per agency.
#
# - indicator: A string indicating the indicator group (e.g., "1a_1f",
#   "4a_4b", "all_indicators") to name output files accordingly.
#
# Side Effects:
# - Creates output directories (if not already present) using:
#   `"C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/output/"`
#
# - Writes agency-specific CSV files to the directory with naming convention:
#   `{agency}_{indicator}.csv`
#
# - Logs progress, warnings, and a summary using the `cli` package.
#
# Output:
# - Returns `invisible(NULL)`; primary function is side-effect file export.
#
# Workflow Context:
# - Use this function after calculating and formatting SEQIC results with
#   `traumar::seqic_indicator_*()` and any additional wrangling or annotation.
# - Designed for batch export of customized reports at the agency level.
#
export_seqic_data <- function(
  agency_names,
  facility_name_col,
  seqic_results,
  indicator,
  output_dir_root = Sys.getenv("OUTPUT_FILE_PATH")
) {
  # Validate input
  if (!tibble::is_tibble(seqic_results) && !is.data.frame(seqic_results)) {
    rlang::abort("`seqic_results` must be a data.frame or tibble.")
  }

  exported_count <- 0
  skipped_count <- 0
  agency_count <- length(agency_names)

  for (agency in agency_names) {
    temp <- seqic_results |>
      dplyr::filter({{ facility_name_col }} == agency)

    if (nrow(temp) == 0) {
      cli::cli_warn("Skipped {agency}: no matching rows in `seqic_results`.")
      skipped_count <- skipped_count + 1
      next
    }

    # Create dynamic output path
    output_path <- fs::path(output_dir_root, tolower(agency))
    fs::dir_create(output_path)

    # Build file path and write CSV
    file_path <- fs::path(
      output_path,
      glue::glue("{tolower(agency)}_{indicator}.csv")
    )
    readr::write_csv(x = temp, file = file_path)
    cli::cli_inform(c("v" = "Exported: {file_path}"))
    exported_count <- exported_count + 1
  }

  # Summary
  cli::cli_h1("SEQIC Indicator Export Summary")
  cli::cli_alert_success("Total agencies processed: {agency_count}")
  cli::cli_alert_success("Successfully exported: {exported_count}")
  cli::cli_alert_warning("Skipped (no matching data): {skipped_count}")

  invisible(NULL)
}

###_____________________________________________________________________________
# we will:
# - examine calendar years 2021-2025 of trauma data
# - ingest data and calculate results by agency and year using the `traumar`
# package
# - send 1 file per indicator to each agency, 1 file for W, M, and Z statistics,
# and 1 file for the RMM statistics
###_____________________________________________________________________________
