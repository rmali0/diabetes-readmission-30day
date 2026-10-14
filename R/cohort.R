# Cohort construction: read the raw encounter file and apply the inclusion
# and exclusion steps, keeping a record of how many encounters each step removes.

# Discharge dispositions after which a 30-day readmission cannot happen
# (codes from IDS_mapping.csv in the UCI download):
#   11 Expired; 13 Hospice / home; 14 Hospice / medical facility;
#   19 Expired at home, 20 Expired in a medical facility,
#   21 Expired, place unknown (19 to 21 are Medicaid hospice codes).
# Strack et al. (2014) removed these encounters for the same reason.
DEATH_HOSPICE_DISPOSITIONS <- c(11L, 13L, 14L, 19L, 20L, 21L)

PEDIATRIC_AGE_BANDS <- c("[0-10)", "[10-20)")

INTEGER_COLUMNS <- c(
  "admission_type_id", "discharge_disposition_id", "admission_source_id",
  "time_in_hospital", "num_lab_procedures", "num_procedures",
  "num_medications", "number_outpatient", "number_emergency",
  "number_inpatient", "number_diagnoses"
)

# Read every column as character first so ICD-9 codes such as "V57", "E909"
# or "250.10" keep their original form, then convert the count columns.
read_diabetes_data <- function(path) {
  raw <- readr::read_csv(
    path,
    na = c("?", ""),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
  raw |>
    dplyr::mutate(
      encounter_id = as.numeric(encounter_id),
      patient_nbr = as.numeric(patient_nbr),
      dplyr::across(dplyr::any_of(INTEGER_COLUMNS), as.integer)
    )
}

# The dataset gives no admission dates, so the lowest encounter_id is treated
# as the earliest (index) encounter, as in Strack et al.
first_encounter_per_patient <- function(data) {
  data |>
    dplyr::arrange(encounter_id) |>
    dplyr::distinct(patient_nbr, .keep_all = TRUE)
}

is_death_or_hospice <- function(disposition_id) {
  !is.na(disposition_id) & disposition_id %in% DEATH_HOSPICE_DISPOSITIONS
}

# Returns the analytic cohort plus a step-by-step flow table.
# exclude_death_hospice = FALSE reproduces the cohort used in the course project.
build_cohort <- function(raw, exclude_death_hospice = TRUE) {
  steps <- list()
  record <- function(label, data, excluded_label = NA_character_) {
    steps[[length(steps) + 1]] <<- tibble::tibble(
      step = label,
      n_remaining = nrow(data),
      excluded_reason = excluded_label
    )
    data
  }

  data <- record("All encounters", raw)

  data <- data |>
    dplyr::filter(!age %in% PEDIATRIC_AGE_BANDS) |>
    record("Adult encounters (age 20+)", data = _,
           excluded_label = "Age under 20")

  data <- data |>
    first_encounter_per_patient() |>
    record("First encounter per patient", data = _,
           excluded_label = "Repeat encounters")

  if (exclude_death_hospice) {
    data <- data |>
      dplyr::filter(!is_death_or_hospice(discharge_disposition_id)) |>
      record("Discharged alive, not to hospice", data = _,
             excluded_label = "Died or discharged to hospice")
  }

  # Three patients have sex recorded as Unknown/Invalid; too few to estimate.
  data <- data |>
    dplyr::filter(gender %in% c("Female", "Male")) |>
    record("Analytic cohort", data = _,
           excluded_label = "Sex unknown or invalid")

  flow <- dplyr::bind_rows(steps) |>
    dplyr::mutate(n_excluded = dplyr::lag(n_remaining) - n_remaining)

  list(data = data, flow = flow)
}
