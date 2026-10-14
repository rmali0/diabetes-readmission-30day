# Recoding raw encounter fields into the analytic variables used in the study.

AGE_LEVELS <- c("[20-30)", "[30-40)", "[40-50)", "[50-60)",
                "[60-70)", "[70-80)", "[80-90)", "[90-100)")

# ICD-9 codes in this file are strings. Numeric codes lost their leading
# zeros ("8" is 008) and supplementary codes start with V or E. Only purely
# numeric codes are converted; V and E codes return NA so they fall into
# "Other" rather than being misread as numbers.
icd9_to_numeric <- function(code) {
  code <- trimws(as.character(code))
  is_numeric_code <- !is.na(code) & grepl("^[0-9]+(\\.[0-9]+)?$", code)
  out <- rep(NA_real_, length(code))
  out[is_numeric_code] <- as.numeric(code[is_numeric_code])
  out
}

# TRUE when a code falls in [lower, upper). Upper bounds are exclusive so that
# "580 to 593" includes 593.xx (upper = 594).
icd9_in_range <- function(code, lower, upper) {
  x <- icd9_to_numeric(code)
  !is.na(x) & x >= lower & x < upper
}

# Primary diagnosis groups follow Strack et al. (2014):
#   Diabetes 250.xx; Circulatory 390-459 and 785; Respiratory 460-519 and 786.
classify_primary_diagnosis <- function(code) {
  diag <- dplyr::case_when(
    icd9_in_range(code, 250, 251) ~ "Diabetes-related",
    icd9_in_range(code, 390, 460) | icd9_in_range(code, 785, 786) ~ "Cardiovascular",
    icd9_in_range(code, 460, 520) | icd9_in_range(code, 786, 787) ~ "Respiratory",
    TRUE ~ "Other"
  )
  factor(diag, levels = c("Cardiovascular", "Diabetes-related",
                          "Respiratory", "Other"))
}

# A comorbidity is flagged if any of the three diagnosis fields is in range.
any_diagnosis_in_range <- function(diag_1, diag_2, diag_3, lower, upper) {
  icd9_in_range(diag_1, lower, upper) |
    icd9_in_range(diag_2, lower, upper) |
    icd9_in_range(diag_3, lower, upper)
}

yes_no <- function(x) factor(ifelse(x, "Yes", "No"), levels = c("No", "Yes"))

recode_race <- function(race) {
  race_cat <- dplyr::case_when(
    race == "Caucasian" ~ "White",
    race == "AfricanAmerican" ~ "Black",
    race == "Hispanic" ~ "Hispanic",
    TRUE ~ "Other/Unknown"
  )
  factor(race_cat, levels = c("White", "Black", "Hispanic", "Other/Unknown"))
}

# "None" and missing both mean no A1C was drawn during the encounter. That is
# kept as its own category because not testing is clinically informative.
recode_a1c <- function(a1c) {
  a1c_cat <- dplyr::case_when(
    a1c == "Norm" ~ "Normal",
    a1c == ">7" ~ ">7",
    a1c == ">8" ~ ">8",
    TRUE ~ "Not measured"
  )
  factor(a1c_cat, levels = c("Not measured", "Normal", ">7", ">8"))
}

recode_analytic <- function(cohort) {
  cohort |>
    dplyr::mutate(
      readmit_30day = as.integer(readmitted == "<30"),
      age_cat = factor(age, levels = AGE_LEVELS),
      sex = factor(gender, levels = c("Female", "Male")),
      race_cat = recode_race(race),
      a1c_cat = recode_a1c(A1Cresult),
      insulin_use = yes_no(insulin != "No"),
      med_change = yes_no(change == "Ch"),
      diag_cat = classify_primary_diagnosis(diag_1),
      heart_failure = yes_no(any_diagnosis_in_range(diag_1, diag_2, diag_3, 428, 429)),
      renal_disease = yes_no(any_diagnosis_in_range(diag_1, diag_2, diag_3, 580, 594)),
      length_of_stay = time_in_hospital,
      prior_inpatient = number_inpatient,
      prior_emergency = number_emergency,
      prior_outpatient = number_outpatient
    ) |>
    dplyr::select(
      encounter_id, patient_nbr, readmit_30day,
      age_cat, sex, race_cat, a1c_cat, insulin_use, med_change, diag_cat,
      heart_failure, renal_disease,
      prior_inpatient, prior_emergency, prior_outpatient, length_of_stay
    )
}
