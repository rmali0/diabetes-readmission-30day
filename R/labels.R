# Display labels shared by tables and figures.

CATEGORICAL_VARS <- c("age_cat", "sex", "race_cat", "a1c_cat", "insulin_use",
                      "med_change", "diag_cat", "heart_failure", "renal_disease")

CONTINUOUS_VARS <- c("prior_inpatient", "prior_emergency",
                     "prior_outpatient", "length_of_stay")

VARIABLE_LABELS <- c(
  age_cat = "Age, years",
  sex = "Sex",
  race_cat = "Race/ethnicity",
  a1c_cat = "HbA1c result",
  insulin_use = "Insulin prescribed",
  med_change = "Diabetes medication changed",
  diag_cat = "Primary diagnosis",
  heart_failure = "Heart failure (ICD-9 428)",
  renal_disease = "Renal disease (ICD-9 580-593)",
  prior_inpatient = "Inpatient admissions, prior year",
  prior_emergency = "Emergency visits, prior year",
  prior_outpatient = "Outpatient visits, prior year",
  length_of_stay = "Length of stay, days"
)

CONTINUOUS_UNITS <- c(
  prior_inpatient = "Per additional stay",
  prior_emergency = "Per additional visit",
  prior_outpatient = "Per additional visit",
  length_of_stay = "Per additional day"
)

# Short labels for the count and length-of-stay terms in figures, where they
# share one panel.
CONTINUOUS_SHORT_LABELS <- c(
  prior_inpatient = "Prior inpatient stays, per stay",
  prior_emergency = "Prior ED visits, per visit",
  prior_outpatient = "Prior outpatient visits, per visit",
  length_of_stay = "Length of stay, per day"
)
UTILIZATION_GROUP <- "Utilization"

# Figure-ready grouping: categorical terms keep their variable as the group;
# continuous terms are pooled into one "Utilization" group.
figure_group_and_level <- function(variable, characteristic, level) {
  variable <- as.character(variable)
  is_cont <- variable %in% CONTINUOUS_VARS
  group <- ifelse(is_cont, UTILIZATION_GROUP, characteristic)
  lvl <- ifelse(is_cont, unname(CONTINUOUS_SHORT_LABELS[variable]), level)
  list(group = group, level = lvl)
}

# "[30-40)" becomes "30-39"; the oldest band is labelled 90+.
format_age_band <- function(band) {
  lower <- as.integer(sub("^\\[(\\d+)-.*$", "\\1", band))
  upper <- as.integer(sub("^.*-(\\d+)\\)$", "\\1", band)) - 1L
  ifelse(lower >= 90, "90+", paste0(lower, "-", upper))
}

format_level <- function(variable, level) {
  out <- as.character(level)
  is_age <- variable == "age_cat"
  out[is_age] <- format_age_band(out[is_age])
  out
}
