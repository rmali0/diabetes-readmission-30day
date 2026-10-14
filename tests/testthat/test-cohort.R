test_that("raw file is read with ? as missing and codes kept as text", {
  raw <- read_fixture()
  expect_equal(nrow(raw), 10)
  expect_type(raw$diag_1, "character")
  expect_true("V57" %in% raw$diag_1)
  expect_true("250.83" %in% raw$diag_1)
  expect_true(is.na(raw$race[raw$encounter_id == 30]))
  expect_type(raw$discharge_disposition_id, "integer")
  expect_equal(raw$A1Cresult[raw$encounter_id == 20], "None")
})

test_that("death and hospice dispositions are identified", {
  ids <- c(1L, 11L, 13L, 14L, 19L, 20L, 21L, 18L, 6L, NA)
  expect_equal(is_death_or_hospice(ids),
               c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("first encounter is the lowest encounter_id, not file order", {
  first <- first_encounter_per_patient(read_fixture())
  expect_equal(nrow(first), 8)
  expect_equal(first$encounter_id[first$patient_nbr == 1], 10)
  expect_equal(first$encounter_id[first$patient_nbr == 8], 80)
})

test_that("corrected cohort applies every step in order", {
  cohort <- build_cohort(read_fixture(), exclude_death_hospice = TRUE)
  expect_equal(cohort$flow$n_remaining, c(10, 9, 7, 4, 3))
  expect_equal(cohort$flow$n_excluded, c(NA, 1, 2, 3, 1))
  expect_setequal(cohort$data$encounter_id, c(10, 60, 70))
})

test_that("a patient whose index stay ended in hospice is dropped, not re-indexed", {
  cohort <- build_cohort(read_fixture(), exclude_death_hospice = TRUE)
  expect_false(8 %in% cohort$data$patient_nbr)
})

test_that("original course cohort keeps deaths and hospice discharges", {
  cohort <- build_cohort(read_fixture(), exclude_death_hospice = FALSE)
  expect_equal(cohort$flow$n_remaining, c(10, 9, 7, 6))
  expect_setequal(cohort$data$encounter_id, c(10, 5, 50, 60, 70, 80))
})
