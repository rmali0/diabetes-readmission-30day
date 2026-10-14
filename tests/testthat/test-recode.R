test_that("ICD-9 strings convert only when purely numeric", {
  codes <- c("250.83", "V57", "E909", NA, "8", " 428 ", "", "250.10")
  expect_equal(icd9_to_numeric(codes), c(250.83, NA, NA, NA, 8, 428, NA, 250.1))
})

test_that("primary diagnosis groups follow the Strack et al. ranges", {
  codes <- c("250", "250.99", "251", "390", "459.9", "460", "785", "786",
             "519.9", "520", "V57", "E909", NA, "428", "8")
  expected <- c("Diabetes-related", "Diabetes-related", "Other", "Cardiovascular",
                "Cardiovascular", "Respiratory", "Cardiovascular", "Respiratory",
                "Respiratory", "Other", "Other", "Other", "Other", "Cardiovascular",
                "Other")
  out <- classify_primary_diagnosis(codes)
  expect_equal(as.character(out), expected)
  expect_equal(levels(out)[1], "Cardiovascular")
})

test_that("comorbidity ranges have the right edges", {
  hf <- any_diagnosis_in_range(c("428", "427.99", "429", "V45"),
                               c(NA, NA, NA, "428.9"), NA, 428, 429)
  expect_equal(hf, c(TRUE, FALSE, FALSE, TRUE))
  renal <- any_diagnosis_in_range(c("580", "593.99", "594", "579.9", "E909"),
                                  NA, NA, 580, 594)
  expect_equal(renal, c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("A1C and race recodes collapse as documented", {
  expect_equal(as.character(recode_a1c(c("None", NA, "Norm", ">7", ">8"))),
               c("Not measured", "Not measured", "Normal", ">7", ">8"))
  expect_equal(as.character(recode_race(c("Caucasian", "AfricanAmerican", "Hispanic",
                                          "Asian", "Other", NA))),
               c("White", "Black", "Hispanic", "Other/Unknown", "Other/Unknown",
                 "Other/Unknown"))
})

test_that("analytic variables are built correctly from the fixture", {
  d <- recode_analytic(build_cohort(read_fixture(), exclude_death_hospice = FALSE)$data)
  row <- function(id) d[d$encounter_id == id, ]

  expect_equal(row(10)$readmit_30day, 1L)
  expect_equal(row(60)$readmit_30day, 0L)   # ">30" is not a 30-day readmission
  expect_equal(as.character(row(10)$diag_cat), "Diabetes-related")
  expect_equal(as.character(row(10)$heart_failure), "Yes")
  expect_equal(as.character(row(10)$insulin_use), "Yes")
  expect_equal(as.character(row(10)$med_change), "Yes")

  expect_equal(as.character(row(5)$diag_cat), "Cardiovascular")
  expect_equal(as.character(row(5)$renal_disease), "Yes")
  expect_equal(as.character(row(5)$race_cat), "Black")

  expect_equal(as.character(row(50)$diag_cat), "Other")       # E909 primary
  expect_equal(as.character(row(50)$renal_disease), "Yes")    # 593.9
  expect_equal(as.character(row(50)$a1c_cat), "Not measured") # "?" in source

  expect_equal(as.character(row(60)$diag_cat), "Other")       # "8" is ICD-9 008
  expect_equal(as.character(row(60)$heart_failure), "No")     # 427 is not 428
  expect_equal(as.character(row(70)$diag_cat), "Respiratory")
  expect_equal(as.character(row(70)$renal_disease), "No")     # 594 is outside 580-593

  expect_equal(row(70)$prior_inpatient, 3L)
  expect_equal(levels(d$age_cat)[1], "[20-30)")
  expect_false(anyNA(d))
})

test_that("age bands get readable labels", {
  expect_equal(format_age_band(c("[20-30)", "[80-90)", "[90-100)")),
               c("20-29", "80-89", "90+"))
})
