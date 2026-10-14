test_that("Table 1 uses column percentages within each outcome group", {
  d <- recode_analytic(build_cohort(read_fixture(), exclude_death_hospice = FALSE)$data)
  t1 <- make_table1(d)
  sex_rows <- t1[t1$variable == "sex", ]
  expect_equal(sex_rows$level, c("Female", "Male"))
  # 6 patients: 3 female; readmitted group has 2 patients (both female)
  expect_equal(sex_rows$overall, c("3 (50.0)", "3 (50.0)"))
  expect_equal(sex_rows$readmitted, c("2 (100.0)", "0 (0.0)"))
  expect_true(all(c("median [IQR]", "mean (SD)") %in% t1$statistic))
})

test_that("rank-based AUC matches pROC", {
  set.seed(1)
  y <- rbinom(500, 1, 0.2)
  p <- plogis(rnorm(500) + y)
  roc <- pROC::roc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE)
  expect_equal(auc_rank(y, p), as.numeric(pROC::auc(roc)), tolerance = 1e-10)
})

test_that("calibration metrics are near ideal when risks are correct", {
  set.seed(2)
  lp <- rnorm(20000, -2.3, 0.6)
  y <- rbinom(length(lp), 1, plogis(lp))
  m <- calibration_metrics(y, lp)
  expect_lt(abs(m[["cal_intercept"]]), 0.1)
  expect_lt(abs(m[["cal_slope"]] - 1), 0.1)
})

test_that("Wilson interval matches a known value", {
  ci <- wilson_interval(10, 100)
  expect_equal(c(ci$low, ci$high), c(0.05522914, 0.17436566), tolerance = 1e-6)
})

test_that("every model coefficient maps to a labelled variable and level", {
  set.seed(3)
  n <- 3000
  sim <- tibble::tibble(
    readmit_30day = rbinom(n, 1, 0.1),
    age_cat = factor(sample(AGE_LEVELS, n, TRUE), levels = AGE_LEVELS),
    sex = factor(sample(c("Female", "Male"), n, TRUE)),
    race_cat = recode_race(sample(c("Caucasian", "AfricanAmerican", "Hispanic", "Other"), n, TRUE)),
    a1c_cat = recode_a1c(sample(c("None", "Norm", ">7", ">8"), n, TRUE)),
    insulin_use = yes_no(runif(n) < 0.5),
    med_change = yes_no(runif(n) < 0.4),
    diag_cat = classify_primary_diagnosis(sample(c("250", "414", "486", "V57"), n, TRUE)),
    heart_failure = yes_no(runif(n) < 0.15),
    renal_disease = yes_no(runif(n) < 0.08),
    prior_inpatient = rpois(n, 0.5), prior_emergency = rpois(n, 0.2),
    prior_outpatient = rpois(n, 0.3), length_of_stay = rpois(n, 4) + 1L
  )
  ors <- odds_ratio_table(fit_readmission_model(sim))
  expect_equal(nrow(ors), 25)
  expect_false(anyNA(ors$variable))
  expect_false(anyNA(ors$level))
  expect_equal(ors$reference[ors$variable == "age_cat"][1], "20-29")
  expect_true(all(ors$conf_low < ors$or & ors$or < ors$conf_high))
})
