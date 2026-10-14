# Table 1 (characteristics by readmission status) and bivariate tests.

format_n_pct <- function(n, total) {
  sprintf("%s (%.1f)", format(n, big.mark = ","), 100 * n / total)
}

format_median_iqr <- function(x) {
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  sprintf("%g [%g, %g]", q[2], q[1], q[3])
}

format_mean_sd <- function(x) {
  sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
}

# Column percentages within each outcome group, as in a standard Table 1.
table1_categorical <- function(data, variable) {
  groups <- list(
    overall = data,
    not_readmitted = data[data$readmit_30day == 0, ],
    readmitted = data[data$readmit_30day == 1, ]
  )
  levels_present <- levels(data[[variable]])
  cells <- lapply(groups, function(g) {
    counts <- table(factor(g[[variable]], levels = levels_present))
    format_n_pct(as.integer(counts), nrow(g))
  })
  tibble::tibble(
    variable = variable,
    characteristic = VARIABLE_LABELS[[variable]],
    level = format_level(variable, levels_present),
    statistic = "n (%)",
    overall = cells$overall,
    not_readmitted = cells$not_readmitted,
    readmitted = cells$readmitted
  )
}

# Prior-visit counts have a median of 0 in both groups, so the mean is also
# shown; otherwise the table would hide the difference in the upper tail.
table1_continuous <- function(data, variable) {
  x <- data[[variable]]
  y <- data$readmit_30day
  summarise_with <- function(fun, label) {
    tibble::tibble(
      variable = variable,
      characteristic = VARIABLE_LABELS[[variable]],
      level = "",
      statistic = label,
      overall = fun(x),
      not_readmitted = fun(x[y == 0]),
      readmitted = fun(x[y == 1])
    )
  }
  dplyr::bind_rows(
    summarise_with(format_median_iqr, "median [IQR]"),
    summarise_with(format_mean_sd, "mean (SD)")
  )
}

make_table1 <- function(data) {
  dplyr::bind_rows(
    lapply(CATEGORICAL_VARS, table1_categorical, data = data),
    lapply(CONTINUOUS_VARS, table1_continuous, data = data)
  )
}

# Pearson chi-square with Cramer's V (equal to phi for 2 x 2 tables). R's
# default Yates correction on 2 x 2 tables is kept, as in the course version.
chi_square_test <- function(data, variable) {
  ct <- table(data[[variable]], data$readmit_30day)
  res <- suppressWarnings(stats::chisq.test(ct))
  k <- min(dim(ct))
  tibble::tibble(
    variable = variable,
    characteristic = VARIABLE_LABELS[[variable]],
    test = "Chi-square",
    statistic = unname(res$statistic),
    df = unname(res$parameter),
    p_value = res$p.value,
    effect_size_type = if (k == 2 && all(dim(ct) == 2)) "phi" else "Cramer's V",
    effect_size = sqrt(unname(res$statistic) / (sum(ct) * (k - 1))),
    min_expected = min(res$expected)
  )
}

# Mann-Whitney U with rank-biserial correlation. Positive values mean the
# readmitted group tends to have higher values.
mann_whitney_test <- function(data, variable) {
  x1 <- data[[variable]][data$readmit_30day == 1]
  x0 <- data[[variable]][data$readmit_30day == 0]
  res <- stats::wilcox.test(x1, x0, exact = FALSE, correct = TRUE)
  u1 <- unname(res$statistic)
  tibble::tibble(
    variable = variable,
    characteristic = VARIABLE_LABELS[[variable]],
    test = "Mann-Whitney U",
    statistic = u1,
    df = NA_real_,
    p_value = res$p.value,
    effect_size_type = "rank-biserial r",
    effect_size = 2 * u1 / (length(x1) * length(x0)) - 1,
    min_expected = NA_real_
  )
}

bivariate_tests <- function(data) {
  dplyr::bind_rows(
    lapply(CATEGORICAL_VARS, chi_square_test, data = data),
    lapply(CONTINUOUS_VARS, mann_whitney_test, data = data)
  )
}

format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}
