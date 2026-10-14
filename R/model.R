# Multivariable logistic regression, model checks, and validation.

MODEL_PREDICTORS <- c(CATEGORICAL_VARS, CONTINUOUS_VARS)
MODEL_FORMULA <- stats::reformulate(MODEL_PREDICTORS, response = "readmit_30day")

fit_readmission_model <- function(data) {
  stats::glm(MODEL_FORMULA, data = data, family = stats::binomial())
}

# Maps model terms such as "age_cat[70-80)" back to variable and level.
term_lookup <- function(model) {
  rows <- lapply(MODEL_PREDICTORS, function(v) {
    lv <- model$xlevels[[v]]
    if (is.null(lv)) {
      tibble::tibble(term = v, variable = v, level = CONTINUOUS_UNITS[[v]],
                     reference = NA_character_)
    } else {
      tibble::tibble(term = paste0(v, lv[-1]), variable = v,
                     level = format_level(v, lv[-1]),
                     reference = format_level(v, lv[1]))
    }
  })
  dplyr::bind_rows(rows) |>
    dplyr::mutate(characteristic = unname(VARIABLE_LABELS[variable]))
}

# Wald intervals; with about 70,000 patients they match profile-likelihood
# intervals to the third decimal and are far cheaper to compute.
odds_ratio_table <- function(model) {
  est <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model)))
  z <- stats::qnorm(0.975)
  tibble::tibble(term = names(est), log_or = unname(est), se = unname(se)) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      or = exp(log_or),
      conf_low = exp(log_or - z * se),
      conf_high = exp(log_or + z * se),
      p_value = 2 * stats::pnorm(-abs(log_or / se))
    ) |>
    dplyr::left_join(term_lookup(model), by = "term") |>
    dplyr::select(variable, characteristic, level, reference,
                  or, conf_low, conf_high, p_value, term)
}

# Likelihood ratio test for dropping each predictor (all its dummy terms
# together), which gives one test per variable rather than per level.
lr_tests <- function(model) {
  d <- stats::drop1(model, test = "LRT")
  tibble::tibble(variable = rownames(d), df = d$Df,
                 lr_chisq = d$LRT, p_value = d[["Pr(>Chi)"]]) |>
    dplyr::filter(variable != "<none>") |>
    dplyr::mutate(characteristic = unname(VARIABLE_LABELS[variable]), .after = 1)
}

vif_table <- function(model) {
  v <- car::vif(model)
  tibble::tibble(
    variable = rownames(v),
    characteristic = unname(VARIABLE_LABELS[rownames(v)]),
    gvif = v[, "GVIF"],
    df = v[, "Df"],
    gvif_adjusted = v[, "GVIF^(1/(2*Df))"]
  )
}

# Box-Tidwell style check used in the course version: x * log(x + 1) terms
# (the + 1 is needed because the counts include zeros).
box_tidwell_check <- function(data) {
  d <- data
  for (v in CONTINUOUS_VARS) d[[paste0(v, "_xlog")]] <- d[[v]] * log(d[[v]] + 1)
  f <- stats::reformulate(c(CONTINUOUS_VARS, paste0(CONTINUOUS_VARS, "_xlog")),
                          response = "readmit_30day")
  fit <- stats::glm(f, data = d, family = stats::binomial())
  broom::tidy(fit) |>
    dplyr::filter(grepl("_xlog$", term)) |>
    dplyr::transmute(variable = sub("_xlog$", "", term),
                     characteristic = unname(VARIABLE_LABELS[variable]),
                     estimate, p_value = p.value)
}

# --- Performance -------------------------------------------------------------

# AUC as the Mann-Whitney probability; fast enough to call inside a bootstrap.
auc_rank <- function(y, p) {
  r <- rank(p)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Calibration intercept (calibration-in-the-large, slope fixed at 1) and
# calibration slope, both estimated on the linear predictor.
calibration_metrics <- function(y, lp) {
  p <- stats::plogis(lp)
  intercept_fit <- stats::glm.fit(matrix(1, length(y)), y, offset = lp,
                                  family = stats::binomial())
  slope_fit <- stats::glm.fit(cbind(1, lp), y, family = stats::binomial())
  c(
    auc = auc_rank(y, p),
    cal_intercept = unname(intercept_fit$coefficients[1]),
    cal_slope = unname(slope_fit$coefficients[2]),
    brier = mean((p - y)^2)
  )
}

wilson_interval <- function(events, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- events / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(low = centre - half, high = centre + half)
}

calibration_bins <- function(y, p, groups = 10) {
  tibble::tibble(y = y, p = p) |>
    dplyr::mutate(bin = dplyr::ntile(p, groups)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(n = dplyr::n(), events = sum(y),
                     mean_predicted = mean(p), observed = mean(y),
                     .groups = "drop") |>
    dplyr::mutate(
      observed_low = wilson_interval(events, n)$low,
      observed_high = wilson_interval(events, n)$high
    )
}

stratified_split <- function(y, train_prop, seed) {
  set.seed(seed)
  train <- logical(length(y))
  for (cls in unique(y)) {
    idx <- which(y == cls)
    train[sample(idx, round(train_prop * length(idx)))] <- TRUE
  }
  train
}

# Fit on a stratified 70% split and evaluate on the untouched 30%.
holdout_validation <- function(data, train_prop = 0.7, seed = 2026) {
  is_train <- stratified_split(data$readmit_30day, train_prop, seed)
  train <- data[is_train, ]
  test <- data[!is_train, ]
  fit <- fit_readmission_model(train)
  lp <- stats::predict(fit, newdata = test, type = "link")
  p <- stats::plogis(lp)
  roc <- pROC::roc(test$readmit_30day, p, levels = c(0, 1),
                   direction = "<", quiet = TRUE)
  auc_ci <- as.numeric(pROC::ci.auc(roc, method = "delong"))
  metrics <- calibration_metrics(test$readmit_30day, lp)
  # A single split is noisy, so report the uncertainty of the slope too.
  slope_fit <- stats::glm(test$readmit_30day ~ lp, family = stats::binomial())
  slope_ci <- unname(stats::confint.default(slope_fit)["lp", ])
  list(
    n_train = nrow(train), n_test = nrow(test),
    events_test = sum(test$readmit_30day),
    roc = roc,
    summary = tibble::tibble(
      metric = c("AUC", "AUC 95% CI lower", "AUC 95% CI upper",
                 "Calibration intercept", "Calibration slope",
                 "Calibration slope 95% CI lower", "Calibration slope 95% CI upper",
                 "Brier score", "Observed rate", "Mean predicted risk"),
      value = c(auc_ci[2], auc_ci[1], auc_ci[3], metrics[["cal_intercept"]],
                metrics[["cal_slope"]], slope_ci[1], slope_ci[2], metrics[["brier"]],
                mean(test$readmit_30day), mean(p))
    ),
    bins = calibration_bins(test$readmit_30day, p)
  )
}

# Harrell's bootstrap optimism correction on the full cohort: refit in each
# resample, measure how much better the model looks on its own resample than
# on the original data, and subtract the average of that gap.
bootstrap_optimism <- function(data, B = 200, seed = 2026) {
  X <- stats::model.matrix(MODEL_FORMULA, data = data)
  y <- data$readmit_30day
  fam <- stats::binomial()
  apparent_fit <- stats::glm.fit(X, y, family = fam)
  apparent <- calibration_metrics(y, drop(X %*% apparent_fit$coefficients))

  set.seed(seed)
  optimism <- t(vapply(seq_len(B), function(b) {
    idx <- sample.int(nrow(X), replace = TRUE)
    beta <- stats::glm.fit(X[idx, ], y[idx], family = fam)$coefficients
    beta[is.na(beta)] <- 0
    on_boot <- calibration_metrics(y[idx], drop(X[idx, ] %*% beta))
    on_original <- calibration_metrics(y, drop(X %*% beta))
    on_boot - on_original
  }, numeric(4)))

  tibble::tibble(
    metric = c("AUC", "Calibration intercept", "Calibration slope", "Brier score"),
    apparent = unname(apparent),
    mean_optimism = unname(colMeans(optimism)),
    corrected = unname(apparent - colMeans(optimism)),
    bootstrap_reps = B
  )
}
