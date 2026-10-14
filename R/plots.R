# Figures. Colors: one blue for single-series marks, blue and orange when two
# cohorts are compared; text stays in neutral ink.

COL_PRIMARY <- "#2a78d6"
COL_SECONDARY <- "#eb6834"
COL_INK <- "#0b0b0b"
COL_INK_2 <- "#52514e"
COL_GRID <- "#e4e3df"

theme_readmission <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = COL_GRID, linewidth = 0.3),
      text = ggplot2::element_text(colour = COL_INK),
      axis.text = ggplot2::element_text(colour = COL_INK_2),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(colour = COL_INK_2),
      plot.caption = ggplot2::element_text(colour = COL_INK_2, hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)

# CONSORT-style flow: retained counts down the left, exclusions to the right.
plot_cohort_flow <- function(flow) {
  k <- nrow(flow)
  main <- tibble::tibble(
    x = 0, y = -(seq_len(k) - 1) * 2,
    label = paste0(flow$step, "\nn = ", fmt_n(flow$n_remaining))
  )
  excl <- tibble::tibble(
    x = 3.1, y = -(seq_len(k - 1) - 0.5) * 2,
    label = paste0(flow$excluded_reason[-1], "\nn = ", fmt_n(flow$n_excluded[-1]))
  )
  box_w <- 1.35
  box_h <- 0.5
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = tibble::tibble(y = main$y[-k] - box_h, yend = main$y[-1] + box_h),
      ggplot2::aes(x = 0, xend = 0, y = y, yend = yend),
      arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
      colour = COL_INK_2, linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      data = excl,
      ggplot2::aes(x = 0, xend = x - 1.25, y = y, yend = y),
      arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
      colour = COL_INK_2, linewidth = 0.4
    ) +
    ggplot2::geom_rect(
      data = main,
      ggplot2::aes(xmin = x - box_w, xmax = x + box_w, ymin = y - box_h, ymax = y + box_h),
      fill = "#eef4fc", colour = COL_PRIMARY, linewidth = 0.4
    ) +
    ggplot2::geom_rect(
      data = excl,
      ggplot2::aes(xmin = x - 1.25, xmax = x + 1.25, ymin = y - 0.42, ymax = y + 0.42),
      fill = "white", colour = COL_INK_2, linewidth = 0.3
    ) +
    ggplot2::geom_text(data = main, ggplot2::aes(x, y, label = label),
                       size = 3.4, lineheight = 0.95, colour = COL_INK) +
    ggplot2::geom_text(data = excl, ggplot2::aes(x, y, label = label),
                       size = 3.1, lineheight = 0.95, colour = COL_INK_2) +
    ggplot2::coord_cartesian(xlim = c(-1.5, 4.5), clip = "off") +
    ggplot2::labs(title = "Cohort selection") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.title = ggplot2::element_text(face = "bold", size = 12),
                   plot.margin = ggplot2::margin(10, 10, 10, 10))
}

# Adds the reference level of each factor as an OR = 1 row for plotting.
with_reference_rows <- function(or_table) {
  refs <- or_table |>
    dplyr::filter(!is.na(reference)) |>
    dplyr::distinct(variable, characteristic, reference) |>
    dplyr::transmute(variable, characteristic, level = paste(reference, "(ref)"),
                     or = 1, conf_low = NA_real_, conf_high = NA_real_,
                     p_value = NA_real_, is_reference = TRUE)
  dplyr::bind_rows(
    refs,
    dplyr::mutate(or_table, is_reference = FALSE)
  ) |>
    dplyr::mutate(variable = factor(variable, levels = MODEL_PREDICTORS)) |>
    dplyr::arrange(variable, !is_reference)
}

plot_forest <- function(or_table, n, events) {
  d <- with_reference_rows(or_table)
  fg <- figure_group_and_level(d$variable, d$characteristic, d$level)
  d <- d |>
    dplyr::mutate(
      characteristic = factor(fg$group, levels = unique(fg$group)),
      level = fg$level,
      row_id = factor(paste(variable, level), levels = rev(unique(paste(variable, level)))),
      label = ifelse(is_reference, "",
                     sprintf("%.2f (%.2f, %.2f)", or, conf_low, conf_high))
    )
  x_text <- max(d$conf_high, na.rm = TRUE) * 1.25
  ggplot2::ggplot(d, ggplot2::aes(y = row_id, x = or)) +
    ggplot2::geom_vline(xintercept = 1, colour = COL_INK_2, linewidth = 0.4,
                        linetype = "dashed") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = conf_low, xmax = conf_high),
                            colour = COL_PRIMARY, linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = is_reference), colour = COL_PRIMARY,
                        fill = "white", size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(x = x_text, label = label), hjust = 0,
                       size = 2.9, colour = COL_INK_2) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), guide = "none") +
    ggplot2::scale_y_discrete(labels = function(x) sub("^\\S+ ", "", x)) +
    ggplot2::scale_x_log10(breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
                           expand = ggplot2::expansion(mult = c(0.05, 0.45))) +
    ggplot2::facet_grid(characteristic ~ ., scales = "free_y", space = "free_y",
                        switch = "y", labeller = ggplot2::label_wrap_gen(22)) +
    ggplot2::labs(
      title = "Adjusted odds ratios for 30-day readmission",
      subtitle = sprintf("Multivariable logistic regression, n = %s patients (%s readmitted)",
                         fmt_n(n), fmt_n(events)),
      x = "Adjusted odds ratio (95% CI, log scale)", y = NULL,
      caption = "Hollow points mark reference levels. Prior-year counts come from the index encounter record."
    ) +
    theme_readmission(10) +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "bold",
                                                colour = COL_INK),
      panel.spacing.y = ggplot2::unit(2, "pt"),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

plot_calibration <- function(bins, summary_tbl, n_test) {
  get <- function(m) summary_tbl$value[summary_tbl$metric == m]
  upper <- max(c(bins$observed_high, bins$mean_predicted)) * 1.1
  ggplot2::ggplot(bins, ggplot2::aes(mean_predicted, observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = COL_INK_2,
                         linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = observed_low, ymax = observed_high),
                            colour = COL_PRIMARY, linewidth = 0.6) +
    ggplot2::geom_line(colour = COL_PRIMARY, linewidth = 0.5, alpha = 0.6) +
    ggplot2::geom_point(colour = COL_PRIMARY, size = 2.4) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(1), limits = c(0, upper)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(1), limits = c(0, upper)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Calibration on the held-out 30%",
      subtitle = sprintf("Intercept %.2f, slope %.2f, AUC %.3f (n = %s)",
                         round(get("Calibration intercept"), 2) + 0,
                         round(get("Calibration slope"), 2),
                         get("AUC"), fmt_n(n_test)),
      x = "Mean predicted risk (decile of predicted risk)",
      y = "Observed 30-day readmission",
      caption = "Bars are 95% Wilson intervals. Dashed line is perfect calibration."
    ) +
    theme_readmission(10)
}

plot_roc <- function(roc, auc_label) {
  d <- tibble::tibble(fpr = 1 - roc$specificities, tpr = roc$sensitivities) |>
    dplyr::arrange(fpr, tpr)
  ggplot2::ggplot(d, ggplot2::aes(fpr, tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = COL_INK_2,
                         linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_step(colour = COL_PRIMARY, linewidth = 0.6) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(labels = scales::percent_format(1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(1)) +
    ggplot2::labs(title = "ROC curve on the held-out 30%", subtitle = auc_label,
                  x = "False positive rate (1 - specificity)",
                  y = "True positive rate (sensitivity)") +
    theme_readmission(10)
}

COHORT_ORIGINAL <- "Original (course) definition"
COHORT_CORRECTED <- "Corrected: death/hospice excluded"
COHORT_COLOURS <- stats::setNames(c(COL_SECONDARY, COL_PRIMARY),
                                  c(COHORT_ORIGINAL, COHORT_CORRECTED))

plot_sensitivity <- function(or_compare) {
  fg <- figure_group_and_level(or_compare$variable, or_compare$characteristic,
                               or_compare$level)
  d <- or_compare |>
    dplyr::mutate(
      label = ifelse(variable %in% CONTINUOUS_VARS, fg$level,
                     paste0(characteristic, ": ", level)),
      label = factor(label, levels = rev(unique(label))),
      cohort = factor(cohort, levels = names(COHORT_COLOURS))
    )
  ggplot2::ggplot(d, ggplot2::aes(y = label, x = or, colour = cohort)) +
    ggplot2::geom_vline(xintercept = 1, colour = COL_INK_2, linetype = "dashed",
                        linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = conf_low, xmax = conf_high),
                            position = ggplot2::position_dodge(width = 0.6),
                            linewidth = 0.6) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.6), size = 2) +
    ggplot2::scale_colour_manual(values = COHORT_COLOURS, name = NULL) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(title = "Sensitivity analysis: cohort definition",
                  subtitle = "Adjusted odds ratios with and without the death/hospice exclusion",
                  x = "Adjusted odds ratio (95% CI, log scale)", y = NULL) +
    theme_readmission(10) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid.major.y = ggplot2::element_blank())
}

save_figure <- function(plot, path, width, height) {
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 150, bg = "white")
  invisible(path)
}
