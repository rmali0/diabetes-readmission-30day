# Reproduces the full analysis from the raw data in data/.
# Usage (from the repository root): Rscript run_all.R
#
# Renders analysis/readmission_analysis.Rmd to GitHub-flavoured markdown and
# writes all figures to outputs/figures and all tables to outputs/tables.

boot_reps <- as.integer(Sys.getenv("BOOT_REPS", "200"))

rmarkdown::render(
  "analysis/readmission_analysis.Rmd",
  params = list(boot_reps = boot_reps),
  envir = new.env(),
  quiet = TRUE
)
message("Done. See analysis/readmission_analysis.md and outputs/.")
