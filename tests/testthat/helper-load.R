# Load the project functions; the repo is a set of scripts, not a package.
r_dir <- testthat::test_path("..", "..", "R")
for (f in list.files(r_dir, pattern = "\\.R$", full.names = TRUE)) source(f)

read_fixture <- function() {
  read_diabetes_data(testthat::test_path("fixtures", "mini_encounters.csv"))
}
