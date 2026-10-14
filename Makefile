# Run from the repository root.
#   make all       run tests, then the full analysis
#   make test      unit tests only (fast, uses a small fixture)
#   make analysis  render analysis/readmission_analysis.md and outputs/

.PHONY: all test analysis clean

all: test analysis

test:
	Rscript tests/testthat.R

analysis:
	Rscript run_all.R

clean:
	rm -f outputs/figures/*.png outputs/tables/*.csv analysis/readmission_analysis.md
