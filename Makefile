
all: README.md

README.md: inst/README.Rmd
	Rscript -e "library(knitr); knit('$<', output = '$@', quiet = TRUE)"
