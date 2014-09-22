library(testthat)
library(parsedate)

if (Sys.getenv("NOT_CRAN") != "") {
  test_check("parsedate")
}
