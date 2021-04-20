
cdate <- function(...) {
  ret <- c(...)
  attr(ret, "tzone") <- "UTC"
  ret
}
