
with_timezone <- function(tzone, expr) {
  old <- get0(".sys.timezone", baseenv(), mode = "character",
              inherits = FALSE, ifnotfound = NA_character_)
  on.exit(assign(".sys.timezone", old, envir = baseenv()), add = TRUE)
  assign(".sys.timezone", NA, envir = baseenv())
  withr::with_envvar(c(TZ = tzone), expr)
}

local_timezone <- function(tzone, .local_envir = parent.frame()) {
  old <- get0(".sys.timezone", baseenv(), mode = "character",
              inherits = FALSE, ifnotfound = NA_character_)
  withr::defer(
     assign(".sys.timezone", old, envir = baseenv()),
     envir = .local_envir)
  assign(".sys.timezone", NA, envir = baseenv())
  withr::local_envvar(c(TZ = tzone), .local_envir = .local_envir)
}

cdate <- function(...) {
  ret <- c(...)
  attr(ret, "tzone") <- "UTC"
  ret
}
