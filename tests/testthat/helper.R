
with_timezone <- function(tzone, expr) {
  old <- get0(".sys.timezone", baseenv(), mode = "character",
              inherits = FALSE, ifnotfound = NA_character_)
  on.exit(assign(".sys.timezone", old, envir = baseenv()), add = TRUE)
  assign(".sys.timezone", tzone, envir = baseenv())
  withr::with_envvar(c(TZ = tzone), expr)
}
