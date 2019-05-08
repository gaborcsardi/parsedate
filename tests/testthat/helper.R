
with_timezone <- function(tzone, expr) {
  ## Need to remove the cache first, and also on exit
  clear <- function() assign(".sys.timezone", NA, envir = baseenv())
  clear()
  on.exit(clear(), add = TRUE)
  withr::with_envvar(c(TZ = tzone), expr)
}
