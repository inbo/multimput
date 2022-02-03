check_old_names <- function(..., old_names) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(invisible(TRUE))
  }
  problem <- old_names %in% names(dots)
  if (!any(problem)) {
    return(invisible(TRUE))
  }
  change <- sprintf(
    "\n`%s` -> `%s`", old_names[problem], names(old_names)[problem]
  )
  stop("some arguments changed name:", change, call. = FALSE)
}
