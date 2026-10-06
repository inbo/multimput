raw_clamp <- function(y, data, dots, missing_obs) {
  if ("minimum" %in% names(dots)) {
    stopifnot(
      "`minimum` is not a character" = is.character(dots$minimum),
      "`minimum` must has a length of 1" = length(dots$minimum) == 1,
      "`minimum` must no be NA" = !is.na(dots$minimum)
    )
    if (dots$minimum != "" && nrow(y) > 0) {
      stopifnot(
        "`minimum` must contain a column in `data`" = dots$minimum %in%
          colnames(data)
      )
      y <- apply(y, 2, pmax, data[missing_obs, dots$minimum], na.rm = TRUE)
    }
  }
  return(y)
}
