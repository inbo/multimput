aggregate_clamp <- function(total, ...) {
  dots <- list(...)
  if (!"minimum" %in% names(dots)) {
    return(total)
  }
  minimum <- dots$minimum
  grouping <- colnames(total)[grep(
    "Imputation[0-9]+",
    colnames(total),
    invert = TRUE
  )]
  # fmt: skip
  stopifnot(
    "`minimum` is not a data.frame" = inherits(minimum, "data.frame"),
    "`minimum` does not contain all grouping columns" = all(
      grouping %in% colnames(minimum)
    ),
    "`minimum` should have exactly one more column than the grouping columns" =
      length(colnames(minimum)) == length(grouping) + 1,
    "`minimum` contains duplicate rows for the grouping columns" =
      nrow(unique(minimum[, grouping])) == nrow(minimum)
  )
  response <- colnames(minimum)[!colnames(minimum) %in% grouping]
  total <- merge(total, minimum, by = grouping, all = TRUE)
  total[, grep("Imputation[0-9]+", colnames(total))] |>
    apply(2, pmax, total[, response], na.rm = TRUE) -> clamped
  cbind(total[, grouping], clamped)
}
