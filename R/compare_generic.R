#' Compare two imputed datasets
#' @param x The first imputed dataset
#' @param y The second imputed dataset
#' @param fun A function to compare two datasets.
#' @inheritParams dplyr::inner_join
#' @name compare
#' @rdname compare
#' @exportMethod compare
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "compare",
  def = function(x, y, fun, by) {
    standard.generic("compare") # nocov
  }
)

#' @rdname compare
#' @importFrom methods setMethod
setMethod(
  f = "compare",
  signature = signature(x = "ANY"),
  definition = function(x, y, fun, by) {
    stop(
      "compare() can't handle an `x` of class ",
      class(x),
      " at this moment.
We will consider adding support for extra classes. Please create an issue with a
reproducible example at https://github.com/inbo/multimput/issues"
    )
  }
)

#' @rdname compare
#' @importFrom methods setMethod
setMethod(
  f = "compare",
  signature = signature(y = "ANY"),
  definition = function(x, y, fun, by) {
    stop(
      "compare() can't handle an `y` of class ",
      class(x),
      " at this moment.
We will consider adding support for extra classes. Please create an issue with a
reproducible example at https://github.com/inbo/multimput/issues"
    )
  }
)

#' @rdname compare
#' @importFrom assertthat assert_that
#' @importFrom dplyr inner_join mutate row_number select starts_with
#' @importFrom methods setMethod
#' @importFrom tidyr pivot_longer pivot_wider
setMethod(
  f = "compare",
  signature = signature(x = "aggregatedImputed", y = "aggregatedImputed"),
  definition = function(x, y, fun, by) {
    assert_that(inherits(fun, "function"))
    x@Imputation |>
      as.data.frame() |>
      mutate(id_x = row_number()) |>
      pivot_longer(-"id_x", names_to = "Imputation", values_to = "x_value") |>
      inner_join(
        x@Covariate |>
          mutate(id_x = row_number()),
        by = "id_x"
      ) |>
      select(-"id_x") |>
      inner_join(
        y@Imputation |>
          as.data.frame() |>
          mutate(id_y = row_number()) |>
          pivot_longer(
            -"id_y",
            names_to = "Imputation",
            values_to = "y_value"
          ) |>
          inner_join(
            y@Covariate |>
              mutate(id_y = row_number()),
            by = "id_y"
          ) |>
          select(-"id_y"),
        by = c("Imputation", by)
      ) |>
      mutate(value = fun(.data$x_value, .data$y_value)) |>
      select(-"x_value", -"y_value") |>
      pivot_wider(names_from = "Imputation", values_from = "value") -> results

    new(
      "aggregatedImputed",
      Covariate = results |>
        select(-starts_with("Imputation")),
      Imputation = results |>
        select(starts_with("Imputation")) |>
        as.matrix()
    )
  }
)
