#' Model an imputed dataset
#' @param object The imputed dataset.
#' @param model_fun The function to apply on each imputation set.
#' @param rhs The right hand side of the model.
#' @param model_args An optional list of arguments to pass to the model
#' function.
#' @param extractor A function which return a `matrix` or `data.frame`.
#' The first column should contain the estimate,
#' the second the standard error of the estimate.
#' @param extractor_args
#' An optional list of arguments to pass to the `extractor` function.
#' @inheritParams aggregate_impute
#' @param mutate An optional argument to alter the aggregated dataset.
#' Will be passed to the `.dots` argument of[dplyr::mutate()].
#' This is mainly useful for simple conversions, e.g. factors to numbers and
#' vice versa.
#' @param ... currently ignored.
#' @name model_impute
#' @rdname model_impute
#' @exportMethod model_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "model_impute",
  def = function(
    object, model_fun, rhs, model_args = list(), extractor,
    extractor_args = list(), filter = list(), mutate = list(), ...
) {
    standard.generic("model_impute") # nocov
  }
)

#' @rdname model_impute
#' @importFrom methods setMethod
setMethod(
  f = "model_impute",
  signature = signature(object = "ANY"),
  definition = function(
    object, model_fun, rhs, model_args = list(), extractor,
    extractor_args = list(), filter = list(), mutate = list(), ...
  ) {
    stop("model_impute() doesn't handle a '", class(object), "' object")
  }
)

#' @rdname model_impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @importFrom digest sha1
#' @importFrom dplyr bind_rows filter group_by mutate n row_number select
#' summarise transmute ungroup
#' @importFrom purrr map
#' @importFrom rlang .data !! !!! :=
#' @importFrom tibble rownames_to_column
#' @importFrom stats as.formula qnorm var
#' @examples
#' dataset <- generate_data(n_year = 10, n_site = 50, n_run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' imputed <- impute(data = dataset, model = model)
#' aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
#' extractor <- function(model) {
#'   summary(model)$coefficients[, c("Estimate", "Std. Error")]
#' }
#' model_impute(
#'   object = aggr,
#'   model_fun = lm,
#'   rhs = "0 + factor(Year)",
#'   extractor = extractor
#' )
#' @include aggregated_imputed_class.R
setMethod(
  f = "model_impute",
  signature = signature(object = "aggregatedImputed"),
  definition = function(
    object, model_fun, rhs, model_args = list(), extractor,
    extractor_args = list(), filter = list(), mutate = list(), ...
  ) {
    check_old_names(
      ...,
      old_names = c(
        model_fun = "model.fun", model_args = "model.args",
        extractor_args = "extractor.args"
      )
    )
    assert_that(
      inherits(model_fun, "function"), inherits(extractor, "function"),
      is.character(rhs), inherits(model_args, "list"),
      inherits(extractor_args, "list"), inherits(mutate, "list"),
      inherits(filter, "list")
    )
    id_column <- paste0("ID", sha1(Sys.time()))
    object@Covariate <- object@Covariate |>
      dplyr::mutate(!!id_column := row_number())
    map(filter, trans) |>
      c(.data = list(object@Covariate)) |>
      do.call(what = dplyr::filter) -> object@Covariate
    map(mutate, trans) |>
      c(.data = list(object@Covariate)) |>
      do.call(what = dplyr::mutate) -> object@Covariate
    object@Imputation <- object@Imputation[object@Covariate[[id_column]], ]

    paste("Imputed", rhs, sep = "~") |>
      as.formula() -> form
    m <- lapply(
      seq_len(ncol(object@Imputation)),
      function(i) {
        data <- cbind(Imputed = object@Imputation[, i], object@Covariate)
        model <- try(
          do.call(model_fun, c(form, list(data = data), model_args)),
          silent = TRUE
        )
        if (inherits(model, "try-error")) {
          NULL
        } else {
          do.call(extractor, c(list(model), extractor_args)) |>
            as.data.frame() |>
            rownames_to_column("Variable")
        }
      }
    )
    failed <- vapply(m, is.null, logical(1))
    assert_that(any(!failed), msg = "model failed on all imputations")
    m |>
      bind_rows() |>
      select(Parameter = 1, Estimate = 2, SE = 3) |>
      dplyr::mutate(
        Parameter = factor(.data$Parameter, levels = unique(.data$Parameter))
      ) -> m
    m |>
      group_by(.data$Parameter) |>
      summarise(
        SE = sqrt(mean(.data$SE ^ 2) + var(.data$Estimate) * (n() + 1) / n()),
        Estimate = mean(.data$Estimate)
      ) |>
      ungroup() |>
      transmute(
        .data$Parameter, .data$Estimate, .data$SE,
        LCL = qnorm(0.025, .data$Estimate, .data$SE),
        UCL = qnorm(0.975, .data$Estimate, .data$SE)
      ) -> result
    attr(result, "detail") <- m
    return(result)
  }
)

#' @importFrom rlang parse_expr
trans <- function(x) {
  stopifnot(inherits(x, "character") || inherits(x, "formula"))
  ifelse(is.character(x), x, as.character(x)[2]) |>
    parse_expr()
}
