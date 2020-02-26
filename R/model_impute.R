#' Model an imputed dataset
#' @param object The imputed dataset.
#' @param model.fun The function to apply on each imputation set.
#' @param rhs The right hand side of the model.
#' @param model.args An optional list of arguments to pass to the model
#' function.
#' @param extractor A function which return a `matrix` or `data.frame`.
#' The first column should contain the estimate,
#' the second the standard error of the estimate.
#' @param extractor.args
#' An optional list of arguments to pass to the `extractor` function.
#' @inheritParams aggregate_impute
#' @param mutate An optional argument to alter the aggregated dataset.
#' Will be passed to the `.dots` argument of \code{\link[dplyr]{mutate_}}.
#' This is mainly useful for simple convertions, e.g. factors to numbers and
#' viceversa.
#' @name model_impute
#' @rdname model_impute
#' @exportMethod model_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "model_impute",
  def = function(
    object,
    model.fun,
    rhs,
    model.args,
    extractor,
    extractor.args,
    filter,
    mutate
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
    object,
    model.fun,
    rhs,
    model.args,
    extractor,
    extractor.args,
    filter,
    mutate
  ) {
    stop("model_impute() doesn't handle a '", class(object), "' object")
  }
)

#' @rdname model_impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% bind_rows filter group_by mutate_ n summarise_
#' transmute_
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#' @importFrom stats var
#' @examples
#' dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' imputed <- impute(data = dataset, model = model)
#' aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
#' extractor <- function(model) {
#'   summary(model)$coefficients[, c("Estimate", "Std. Error")]
#' }
#' model_impute(
#'   object = aggr,
#'   model.fun = lm,
#'   rhs = "0 + factor(Year)",
#'   extractor = extractor
#' )
#' @include aggregatedImputed_class.R
setMethod(
  f = "model_impute",
  signature = signature(object = "aggregatedImputed"),
  definition = function(
    object,
    model.fun,
    rhs,
    model.args,
    extractor,
    extractor.args,
    filter,
    mutate
  ) {
    assert_that(inherits(model.fun, "function"))
    assert_that(inherits(extractor, "function"))
    assert_that(is.character(rhs))

    if (missing(model.args)) {
      model.args <- list()
    } else {
      assert_that(inherits(model.args, "list"))
    }
    if (missing(extractor.args)) {
      extractor.args <- list()
    } else {
      assert_that(inherits(extractor.args, "list"))
    }

    id_column <- paste0("ID", sha1(Sys.time()))
    object@Covariate <- object@Covariate %>%
      mutate_(.dots = setNames("seq_len(n())", id_column))
    if (!missing(filter)) {
      dots <- map(
        filter,
        ~expr(!!parse_expr(as.character(.x)[2]))
      )
      object@Covariate <- object@Covariate %>%
        filter(!!!dots)
    }
    if (!missing(mutate)) {
      object@Covariate <- object@Covariate %>%
        mutate_(.dots = mutate)
    }

    object@Imputation <- object@Imputation[object@Covariate[[id_column]], ]

    form <- as.formula(paste("Imputed", rhs, sep = "~"))
    m <- lapply(
      seq_len(ncol(object@Imputation)),
      function(i) {
        data <- cbind(
          Imputed = object@Imputation[, i],
          object@Covariate
        )
        model <- try(
          do.call(model.fun, c(form, list(data = data), model.args)),
          silent = TRUE
        )
        if (inherits(model, "try-error")) {
          NULL
        } else {
          do.call(extractor, c(list(model), extractor.args)) %>%
            as.data.frame() %>%
            rownames_to_column("Variable")
        }
      }
    )
    failed <- sapply(m, is.null)
    if (all(failed)) {
      stop("model failed on all imputations")
    }
    m %>%
      bind_rows() %>%
      select_(Parameter = 1, Estimate = 2, SE = 3) %>%
      mutate_(
        Parameter = ~factor(Parameter, levels = unique(Parameter))
      ) -> m
    m %>%
      group_by(.data$Parameter) %>%
      summarise_(
        SE = ~sqrt(mean(SE ^ 2) + var(Estimate) * (n() + 1) / n()),
        Estimate = ~mean(Estimate)
      ) %>%
      ungroup() %>%
      transmute_(
        ~Parameter,
        ~Estimate,
        ~SE,
        LCL = ~qnorm(0.025, Estimate, SE),
        UCL = ~qnorm(0.975, Estimate, SE)
      ) -> result
    attr(result, "detail") <- m
    return(result)
  }
)
