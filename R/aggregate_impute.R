#' Aggregate an imputed dataset
#' @param object a rawImputed object
#' @param grouping a vector of variables names to group the aggregation on
#' @param fun the function to aggregate
#' @param filter an optional argument to filter the raw dataset before aggregation. Will be passed to the \code{.dots} argument of \code{\link[dplyr]{filter_}}
#' @name aggregate_impute
#' @rdname aggregate_impute
#' @exportMethod aggregate_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "aggregate_impute",
  def = function(object, grouping, fun, filter){
    standard.generic("aggregate_impute") # nocov
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "ANY"),
  definition = function(object, grouping, fun, filter){
    stop("aggregate_impute() requires a 'rawImputed' object. See ?impute")
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @importFrom tidyr spread_
#' @import dplyr
#' @importFrom dplyr %>% group_by_ summarise_each_ funs mutate_ bind_rows ungroup select_ filter_ n
#' @importFrom methods new
#' @importFrom stats setNames na.omit
#' @importFrom digest sha1
#' @examples
#' dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' imputed <- impute(data = dataset, model = model)
#' aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
#' @include rawImputed_class.R
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "rawImputed"),
  definition = function(object, grouping, fun, filter){
    assert_that(is.character(grouping))
    assert_that(inherits(fun, "function"))

    response <- object@Response
    data <- object@Data
    imputation <- object@Imputation
    if (!missing(filter)) {
      assert_that(is.list(filter))
      id_column <- paste0("ID", sha1(data))
      data <- data %>%
        mutate_(.dots = "ifelse(is.na(%1$s), cumsum(is.na(%1$s)), NA)" %>%
          sprintf(response) %>%
          setNames(id_column)
        ) %>%
        filter_(.dots = filter)
      imputation <- imputation[
        data[[id_column]] %>%
          na.omit(),
      ]
    }

    missing.obs <- which(is.na(data[, response]))
    total <- lapply(
      seq_len(ncol(imputation)),
      function(i) {
        data[missing.obs, response] <- imputation[, i]
        data %>%
          group_by_(.dots = grouping) %>%
          summarise_each_(funs = funs(fun), vars = response) %>%
          mutate_(Imputation = ~sprintf("Imputation%04i", i))
      }
    ) %>%
      do.call(what = bind_rows) %>%
      spread_(key_col = "Imputation", value_col = response) %>%
      ungroup()
    new(
      "aggregatedImputed",
      Covariate = total %>%
        select_(~-starts_with("Imputation")) %>%
        as.data.frame(),
      Imputation = total %>%
        select_(~starts_with("Imputation")) %>%
        as.matrix()
    )
  }
)
