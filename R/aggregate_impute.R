#' Aggregate an imputed dataset
#' @param object a rawImputed object
#' @param grouping a vector of variables names to group the aggregation on
#' @param fun the function to aggregate
#' @name aggregate_impute
#' @rdname aggregate_impute
#' @exportMethod aggregate_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "aggregate_impute",
  def = function(object, grouping, fun){
    standard.generic("aggregate_impute") # nocov
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
#' @examples
#' @include rawImputed_class.R
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "rawImputed"),
  definition = function(object, grouping, fun){
    response <- object@Response
    data <- object@Data
    missing.obs <- which(is.na(data[, response]))
    imputation <- object@Imputation
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
      spread_(key_col = "Imputation", value_col = response)
    new(
      "aggregatedImputed",
      Covariate = total %>%
        select(-starts_with("Imputation")) %>%
        as.data.frame(),
      Imputation = total %>%
        select(starts_with("Imputation")) %>%
        as.matrix()
    )
  }
)
