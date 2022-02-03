#' Impute a dataset
#' @param model model to impute the dataset
#' @param ... other arguments.
#' See details
#' @param n_imp the number of imputations.
#' Defaults to `19`.
#' @name impute
#' @rdname impute
#' @exportMethod impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "impute",
  def = function(model, ..., n_imp = 19) {
    standard.generic("impute") # nocov
  }
)

#' @rdname impute
#' @importFrom methods setMethod
setMethod(
  f = "impute",
  signature = signature(model = "ANY"),
  definition = function(model, ..., n_imp) {
    stop(
"impute() can't handle a model of class ", class(model), " at this moment.
We will consider adding support for extra classes. Please create an issue with a
reproducible example at https://github.com/inbo/multimput/issues"
    )
  }
)
