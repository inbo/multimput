#' Impute a dataset
#' @param model model to impute the dataset
#' @param ... other arguments. See details
#' @param n.imp the number of imputations. Defaults to 19
#' @name impute
#' @rdname impute
#' @exportMethod impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "impute",
  def = function(model, ..., n.imp = 19){
    standard.generic("impute") # nocov
  }
)
