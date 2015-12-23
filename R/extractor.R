#' Extract parameter estimated from a model
#' @param model model to extract the estimate and standard error of parameters
#' @param ... other arguments
#' @name extractor
#' @rdname extractor
#' @exportMethod extractor
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "extractor",
  def = function(model, ...){
    standard.generic("extractor") # nocov
  }
)

#' @rdname extractor
#' @importFrom methods setMethod
setMethod(
  f = "extractor",
  signature = signature(model = "ANY"),
  definition = function(model, ...){
    stop(
"Currently, no default extractor() for a ", class(model), " model is available.
Please provide an extractor function through the extractor.fun argument.
We will consider adding support for extra classes. Please create an issue with a
reproducible example at https://github.com/ThierryO/multimput/issues"
    )
  }
)
