#' @importFrom methods setOldClass
setOldClass("inla")

#' The `maybeInla` class
#'
#' A superclass holding either `NULL` or an object of the `inla` class.
#' @importFrom methods setClassUnion
#' @exportClass maybeInla
setClassUnion("maybeInla", "NULL")

#' @importFrom methods getClassDef setIs
.onLoad <- function(...) {
  if (requireNamespace("INLA", quietly = TRUE)) {
    setIs("inla", "maybeInla", classDef = getClassDef("inla", package = "INLA"))
  }
}
