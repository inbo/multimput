#' The `rawImputed` class
#' Holds a dataset and imputed values
#' @slot Data A data.frame with the data.
#' @slot Response A character holding the name of the response variable.
#' @slot Minimum An optional character holding the name of the variable with the
#' minimum.
#' @slot Imputation A matrix with imputed values.
#' @slot Extra A data.frame with extra data to add to the imputations.
#' This data is not used in the imputation model.
#' It must contain the same variables as the original data.
#' @name rawImputed-class
#' @rdname rawImputed-class
#' @exportClass rawImputed
#' @aliases rawImputed-class
#' @importFrom methods setClass
#' @docType class
setClass(
  Class = "rawImputed",
  representation = representation(
    Data = "data.frame", Response = "character", Minimum = "character",
    Imputation = "matrix", Extra = "data.frame"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name is.string
setValidity(
  "rawImputed",
  function(object) {
    assert_that(is.string(object@Response))
    assert_that(has_name(object@Data, object@Response))

    assert_that(
      all(colnames(object@Data) %in% colnames(object@Extra)),
      msg = "All colnames in `Extra` must be contain all variables in `Data`"
    )
    assert_that(
      all(!is.na(object@Extra[[object@Response]])),
      msg = "Response variable in `Extra` contains `NA` values."
    )

    assert_that(length(object@Minimum) == 1)
    if (!is.na(object@Minimum) && object@Minimum != "") {
      assert_that(has_name(object@Data, object@Minimum))
    }
  }
)
