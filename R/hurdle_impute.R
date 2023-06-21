#' Combine two models into a hurdle model
#'
#' Multiplies the imputed values for the `presence` model with those of the
#' `count` model.
#' Please make sure that the order of the observations in both models is
#' identical.
#' The resulting object will contain the union of the covariates of both models.
#' Variables with the same name and different values get a `presence_` or
#' `count_` prefix.
#' @param presence the model for the presence.
#' @param count the model for counts.
#' @export
#' @importFrom methods new
hurdle_impute <- function(presence, count) {
  stopifnot(
    "`presence` is not a `rawImputed` object" =
      inherits(presence, "rawImputed"),
    "`count` is not a `rawImputed` object" =
      inherits(count, "rawImputed"),
    "unequal number of rows in count and presence" =
      nrow(count@Data) == nrow(presence@Data),
    "unequal number of imputations in count and presence" =
      ncol(count@Imputation) == ncol(presence@Imputation)
  )

  # prepare imputations
  count@Data[[count@Response]] |>
    matrix(nrow = nrow(count@Data), ncol = ncol(count@Imputation)) -> count_resp
  count_resp[is.na(count@Data[[count@Response]]), ] <- count@Imputation
  if (count@Minimum != "") {
    minimum <- count@Data[[count@Minimum]]
    relevant <- !is.na(minimum) & minimum > 0
    minimum[relevant] |>
      matrix(nrow = sum(relevant), ncol = ncol(count@Imputation)) |>
      pmax(count_resp[relevant, ]) -> count_resp[relevant, ]
  }
  presence@Data[[presence@Response]] |>
    matrix(
      nrow = nrow(presence@Data), ncol = ncol(presence@Imputation)
    ) -> presence_resp
  presence_resp[
    is.na(presence@Data[[presence@Response]]),
  ] <- presence@Imputation
  if (presence@Minimum != "") {
    minimum <- presence@Data[[presence@Minimum]]
    relevant <- !is.na(minimum) & minimum > 0
    minimum[relevant] |>
      matrix(nrow = sum(relevant), ncol = ncol(presence@Imputation)) |>
      pmax(presence_resp[relevant, ]) -> presence_resp[relevant, ]
  }

  # prepare covariates
  cv_count <- count@Data[
    , !colnames(count@Data) %in% c(count@Response, count@Minimum)
  ]
  cv_presence <- presence@Data[
    , !colnames(presence@Data) %in% c(presence@Response, presence@Minimum)
  ]
  common <- colnames(cv_count)[colnames(cv_count) %in% colnames(cv_presence)]
  common <- common[apply(cv_count[, common] == cv_presence[, common], 2, all)]
  extra_count <- colnames(cv_count)[!colnames(cv_count) %in% common]
  extra_presence <- colnames(cv_presence)[!colnames(cv_presence) %in% common]
  ren_count <- cv_count[, extra_count[extra_count %in% extra_presence]]
  colnames(ren_count) <- sprintf("count_", colnames(ren_count))
  ren_presence <- cv_presence[, extra_presence[extra_presence %in% extra_count]]
  colnames(ren_presence) <- sprintf("presence_", colnames(ren_presence))

  new(
    "aggregatedImputed",
    Covariate = cbind(
      cv_count[, common],
      cv_count[, extra_count[!extra_count %in% extra_presence]], ren_count,
      cv_presence[, extra_presence[!extra_presence %in% extra_count]],
      ren_presence
    ),
    Imputation = presence_resp * count_resp
  )
}
