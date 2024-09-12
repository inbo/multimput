#' Combine two models into a hurdle model
#'
#' Multiplies the imputed values for the `presence` model with those of the
#' `count` model.
#' Please make sure that the order of the observations in both models is
#' identical.
#' The resulting object will contain the union of the covariates of both models.
#' Variables with the same name and different values get a `presence_` or
#' `count_` prefix.
#' @param presence the `rawImputed` object for the presence.
#' @param count the `rawImputed` object for counts.
#' @export
#' @importFrom methods new validObject
hurdle_impute <- function(presence, count) {
  stopifnot(
    "`presence` is not a `rawImputed` object" =
      inherits(presence, "rawImputed"),
    "`count` is not a `rawImputed` object" =
      inherits(count, "rawImputed"),
    validObject(presence), validObject(count),
    "unequal number of rows in count and presence" =
      nrow(count@Data) == nrow(presence@Data),
    "unequal number of imputations in count and presence" =
      ncol(count@Imputation) == ncol(presence@Imputation),
    "Only add extra observations to the `count` model" =
      nrow(presence@Extra) == 0
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
  colnames(ren_count) <- sprintf("count_%s", colnames(ren_count))
  ren_presence <- cv_presence[, extra_presence[extra_presence %in% extra_count]]
  colnames(ren_presence) <- sprintf("presence_%s", colnames(ren_presence))
  cv <- cbind(
    cv_count[, common],
    cv_count[, extra_count[!extra_count %in% extra_presence]], ren_count,
    cv_presence[, extra_presence[!extra_presence %in% extra_count]],
    ren_presence
  )
  if (nrow(count@Extra) == 0) {
    extra <- matrix(nrow = 0, ncol = ncol(count_resp))
  } else {
    extra <- matrix(
      count@Extra[[count@Response]], nrow = nrow(count@Extra),
      ncol = ncol(count_resp)
    )
    ren_count <- count@Extra[, extra_count[extra_count %in% extra_presence]]
    colnames(ren_count) <- sprintf("count_%s", colnames(ren_count))
    cv_extra <- cbind(
      count@Extra[, common],
      count@Extra[, extra_count[!extra_count %in% extra_presence]], ren_count
    )
    c(
      extra_presence[!extra_presence %in% extra_count],
      extra_presence[extra_presence %in% extra_count] |>
        sprintf(fmt = "presence_%s")
    ) -> extra_presence
    matrix(NA, ncol = length(extra_presence), nrow = nrow(cv_extra)) |>
      `colnames<-`(extra_presence) |>
      as.data.frame() -> cv_extra_presence
    cv <- rbind(cv, cbind(cv_extra, cv_extra_presence))
  }

  new(
    "aggregatedImputed", Covariate = cv,
    Imputation = rbind(presence_resp * count_resp, extra) |>
      `colnames<-`(sprintf("Imputation%04i", seq_len(ncol(count_resp))))
  )
}
