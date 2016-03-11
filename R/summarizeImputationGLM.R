#' summarize the imputed dataset with a glm model
#' @param data the multiple imputed dataset
#' @param rhs the right hand side of the glm model to summarize the imputed data
#' @export
summarizeImputationGLM <- function(data, rhs){
  .Deprecated(
    new = "model_impute"
  )
  imputations <- grep("^Imputation", colnames(data))
  raw.output <- do.call(rbind, lapply(seq_along(imputations), function(i){
    model <- glm(
      as.formula(
        paste(
          colnames(data)[imputations[i]],
          rhs,
          sep = "~"
        )
      ),
      data = data,
      family = poisson
    )
    tmp <- coef(summary(model))[, c("Estimate", "Std. Error")]
    data.frame(
      Run = i,
      Parameter = as.factor(rownames(tmp)),
      Index = tmp[, "Estimate"],
      Var = tmp[, "Std. Error"] ^ 2
    )
  }))
  output <- aggregate(
    cbind(Index, Var) ~ Parameter,
    data = raw.output,
    FUN = mean
  )
  B <- aggregate(
    Index ~ Parameter,
    data = raw.output,
    FUN = var
  )
  output$SE <- sqrt(output$Var + (1 + 1 / length(imputations)) * B$Index)
  output$Var <- NULL
  output
}
