#' @rdname impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that is.count
#' @include impute_generic.R
setMethod(
  f = "impute",
  signature = signature(model = "inla"),
  definition = function(model, ..., n.imp){
    assert_that(is.count(n.imp))

    if (!model$.args$control.predictor$compute) {
      stop(
"model must be fit with the 'compute = TRUE' argument of control.predictor"
      )
    }

    response <- as.character(model$.args$formula)[2]
    missing.obs <- which(is.na(model$.args$data[, response]))

    assert_that(requireNamespace("INLA", quietly = TRUE))
    imputation <- switch(
      model$.args$family,
      poisson = {
        linpred <- sapply(
          model$marginals.linear.predictor[missing.obs],
          INLA::inla.rmarginal,
          n = n.imp
        )
        matrix(
          rpois(length(linpred), lambda = exp(linpred)),
          ncol = n.imp,
          byrow = TRUE
        )
      },
      nbinomial = {
        linpred <- sapply(
          model$marginals.linear.predictor[missing.obs],
          INLA::inla.rmarginal,
          n = n.imp
        )
        h <- model$marginals.hyperpar
        size <- INLA::inla.rmarginal(
          n = n.imp,
          marginal = h$`size for the nbinomial observations (overdispersion)` # nolint
        )
        matrix(
          rnbinom(
            n = length(linpred),
            size = rep(size, ncol(linpred)),
            mu = exp(linpred)
          ),
          ncol = n.imp,
          byrow = TRUE
        )
      },
      stop(
        "Imputations from the '", model$.args$family, "' family not yet defined.
We will consider adding support for other families. Please create an issue with
a reproducible example at https://github.com/ThierryO/multimput/issues"
      )
    )

    new(
      "rawImputed",
      Data = model$.args$data,
      Response = response,
      Imputation = imputation,
      Minimum = ""
    )
  }
)
