#' @rdname impute
#' @importFrom methods new setMethod
#' @importFrom assertthat assert_that is.count
#' @importFrom stats rgamma rnorm rpois
#' @include impute_generic.R
setMethod(
  f = "impute",
  signature = signature(model = "inla"),
  definition = function(model, ..., n_imp) {
    dots <- list(...)
    assert_that(
      !has_name(dots, "n.imp"), msg = "please use `n_imp` instead of `n.imp`"
    )
    assert_that(is.count(n_imp))
    assert_that(
      model$.args$control.compute$config,
      msg =
"model must be fit with the 'config = TRUE' argument of control.compute"
    )

    if (is.null(dots$minimum)) {
      dots$minimum <- ""
    }

    response <- as.character(model$.args$formula)[2]
    missing_obs <- which(is.na(model$.args$data[, response]))
    if (length(missing_obs) == 0) {
      return(
        new(
          "rawImputed",
          Data = model$.args$data,
          Response = response,
          Imputation = matrix(integer(0), ncol = n_imp),
          Minimum = dots$minimum
        )
      )
    }

    missing_obs <- sprintf(paste0("Predictor:%i"), missing_obs)

    assert_that(requireNamespace("INLA", quietly = TRUE))
    samples <- INLA::inla.posterior.sample(
      n = n_imp,
      model
    )
    imputation <- switch(
      model$.args$family,
      poisson = {
        sapply(
          samples,
          function(x) {
            rpois(
              n = length(missing_obs),
              lambda = exp(x$latent[missing_obs, 1])
            )
          }
        )
      },
      nbinomial = {
        sapply(
          samples,
          function(x) {
            h <- x$hyperpar
            rnbinom(
              n = length(missing_obs),
              size = h[grepl("size for the nbinomial", names(h))],
              mu = exp(x$latent[missing_obs, 1]))
          }
        )
      },
      gamma = {
        sapply(
          samples,
          function(x) {
            h <- x$hyperpar
            prec <- h[grepl("Gamma observations", names(h))]
            mu <- exp(x$latent[missing_obs, 1])
            rate  <- prec * mu
            shape <- mu * rate
            rgamma(n = length(missing_obs), shape = shape, rate = rate)
          }
        )
      },
      gaussian = {
        sapply(
          samples,
          function(x) {
            h <- x$hyperpar
            rnorm(
              n = length(missing_obs),
              mean = x$latent[missing_obs, 1],
              sd = h[grepl("Gaussian observations", names(h))]
            )
          }
        )
      },
      stop(
        "Imputations from the '", model$.args$family, "' family not yet defined.
We will consider adding support for other families. Please create an issue with
a reproducible example at https://github.com/inbo/multimput/issues"
      )
    )

    new(
      "rawImputed",
      Data = model$.args$data,
      Response = response,
      Imputation = imputation,
      Minimum = dots$minimum
    )
  }
)
