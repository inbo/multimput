#' @rdname impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that is.count
#' @importFrom stats rgamma
#' @include impute_generic.R
setMethod(
  f = "impute",
  signature = signature(model = "inla"),
  definition = function(model, ..., n.imp){
    assert_that(is.count(n.imp))


    if (!model$.args$control.compute$config) {
      stop(
"model must be fit with the 'config = TRUE' argument of control.compute"
      )
    }

    dots <- list(...)
    if (is.null(dots$minimum)) {
      dots$minimum <- ""
    }

    response <- as.character(model$.args$formula)[2]
    missing.obs <- which(is.na(model$.args$data[, response]))
    if (length(missing.obs) == 0) {
      return(
        new(
          "rawImputed",
          Data = model$.args$data,
          Response = response,
          Imputation = matrix(integer(0), ncol = n.imp),
          Minimum = dots$minimum
        )
      )
    }

    magnitude <- ceiling(log10(nrow(model$.args$data)))
    missing.obs <- sprintf(paste0("Predictor:%0", magnitude, "i"), missing.obs)

    assert_that(requireNamespace("INLA", quietly = TRUE))
    samples <- INLA::inla.posterior.sample(
      n = n.imp,
      model
    )
    imputation <- switch(
      model$.args$family,
      poisson = {
        sapply(
          samples,
          function(x) {
            rpois(
              n = length(missing.obs),
              lambda = exp(x$latent[missing.obs, 1])
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
              n = length(missing.obs),
              size = h[grepl("size for the nbinomial", names(h))],
              mu = exp(x$latent[missing.obs, 1]))
          }
        )
      },
      gamma = {
        sapply(
          samples,
          function(x) {
            h <- x$hyperpar
            prec <- h[grepl("Gamma observations", names(h))]
            mu <- exp(x$latent[missing.obs, 1])
            rate  <- prec * mu
            shape <- mu * rate
            rgamma(n = length(missing.obs), shape = shape, rate = rate)
          }
        )
      },
      gaussian = {
        sapply(
          samples,
          function(x) {
            h <- x$hyperpar
            rnorm(
              n = length(missing.obs),
              mean = x$latent[missing.obs, 1],
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
