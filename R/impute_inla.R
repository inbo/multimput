#' @rdname impute
#' @importFrom methods new setMethod
#' @importFrom assertthat assert_that is.count
#' @importFrom stats rgamma rnorm rpois
#' @include impute_generic.R
#' @param seed See the same argument in [INLA::inla.qsample()] for further
#' information.
#' In order to produce reproducible results, you ALSO need to make sure the RNG
#' in R is in the same state, see the example in
#' [INLA::inla.posterior.sample()].
#' When seed is non-zero, `num_threads` is forced to `"1:1"` and
#' `parallel_configs` is set to `FALSE`, since parallel sampling would not
#' produce a reproducible sequence of pseudo-random numbers.
#' @param num_threads The number of threads to use in the format `"A:B"`
#' defining the number threads in the outer (`A`) and inner (`B`) layer for
#' nested parallelism.
#' `A "0"` will be replaced intelligently.
#' `seed != 0` requires serial computations.
#' @param parallel_configs Logical.
#' If TRUE and not on Windows, then try to run each configuration in parallel
#' (not Windows) using `A` threads (see `num_threads`), where each of them is
#' using `B:0` threads.
setMethod(
  f = "impute",
  signature = signature(model = "inla"),
  definition = function(
    model, ..., seed = 0L, num_threads = NULL, parallel_configs = TRUE,
    n_imp = 19
  ) {
    check_old_names(..., old_names = c(n_imp = "n.imp"))
    assert_that(is.count(n_imp))
    assert_that(
      model$.args$control.compute$config,
      msg =
"model must be fit with the 'config = TRUE' argument of control.compute"
    )

    dots <- list(...)
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
    assert_that(requireNamespace("sn", quietly = TRUE))
    samples <- INLA::inla.posterior.sample(
      n = n_imp, result = model, seed = seed, num.threads = num_threads,
      parallel.configs = parallel_configs
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
