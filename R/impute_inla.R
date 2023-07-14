#' @rdname impute
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr coalesce
#' @importFrom methods new setMethod
#' @importFrom purrr map map_dfr map2_dfr pmap_dfr
#' @importFrom stats plogis qpois rgamma rnorm rpois setNames
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
  signature = signature(model = "maybeInla"),
  definition = function(
    model, ..., seed = 0L, num_threads = NULL, parallel_configs = TRUE, extra,
    n_imp = 19
  ) {
    assert_that(!is.null(model), msg = "model should be an inla object")
    check_old_names(..., old_names = c(n_imp = "n.imp"))
    assert_that(is.count(n_imp))
    assert_that(
      model$.args$control.compute$config,
      msg =
"model must be fit with the 'config = TRUE' argument of control.compute"
    )

    dots <- list(...)
    if (missing(extra)) {
      extra <- model$.args$data[0, ]
    }

    response <- as.character(model$.args$formula)[2]
    missing_obs <- which(is.na(model$.args$data[, response]))
    if (length(missing_obs) == 0) {
      return(
        new(
          "rawImputed", Data = model$.args$data, Response = response,
          Imputation = matrix(integer(0), ncol = n_imp),
          Minimum = coalesce(dots$minimum, ""), Extra = extra
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
    map(samples, "latent") |>
      map(`[`, missing_obs, 1) |>
      setNames(paste0("sim_", seq_len(n_imp))) -> latent
    INLA::inla.hyperpar.sample(n = n_imp, result = model) |>
      as.data.frame() -> hyperpar
    if (
      model$.args$family == "zeroinflatednbinomial0" &&
      !"zero-probability parameter for zero-inflated nbinomial_0" %in%
        colnames(hyperpar)
    ) {
      hyperpar[["zero-probability parameter for zero-inflated nbinomial_0"]] <-
        plogis(model$.args$control.family[[1]]$hyper$theta2$initial)
    }

    imputation <- switch(
      model$.args$family,
      binomial = map_dfr(
        .x = latent,
        .f = ~rbinom(n = length(missing_obs), size = 1, prob = plogis(.x))
      ),
      gamma = map2_dfr(
        .x = latent, .f = ~rgamma(n = n, shape = .y * .x ^ 2, rate = .y * .x),
        n = length(missing_obs),
        .y = hyperpar[[grep("Gamma observations", colnames(hyperpar))]]
      ),
      gaussian = map2_dfr(
        .x = latent, .f = rnorm, n = length(missing_obs),
        .y = hyperpar[[grep("Gaussian observations", colnames(hyperpar))]]
      ),
      nbinomial = map2_dfr(
        .x = latent,
        .f = ~rnbinom(size = .y, mu = exp(.x), n = length(missing_obs)),
        .y = hyperpar[[grep("size for the nbinomial", colnames(hyperpar))]]
      ),
      poisson = map_dfr(
        .x = latent, .f = ~rpois(n = length(missing_obs), lambda = exp(.x))
      ),
      zeroinflatednbinomial0 = pmap_dfr(
        list(
          eta = latent,
          prob = hyperpar[[grep("zero-probability", colnames(hyperpar))]],
          size = hyperpar[[grep("size for nbinomial", colnames(hyperpar))]]
        ),
        .f = function(n, eta, prob, size) {
          rzinb0(
            n = length(missing_obs), mu = exp(eta), prob = prob, size = size
          )
        }
      ),
      zeroinflatednbinomial1 = pmap_dfr(
        list(
          eta = latent,
          prob = hyperpar[[grep("zero-probability", colnames(hyperpar))]],
          size = hyperpar[[grep("size for nbinomial", colnames(hyperpar))]]
        ),
        .f = function(n, eta, prob, size) {
          rzinb1(
            n = length(missing_obs), mu = exp(eta), prob = prob, size = size
          )
        }
      ),
      zeroinflatedpoisson0 = map2_dfr(
        .x = latent,
        .y = hyperpar[[grep("zero-probability", colnames(hyperpar))]],
        .f = ~rzip0(n = length(missing_obs), lambda = exp(.x), prob = .y)
      ),
      zeroinflatedpoisson1 = map2_dfr(
        .x = latent,
        .y = hyperpar[[grep("zero-probability", colnames(hyperpar))]],
        .f = ~rzip1(n = length(missing_obs), lambda = exp(.x), prob = .y)
      ),
      stop(
        "Imputations from the '", model$.args$family, "' family not yet defined.
We will consider adding support for other families. Please create an issue with
a reproducible example at https://github.com/inbo/multimput/issues"
      )
    )

    new(
      "rawImputed", Data = model$.args$data, Response = response,
      Imputation = as.matrix(imputation), Minimum = coalesce(dots$minimum, ""),
      Extra = extra
    )
  }
)

#' @importFrom assertthat assert_that is.count is.number noNA
#' @importFrom stats dnbinom qnbinom rbinom rnbinom runif
rzinb0 <- function(n, mu, size, prob, tol = 2e-10) {
  assert_that(
    is.count(n), noNA(n), is.numeric(mu), noNA(mu), is.numeric(size),
    noNA(size), is.numeric(prob), noNA(prob), is.number(tol)
  )
  assert_that(
    tol > 0, tol < 1e-5, length(mu) %in% c(1, n), length(size) %in% c(1, n),
    length(prob) %in% c(1, n), all(prob >= 0), all(prob <= 1), all(size > 0)
  )
  count <- rbinom(n = n, size = 1, prob = 1 - prob)
  non_zero <- which(count == 1)
  n <- length(non_zero)
  if (n == 0) {
    return(count)
  }
  if (length(mu) == 1) {
    mu <- rep(mu, n)
  } else {
    mu <- mu[non_zero]
  }
  if (length(size) == 1) {
    size <- rep(size, n)
  } else {
    size <- size[non_zero]
  }
  low <- which(mu < tol)
  if (length(low) == 0) {
    dnbinom(x = 0, mu = mu, size = size) |>
      runif(n = n, max = 1) |>
      qnbinom(mu = mu, size = size) -> count[non_zero]
    return(count)
  }
  if (length(low) == n) {
    return(count)
  }
  dnbinom(x = 0, mu = mu[-low], size = size) |>
    runif(n = n - length(low), max = 1) |>
    qnbinom(mu = mu[-low], size = size) -> count[non_zero][-low]
  return(count)
}

#' @importFrom stats rbinom rnbinom
rzinb1 <- function(n, mu, size, prob) {
  rbinom(n = n, size = 1, prob = 1 - prob) * rnbinom(n, mu = mu, size = size)
}

#' @importFrom stats dpois qpois rbinom rpois runif
rzip0 <- function(n, lambda, prob, tol = 2e-10) {
  count <- rbinom(n = n, size = 1, prob = 1 - prob)
  non_zero <- which(count == 1)
  n <- length(non_zero)
  if (n == 0) {
    return(count)
  }
  if (length(lambda) == 1) {
    lambda <- rep(lambda, n)
  } else {
    lambda <- lambda[non_zero]
  }
  low <- which(lambda < tol)
  if (length(low) == 0) {
    dpois(x = 0, lambda = lambda) |>
      runif(n = n, max = 1) |>
      qpois(lambda = lambda) -> count[non_zero]
    return(count)
  }
  if (length(low) == n) {
    return(count)
  }
  dpois(0, lambda[-low]) |>
    runif(n = n - length(low), max = 1) |>
    qpois(lambda = lambda[-low]) -> count[non_zero][-low]
  return(count)
}

#' @importFrom stats rbinom rpois
rzip1 <- function(n, lambda, prob) {
  rbinom(n = n, size = 1, prob = 1 - prob) * rpois(n, lambda = lambda)
}
