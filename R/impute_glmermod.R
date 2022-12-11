#' @rdname impute
#' @importFrom methods new setMethod
#' @importFrom assertthat assert_that has_name is.count
#' @importFrom mvtnorm rmvnorm
#' @importFrom digest sha1
#' @importFrom dplyr %>%
#' @importClassesFrom lme4 glmerMod
#' @importFrom purrr map map_int map2
#' @importFrom stats model.matrix vcov rnorm as.formula rpois rbinom
#' @include impute_generic.R
setMethod(
  f = "impute",
  signature = signature(model = "glmerMod"),
  definition = function(model, data, ..., n_imp) {
    check_old_names(..., old_names = c(n_imp = "n.imp"))
    assert_that(requireNamespace("lme4", quietly = TRUE))
    assert_that(is.count(n_imp))
    assert_that(inherits(data, "data.frame"))
    assert_that(
      length(grep("factor\\(", model@call)) == 0,
      msg = "impute can't handle factor() in the model.
Convert the factor in the dataset and refit the model."
    )

    response <- colnames(model@frame)[
      attr(attr(model@frame, "terms"), "response")
    ]
    assert_that(has_name(data, response))
    missing_obs <- which(is.na(data[, response]))
    call <- as.character(model@call)
    mm <- call[grepl("~", call)] %>%
      gsub(pattern = "^.*~", replacement = "~") %>%
      gsub(pattern = "\\+ \\(.*\\|.*\\)", replacement = "") %>%
      as.formula() %>%
      model.matrix(data = data[missing_obs, ])
    fixed <- rmvnorm(
      n_imp, mean = lme4::fixef(model), sigma = as.matrix(vcov(model))
    ) %>%
      tcrossprod(x = mm)
    rf <- lme4::ranef(model, condVar = TRUE)
    assert_that(
      max(map_int(rf, ncol)) == 1,
      msg = "Random slopes are not yet handled."
    )
    map(rf, attr, which = "postVar") %>%
      map(as.vector) %>%
      map(sqrt) -> rf_sd
    map2(
      .x = rf, .y = rf_sd, n_imp = n_imp,
      ~rnorm(n = n_imp * nrow(.x), mean = .x[[1]], sd = .y)
    ) %>%
      map(matrix, ncol = n_imp) -> random
    eta <- lapply(
      names(random),
      function(x) {
        if (!inherits(data[, x], "factor")) {
          hash <- paste0("tmp", sha1(data[, x]))
          data[, hash] <- factor(data[, x])
        } else {
          hash <- x
        }
        paste("~0 + ", hash) %>% #nolint
            as.formula() %>%
            model.matrix(data = data[missing_obs, ]) %>%
            tcrossprod(t(random[[x]]))
      }
    ) %>%
      c(list(fixed)) %>%
      do.call(what = "+")
    mu <- model@resp$family$linkinv(eta)
    y <- switch(
      model@resp$family$family,
      "poisson" = rpois(length(mu), lambda = mu),
      "binomial" = rbinom(length(mu), prob = mu, size = 1),
      stop(model@resp$family$family, " family not yet handled.")
    ) %>%
      matrix(ncol = n_imp)
    dots <- list(...)
    if (is.null(dots$minimum)) {
      dots$minimum <- ""
    }
    new(
      "rawImputed",
      Data = data,
      Response = response,
      Imputation = y,
      Minimum = dots$minimum
    )
  }
)
