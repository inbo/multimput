#' @rdname impute
#' @importFrom methods new setMethod
#' @importFrom assertthat assert_that has_name is.count
#' @importFrom mvtnorm rmvnorm
#' @importFrom digest sha1
#' @importFrom dplyr %>%
#' @importClassesFrom lme4 glmerMod
#' @importFrom stats model.matrix vcov rnorm as.formula rpois rbinom
#' @include impute_generic.R
setMethod(
  f = "impute",
  signature = signature(model = "glmerMod"),
  definition = function(model, data, ..., n.imp) {
    assert_that(requireNamespace("lme4", quietly = TRUE))
    assert_that(is.count(n.imp))
    assert_that(inherits(data, "data.frame"))
    if (any(grepl("factor\\(", model@call))) {
      stop(
"impute can't handle factor() in the model. Convert the factor in the dataset
and refit the model."
      )
    }

    response <- colnames(model@frame)[
      attr(
        attr(model@frame, "terms"),
        "response"
      )
    ]
    assert_that(has_name(data, response))
    missing.obs <- which(is.na(data[, response]))
    call <- model@call %>%
      as.character()
    mm <- call[grepl("~", call)] %>%
      gsub(pattern = "^.*~", replacement = "~") %>%
      gsub(pattern = "\\+ \\(.*\\|.*\\)", replacement = "") %>%
      as.formula() %>%
      model.matrix(data = data[missing.obs, ])
    fixed <- rmvnorm(
      n.imp,
      mean = lme4::fixef(model),
      sigma = vcov(model) %>%
        as.matrix()
    ) %>%
      tcrossprod(x = mm)
    random <- lapply(
      lme4::ranef(model, condVar = TRUE),
      function(x) {
        if (ncol(x) > 1) {
          stop("Random slopes are not yet handled.")
        } else {
          rnorm(
            n.imp * nrow(x),
            mean = x[, 1],
            sd = sqrt(attr(x, "postVar")[1, ,]) #nolint
          ) %>%
            matrix(ncol = n.imp)
        }
      }
    )
    eta <- lapply(
      names(random),
      function(x) {
        if (class(data[, x]) != "factor") {
          hash <- paste0("tmp", sha1(data[, x]))
          data[, hash] <- factor(data[, x])
        } else {
          hash <- x
        }
        paste("~0 + ", hash) %>% #nolint
            as.formula() %>%
            model.matrix(data = data[missing.obs, ]) %>%
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
      matrix(ncol = n.imp)
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
