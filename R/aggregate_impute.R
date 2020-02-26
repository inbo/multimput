#' Aggregate an imputed dataset
#' @param object A rawImputed object.
#' @param grouping A vector of variables names to group the aggregation on.
#' @param fun The function to aggregate.
#' @param filter
#' An optional argument to filter the raw dataset before aggregation.
#' Will be passed to \code{\link[dplyr]{filter}}.
#' @param join
#' An optional argument to filter the raw dataset based on a data.frame.
#' A \code{\link[dplyr]{semi_join}} will be applied with `join` or each element
#' of `join` in case join is a list.
#' @name aggregate_impute
#' @rdname aggregate_impute
#' @exportMethod aggregate_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "aggregate_impute",
  def = function(object, grouping, fun, filter, join) {
    standard.generic("aggregate_impute") # nocov
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "ANY"),
  definition = function(object, grouping, fun, filter, join) {
    stop(
"aggregate_impute() requires a 'rawImputed' or 'aggregatedImputed' object.
See ?impute or ?aggregate_impute"
    )
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @importFrom tidyr spread_
#' @importFrom dplyr %>% bind_rows filter funs group_by mutate_ n select_
#' semi_join starts_with summarise_at vars ungroup
#' @importFrom rlang expr parse_expr syms !! !!!
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom stats setNames na.omit
#' @importFrom digest sha1
#' @examples
#' dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' imputed <- impute(data = dataset, model = model)
#' aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
#' @include rawImputed_class.R
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "rawImputed"),
  definition = function(object, grouping, fun, filter, join) {
    assert_that(is.character(grouping))
    assert_that(inherits(fun, "function"))
    grouping <- syms(grouping)

    id_column <- paste0("ID", sha1(Sys.time()))
    minimum_column <- paste0("Minimum", sha1(Sys.time()))
    response <- object@Response
    data <- object@Data %>%
      mutate_(.dots = "ifelse(is.na(%1$s), cumsum(is.na(%1$s)), NA)" %>%
        sprintf(response) %>%
        setNames(id_column)
      )
    if (object@Minimum == "") {
      data <- data %>%
        mutate_(.dots = list(-Inf) %>%
          setNames(minimum_column)
        )
    } else {
      data <- data %>%
        mutate_(.dots = "%s" %>%
          sprintf(object@Minimum) %>%
          setNames(minimum_column)
        )
    }
    imputation <- object@Imputation

    if (!missing(filter)) {
      assert_that(is.list(filter))
      dots <- map(
        filter,
        ~expr(!!parse_expr(as.character(.x)[2]))
      )
      data <- data %>%
        filter(!!!dots)
    }

    if (!missing(join)) {
      if (inherits(join, "data.frame")) {
        join <- list(join)
      }
      assert_that(is.list(join))
      if (!all(sapply(join, inherits, "data.frame"))) {
        stop("not all objects in join are data.frames")
      }
      for (i in seq_along(join)) {
        if (!all(colnames(join[[i]]) %in% colnames(data))) {
          stop("all columns in join with be available in the dataset")
        }
        data <- data %>%
          semi_join(join[[i]], by = colnames(join[[i]]))
      }
    }

    imputation <- imputation[
      data[[id_column]] %>%
        na.omit(),
      ,
      drop = FALSE
    ]

    missing.obs <- which(is.na(data[, response]))
    total <- lapply(
      seq_len(ncol(imputation)),
      function(i) {
        data[missing.obs, response] <- pmax(
          imputation[, i],
          data[[minimum_column]][missing.obs],
          na.rm = TRUE
        )
        data %>%
          group_by(!!!grouping) %>%
          summarise_at(.funs = funs(fun), .vars = vars(response)) %>%
          mutate_(Imputation = ~sprintf("Imputation%04i", i))
      }
    ) %>%
      do.call(what = bind_rows) %>%
      spread_(key_col = "Imputation", value_col = response) %>%
      ungroup()
    new(
      "aggregatedImputed",
      Covariate = total %>%
        select_(~-starts_with("Imputation")) %>%
        as.data.frame(),
      Imputation = total %>%
        select_(~starts_with("Imputation")) %>%
        as.matrix()
    )
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter funs group_by inner_join mutate_ n semi_join
#' starts_with summarise_at vars
#' @importFrom methods new
#' @importFrom stats setNames
#' @importFrom digest sha1
#' @include aggregatedImputed_class.R
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "aggregatedImputed"),
  definition = function(object, grouping, fun, filter, join) {
    assert_that(is.character(grouping))
    assert_that(inherits(fun, "function"))

    id_column <- paste0("ID", sha1(Sys.time()))
    data <- object@Covariate %>%
      mutate_(.dots = "seq_along(%s)" %>%
          sprintf(grouping[1]) %>%
          setNames(id_column)
      )
    grouping <- syms(grouping)
    imputation <- object@Imputation %>%
      as.data.frame() %>%
      mutate_(.dots = "seq_along(Imputation0001)" %>%
          setNames(id_column)
      )

    if (!missing(filter)) {
      assert_that(is.list(filter))
      dots <- map(
        filter,
        ~expr(!!parse_expr(as.character(.x)[2]))
      )
      data <- data %>%
        filter(!!!dots)
    }

    if (!missing(join)) {
      if (inherits(join, "data.frame")) {
        join <- list(join)
      }
      assert_that(is.list(join))
      if (!all(sapply(join, inherits, "data.frame"))) {
        stop("not all objects in join are data.frames")
      }
      for (i in seq_along(join)) {
        if (!all(colnames(join[[i]]) %in% colnames(data))) {
          stop("all columns in join with be available in the dataset")
        }
        data <- data %>%
          semi_join(join[[i]], by = colnames(join[[i]]))
      }
    }

    total <- data %>%
      inner_join(imputation, by = id_column) %>%
      group_by(!!!grouping) %>%
      summarise_at(
        .funs = funs(fun),
        .vars = vars(colnames(object@Imputation))
      )

    new(
      "aggregatedImputed",
      Covariate = total %>%
        select_(~-starts_with("Imputation")) %>%
        as.data.frame(),
      Imputation = total %>%
        select_(~starts_with("Imputation")) %>%
        as.matrix()
    )
  }
)
