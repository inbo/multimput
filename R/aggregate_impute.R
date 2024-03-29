#' Aggregate an imputed dataset
#' @param object A `rawImputed` object.
#' @param grouping A vector of variables names to group the aggregation on.
#' @param fun The function to aggregate.
#' @param filter
#' An optional argument to filter the raw dataset before aggregation.
#' Will be passed to [dplyr::filter()].
#' @param join
#' An optional argument to filter the raw dataset based on a data.frame.
#' A [dplyr::semi_join()] will be applied with `join` or each element
#' of `join` in case join is a list.
#' @name aggregate_impute
#' @rdname aggregate_impute
#' @exportMethod aggregate_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "aggregate_impute",
  def = function(object, grouping, fun, filter = list(), join) {
    standard.generic("aggregate_impute") # nocov
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "ANY"),
  definition = function(object, grouping, fun, filter = list(), join) {
    stop(
"aggregate_impute() requires a 'rawImputed' or 'aggregatedImputed' object.
See ?impute or ?aggregate_impute"
    )
  }
)

#' @rdname aggregate_impute
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
#' @importFrom digest sha1
#' @importFrom dplyr across bind_rows filter group_by mutate n select
#' semi_join starts_with summarise ungroup
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom rlang expr parse_expr syms !! !!! :=
#' @importFrom stats na.omit
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @examples
#' dataset <- generate_data(n_year = 10, n_site = 50, n_run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' imputed <- impute(data = dataset, model = model)
#' aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
#' @include raw_imputed_class.R
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "rawImputed"),
  definition = function(object, grouping, fun, filter = list(), join) {
    assert_that(
      is.character(grouping), inherits(fun, "function"),
      inherits(filter, "list")
    )
    grouping <- syms(grouping)
    id_column <- paste0("ID", sha1(Sys.time()))
    minimum_column <- paste0("Minimum", sha1(Sys.time()))
    response <- object@Response
    dots <- expr(
      !!parse_expr(
        sprintf("ifelse(is.na(%1$s), cumsum(is.na(%1$s)), NA)", response)
      )
    )
    data <- object@Data |>
      mutate(!!id_column := !!dots) |>
      bind_rows(object@Extra)
    if (object@Minimum == "") {
      data <- data |>
        mutate(!!minimum_column := -Inf)
    } else {
      data <- data |>
        mutate(!!minimum_column := !!parse_expr(object@Minimum))
    }
    imputation <- object@Imputation

    map(filter, trans) |>
      c(.data = list(data)) |>
      do.call(what = dplyr::filter) -> data
    if (!missing(join)) {
      if (inherits(join, "data.frame")) {
        join <- list(join)
      }
      assert_that(is.list(join))

      assert_that(
        all(vapply(join, inherits, logical(1), "data.frame")),
        msg = "not all objects in join are data.frames"
      )
      for (i in seq_along(join)) {
        assert_that(
          all(colnames(join[[i]]) %in% colnames(data)),
          msg = "all columns in join with be available in the dataset"
        )
        data <- data |>
          semi_join(join[[i]], by = colnames(join[[i]]))
      }
    }

    if (nrow(data) == 0) {
      return(
        new(
          "aggregatedImputed",
          Covariate = data |>
            select(!!!grouping) |>
            as.data.frame(),
          Imputation = ncol(imputation) |>
            seq_len() |>
            sprintf(fmt = "Imputation%04i") |>
            list() |>
            c(list(character(0))) |>
            rev() |>
            matrix(
              data = NA_real_, nrow = 0, ncol = ncol(imputation),
              byrow = FALSE
            )
        )
      )
    }

    imputation <- imputation[na.omit(data[[id_column]]), , drop = FALSE]

    missing_obs <- which(is.na(data[, response]))
    total <- lapply(
      seq_len(ncol(imputation)),
      function(i) {
        data[missing_obs, response] <- pmax(
          imputation[, i], data[[minimum_column]][missing_obs], na.rm = TRUE
        )
        data |>
          group_by(!!!grouping) |>
          summarise(
            across(
              .cols = all_of(response), .fns = list(fun), .names = "{.col}"
            ), .groups = "drop"
          ) |>
          mutate(Imputation = !!sprintf("Imputation%04i", i))
      }
    ) |>
      bind_rows() |>
      pivot_wider(names_from = "Imputation", values_from = all_of(response)) |>
      ungroup()
    new(
      "aggregatedImputed",
      Covariate = total |>
        select(-starts_with("Imputation")) |>
        as.data.frame(),
      Imputation = total |>
        select(starts_with("Imputation")) |>
        as.matrix()
    )
  }
)

#' @rdname aggregate_impute
#' @importFrom assertthat assert_that
#' @importFrom digest sha1
#' @importFrom dplyr across filter group_by inner_join mutate n row_number
#' select semi_join starts_with
#' @importFrom methods new setMethod
#' @importFrom rlang !! !!! :=
#' @importFrom tidyselect all_of
#' @include aggregated_imputed_class.R
setMethod(
  f = "aggregate_impute",
  signature = signature(object = "aggregatedImputed"),
  definition = function(object, grouping, fun, filter = list(), join) {
    assert_that(
      is.character(grouping), inherits(fun, "function"),
      inherits(filter, "list")
    )

    id_column <- paste0("ID", sha1(Sys.time()))
    data <- object@Covariate |>
      mutate(!!id_column := row_number())
    grouping <- syms(grouping)
    imputation <- object@Imputation |>
      as.data.frame() |>
      mutate(!!id_column := row_number())

    map(filter, trans) |>
      c(.data = list(data)) |>
      do.call(what = dplyr::filter) -> data

    if (!missing(join)) {
      if (inherits(join, "data.frame")) {
        join <- list(join)
      }
      assert_that(is.list(join))
      assert_that(
        all(sapply(join, inherits, "data.frame")),
        msg = "not all objects in join are data.frames"
      )
      for (i in seq_along(join)) {
        assert_that(
          all(colnames(join[[i]]) %in% colnames(data)),
          msg = "all columns in join with be available in the dataset"
        )
        data <- data |>
          semi_join(join[[i]], by = colnames(join[[i]]))
      }
    }

    if (nrow(data) == 0) {
      return(
        new(
          "aggregatedImputed",
          Covariate = data |>
            select(!!!grouping) |>
            as.data.frame(),
          Imputation = imputation |>
            select(starts_with("Imputation")) |>
            colnames() |>
            list() |>
            c(list(character(0))) |>
            rev() |>
            matrix(
              data = NA_real_, nrow = 0, ncol = ncol(imputation) - 1,
              byrow = FALSE
            )
        )
      )
    }

    data |>
      inner_join(imputation, by = id_column) |>
      group_by(!!!grouping) |>
      summarise(
        across(
          .cols = all_of(colnames(object@Imputation)), .fns = list(fun),
          .names = "{.col}"
        ),
        .groups = "drop"
      ) -> total

    new(
      "aggregatedImputed",
      Covariate = total |>
        select(-starts_with("Imputation")) |>
        as.data.frame(),
      Imputation = total |>
        select(starts_with("Imputation")) |>
        as.matrix()
    )
  }
)
