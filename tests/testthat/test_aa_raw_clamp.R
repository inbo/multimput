test_that("raw_clamp() returns the imputations unchanged without `minimum`", {
  y <- matrix(1:6, ncol = 2)
  data <- data.frame(Count = c(NA, NA, 1, 2), Bottom = c(4, 0, 1, 1))
  missing_obs <- which(is.na(data$Count))

  # `dots` without a `minimum` element must leave `y` untouched
  expect_identical(
    raw_clamp(y = y, data = data, dots = list(), missing_obs = missing_obs),
    y
  )
  expect_identical(
    raw_clamp(
      y = y,
      data = data,
      dots = list(other = "Bottom"),
      missing_obs = missing_obs
    ),
    y
  )

  # an empty `minimum` is the documented way to switch off clamping
  expect_identical(
    raw_clamp(
      y = y,
      data = data,
      dots = list(minimum = ""),
      missing_obs = missing_obs
    ),
    y
  )
})

test_that("raw_clamp() clamps the imputations to the minimum", {
  y <- matrix(c(1, 2, 3, 4), ncol = 2)
  data <- data.frame(Count = c(NA, NA, 10), Bottom = c(3, 0, 100))
  missing_obs <- which(is.na(data$Count))

  # only the rows with missing observations are relevant
  # row 1 has a minimum of 3, row 2 a minimum of 0
  expect_identical(
    raw_clamp(
      y = y,
      data = data,
      dots = list(minimum = "Bottom"),
      missing_obs = missing_obs
    ),
    matrix(c(3, 2, 3, 4), ncol = 2)
  )

  # `NA` in the minimum must not propagate into the imputations
  data$Bottom[1] <- NA
  expect_identical(
    raw_clamp(
      y = y,
      data = data,
      dots = list(minimum = "Bottom"),
      missing_obs = missing_obs
    ),
    y
  )
})

test_that("raw_clamp() checks the sanity of `minimum`", {
  y <- matrix(1:4, ncol = 2)
  data <- data.frame(Count = c(NA, 1), Bottom = c(1, 1))
  missing_obs <- which(is.na(data$Count))

  expect_error(
    raw_clamp(y, data, list(minimum = 1), missing_obs),
    "`minimum` is not a character"
  )
  expect_error(
    raw_clamp(y, data, list(minimum = c("Bottom", "Bottom")), missing_obs),
    "`minimum` must has a length of 1"
  )
  expect_error(
    raw_clamp(y, data, list(minimum = NA_character_), missing_obs),
    "`minimum` must no be NA"
  )
  expect_error(
    raw_clamp(y, data, list(minimum = "Junk"), missing_obs),
    "`minimum` must contain a column in `data`"
  )
})
