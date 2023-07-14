test_that("rzip1", {
  set.seed(20230714)
  expect_true(all(rzip1(1e3, lambda = 1e3, prob = 1) == 0))
  expect_true(all(rzip1(1e3, lambda = 1e3, prob = 0) > 0))
})

test_that("rzinb1", {
  set.seed(20230714)
  expect_true(all(rzinb1(1e3, mu = 1e3, prob = 1, size = 4) == 0))
  expect_true(all(rzinb1(1e3, mu = 1e3, prob = 0, size = 4) > 0))
})

test_that("rzip0", {
  set.seed(20230714)
  expect_true(all(rzip0(1e3, lambda = 1e3, prob = 1) == 0))
  expect_true(all(rzip0(1e3, lambda = 1e3, prob = 0) > 0))
  expect_true(all(rzip0(1e3, lambda = 1e-10, prob = 0) == 1))
  expect_true(all(rzip0(2, lambda = c(1, 1e-10), prob = 0) > 0))
})


test_that("rzinb0", {
  set.seed(20230714)
  expect_true(all(rzinb0(1e3, mu = 1e3, prob = 1, size = 4) == 0))
  expect_true(all(rzinb0(1e3, mu = 1e3, prob = 0, size = 4) > 0))
  expect_true(all(rzinb0(1e3, mu = 1e-10, prob = 0, size = 4) == 1))
  expect_true(all(rzinb0(2, mu = c(1, 1e-10), prob = 0, size = c(4, 4)) > 0))
})
