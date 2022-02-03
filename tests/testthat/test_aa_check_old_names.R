test_that("check_old_names", {
  expect_true(
    check_old_names(junk = "foo", old_names = c(foo = "bar"))
  )
  expect_true(
    check_old_names(foo = "bar", old_names = c(foo = "bar"))
  )
  expect_error(
    check_old_names(bar = "foo", old_names = c(foo = "bar")),
    "some arguments changed name"
  )
})
