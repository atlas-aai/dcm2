test_that("test logit", {
  output <- logit(.5)
  expect_equal(output, 0)

  output <- logit(.25)
  expect_equal(output, -1.098612, tolerance = .001)

  output <- logit(.75)
  expect_equal(output, 1.098612, tolerance = .001)
})

test_that("test inv_logit", {
  output <- inv_logit(0)
  expect_equal(output, .5)

  output <- inv_logit(-1)
  expect_equal(output, 0.2689414, tolerance = .001)

  output <- inv_logit(1)
  expect_equal(output, 0.7310586, tolerance = .001)
})

test_that("test check_logit_x", {
  output <- dcm2:::check_logit_x(.5)
  expect_equal(output, .5)

  err <- rlang::catch_cnd(dcm2:::check_logit_x("a"))
  expect_match(err$message, "must be a numeric vector")

  err <- rlang::catch_cnd(dcm2:::check_logit_x(NA))
  expect_match(err$message, "must be a numeric vector")

  err <- rlang::catch_cnd(dcm2:::check_logit_x(1.5))
  expect_match(err$message, "must be between 0 and 1 and non-missing")

  err <- rlang::catch_cnd(dcm2:::check_logit_x(-2))
  expect_match(err$message, "must be between 0 and 1 and non-missing")
})

test_that("test check_invlogit_x", {
  output <- dcm2:::check_invlogit_x(.5)
  expect_equal(output, .5)

  err <- rlang::catch_cnd(dcm2:::check_invlogit_x("a"))
  expect_match(err$message, "must be a numeric vector")

  err <- rlang::catch_cnd(dcm2:::check_invlogit_x(NA))
  expect_match(err$message, "must be a numeric vector")

  err <- rlang::catch_cnd(dcm2:::check_invlogit_x(x = c(NaN)))
  expect_match(err$message, "must not be missing")
})
