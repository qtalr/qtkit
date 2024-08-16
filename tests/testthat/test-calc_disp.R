# Test: calc_disp

library(Matrix)

# Create a dummy term-document matrix for testing
tdm <- Matrix(c(1, 0, 2, 0, 0, 3, 1, 2, 0, 0, 1, 0), nrow = 3)

test_that("calc_rf calculates relative frequency correctly", {
  expect_equal(calc_rf(tdm), c(2, 3, 5) / 10)
})

test_that("calc_orf calculates observed relative frequency correctly", {
  expect_equal(calc_orf(tdm), c(2, 3, 5) / 10 * 100)
})

test_that("calc_df calculates document frequency correctly", {
  expect_equal(calc_df(tdm), c(2, 2, 2))
})

test_that("calc_idf calculates inverse document frequency correctly", {
  expect_equal(calc_idf(tdm), log(4 / c(2, 2, 2)))
})

test_that("calc_dp calculates Gries' Deviation of Proportions correctly", {
  dp <- calc_dp(tdm)
  expect_true(all(dp >= 0 & dp <= 1))
})
