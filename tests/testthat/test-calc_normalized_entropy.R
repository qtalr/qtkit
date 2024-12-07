# Test: calc_normalized_entropy

test_that("complete variation", {
  x <- c("A", "B", "C", "D", "E")
  expect_equal(calc_normalized_entropy(x), 1)
})

test_that("variation", {
  x <- c("A", "A", "A", "A", "E")
  expect_lt(calc_normalized_entropy(x), 1)
})
