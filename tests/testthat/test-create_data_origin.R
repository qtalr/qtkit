
create_data_origin <- function(n = 100, p = 2) {
  x <- matrix(rnorm(n * p), ncol = p)
  colnames(x) <- paste0("X", seq_len(ncol(x)))
  y <- rnorm(n)
  data.frame(y, x)
}

test_that("test create_data_origin function", {
  # check if create_data_origin returns a data frame
  expect_s3_class(create_data_origin(), "data.frame")

  # check if number of rows equals to n
  expect_equal(nrow(create_data_origin()), 100)
  expect_equal(nrow(create_data_origin(50)), 50)

  # check if number of columns equals to p + 1
  expect_equal(ncol(create_data_origin()), 3)
  expect_equal(ncol(create_data_origin(100, 5)), 6)

  # check if the first column is named y
  expect_equal(colnames(create_data_origin())[1], "y")

  # check if the remaining column names are correct
  expect_equal(colnames(create_data_origin(100, 5))[-1], paste0("X", 1:5))

  # check if data is generated using normal distribution
  expect_equal(mean(create_data_origin(1000000, 1)$y) < 0.05, TRUE)
})
