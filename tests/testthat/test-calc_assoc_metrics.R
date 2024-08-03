library(testthat)

test_that("calc_assoc_metrics works correctly", {
  data <- data.frame(
    doc_index = c(1, 1, 1, 2),
    token_index = c(1, 2, 3, 1),
    type = c("word1", "word2", "word3", "word2")
  )
  
  # Test 1: Check if the function returns a data frame
  result <- calc_assoc_metrics(data, doc_index, token_index, type)
  expect_s3_class(result, "data.frame")
  
  # Test 2: Check if the function returns the correct number of rows
  expect_equal(nrow(result), 6)
  
  # Test 3: Check if the function returns the correct column names
  expected_colnames <- c("y", "x", "pmi", "dice_coeff", "g_score")
  expect_equal(colnames(result), expected_colnames)
  
  # Test 4: Check if the function handles invalid input correctly
  expect_error(calc_assoc_metrics(data, invalid_column, token_index, type))
  
  # Test 5: Check if the function handles invalid association argument correctly
  expect_error(calc_assoc_metrics(data, doc_index, token_index, type, association = "invalid_association"))

  # TODO: complete 
  # Test 6: Check if the function returns correct measures 
})

