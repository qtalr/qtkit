# Create a dataset for the `calc_type_metrics` function

set.seed(123) # for reproducibility

# Create a dataset with 100 rows and 2 columns
# The first column contains the type (A, B, C, D, E)
# The second column contains the document (doc1, doc2, doc3)
types_data <-
  data.frame(
    type = rep(c("A", "B", "C", "D", "E"), each = 20),
    document = sample(c("doc1", "doc2", "doc3"), 100, replace = TRUE)
  )

# Save the dataset as an RDS file
saveRDS(types_data, "inst/extdata/types_data.rds")
