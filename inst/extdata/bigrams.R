
library(dplyr)

bigrams_data <- tibble(
  doc_index = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
  token_index = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  type = c("word1", "word2", "word3", "word2", "word3", "word4", "word3", "word4", "word5", "word4", "word5", "word6")
)

# Save the dataset as an RDS file
saveRDS(bigrams_data, "inst/extdata/bigrams_data.rds")
