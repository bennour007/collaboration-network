

################################################################################
##############RELATEDNESS MEASURED BY YEAR WITH COSINE SIMILARITY###############
################################################################################

# Load the libraries
pacman::p_load(tidyverse, combinat, igraph, ggraph, Matrix, data.table, disk.frame)


wos_kw <- read_csv(here::here('data', 'clean_data', 'wos_kw.csv'))


wos_kw_n <- wos_kw %>% 
  group_by(rowid, publication_year) %>% 
  mutate(n_kw = n())


kw_pairs <- wos_kw_n %>% 
  mutate(
    # author_keywords = janitor::make_clean_names(author_keywords),
    kw_pairs = list(author_keywords)
  ) %>% 
  unnest(kw_pairs) %>% 
  ungroup() %>%
  # filter(if_any(n_subjects,  function(x) x == 1))
  filter(author_keywords != kw_pairs )



# Step 3: Count co-occurrences of each subject category pair
co_occurrences <- kw_pairs %>%
  group_by(publication_year, author_keywords, kw_pairs) %>%
  summarise(co_occurrence_count = n(), .groups = 'drop')


cooc_list <- co_occurrences %>%
  pull(publication_year) %>% 
  unique() %>% 
  map(., function(x) filter(co_occurrences, publication_year == {{x}} ))



# Function to create a sparse matrix from co-occurrence data
createSparseMatrix <- function(df) {
  # Ensure the author_keywords and kw_pairs are factors
  df$author_keywords <- factor(df$author_keywords)
  df$kw_pairs <- factor(df$kw_pairs)
  
  # Create the sparse matrix
  sparse_matrix <- sparseMatrix(
    i = as.integer(df$author_keywords),
    j = as.integer(df$kw_pairs),
    x = df$co_occurrence_count,
    dims = c(nlevels(df$author_keywords), nlevels(df$kw_pairs))
  )
  diag(sparse_matrix) <- 0
  
  rownames(sparse_matrix) <- levels(df$author_keywords)
  colnames(sparse_matrix) <- levels(df$kw_pairs)
  
  return(sparse_matrix)
}


calculate_similarity <- function(mat) {
  # Zeroing the diagonal of the matrix as provided
  diag(mat) <- 0
  
  # Calculating column sums
  col_sums <- colSums(mat)
  
  # Avoiding division by zero by replacing zero sums with 1 (or a very small number)
  # to prevent division by zero in the similarity calculation
  col_sums[col_sums == 0] <- 1
  
  # Creating matrices for Si and Sj using outer product to efficiently compute the denominator
  # No need to create matrices by row and column as we can directly compute the denominator
  denominator <- sqrt(outer(col_sums, col_sums, FUN = "*"))
  
  # Calculating the similarity score SC
  SC <- mat / denominator
  
  # Handling NAs if any (though there should not be any given the adjustment above)
  SC[is.na(SC)] <- 0
  
  # Ensuring diagonal elements are zero to avoid self-similarity
  diag(SC) <- 0
  
  return(SC)
}

# Example usage
# Assuming 'mat' is your matrix of interest
# mat <- matrix(rnorm(100), nrow=10) # Example matrix
# SC <- calculate_similarity(mat)


matrix_list <- cooc_list %>% 
  map(createSparseMatrix) 

matrix_list_stded <- matrix_list %>% 
  map(calculate_similarity)




library(data.table)

# Assuming 'matrix_list' is a list of matrices and 'sim2' is defined elsewhere
transformed_list <- map(matrix_list_stded, function(x) {
  
  # Convert the matrix to a data.table
  x_dt <- as.data.table(as.matrix(x))
  
  # Add a column with row names (kw_1)
  x_dt[, kw_1 := rownames(x)]
  
  # Melt the data.table to long format
  melted_dt <- melt(x_dt, id.vars = "kw_1", variable.name = "kw_2", value.name = "relatedness")
  
  # Replace relatedness values of 0 with NA
  melted_dt[relatedness == 0, relatedness := NA_real_]
  
  return(melted_dt)
})





























gc()

tidy_relatedness_1 <- matrix_list_stded %>% 
  map(function(x){
    x %>% 
      as.matrix() %>% 
      as_tibble() %>%
      mutate(kw_1 = colnames(x)) %>% 
      select(kw_1, everything()) 
  })

gc()




# Setup disk.frame to use multiple cores
setup_disk.frame()
# Optional: Increase the number of workers (adjust based on your system)
options(future.globals.maxSize = Inf)

tidy_relatedness_1 <- matrix_list_stded %>% 
  map(
    function(x){
      x %>% 
        as.matrix() %>% 
        as.tibble() %>% 
        as.disk.frame()
    }
  )



tidy_relatedness_2 <- tidy_relatedness_1 %>% 
  map(
    function(x){
      x %>%
        mutate(kw_1 = colnames(x)) %>% 
        select(kw_1, everything()) %>% 
        as
        pivot_longer(2:last_col(), names_to = 'kw_2', values_to = 'relatedness') %>% 
        mutate(relatedness = if_else(relatedness == 0, NA, relatedness)) 
    }
  )



  pivot_longer(2:last_col(), names_to = 'kw_2', values_to = 'relatedness') %>% 
  mutate(relatedness = if_else(relatedness == 0, NA, relatedness))

# matrix_list[1:2] %>% 
#   map(function(x){
#   x <- sim2(x, method = "cosine", norm = "l2") 
#   diag(x) <- 0
#   x <- x %>% as.matrix() %>% 
#     as_tibble() %>%  
#     mutate(kw_1 = colnames(x)) %>% 
#     select(kw_1, everything()) %>% 
#     pivot_longer(2:last_col(), names_to = 'kw_2', values_to = 'relatedness') %>% 
#     mutate(relatedness = if_else(relatedness == 0, NA, relatedness)) 
# })




































