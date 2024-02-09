

################################################################################
##############RELATEDNESS MEASURED BY YEAR WITH COSINE SIMILARITY###############
################################################################################

# Load the libraries
pacman::p_load(dplyr, combinat, igraph, ggraph, Matrix, text2vec)


wos_kw_n <- wos_kw %>% 
  group_by(rowid, publication_year) %>% 
  mutate(n_kw = n())


kw_pairs <- wos_kw_n %>% 
  mutate(
    author_keywords = janitor::make_clean_names(author_keywords),
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


tidy_relatedness <- cooc_list %>% 
  map(createSparseMatrix) %>% 
  map(function(x){
    x <- sim2(x, method = "cosine", norm = "l2") 
    diag(x) <- 0
    x <- x %>% as.matrix() %>% 
      as_tibble() %>%  
      mutate(kw_1 = colnames(x))%>% 
      select(kw_1, everything()) %>% 
      pivot_longer(2:last_col(), names_to = 'kw_2', values_to = 'relatedness') %>% 
      mutate(relatedness = if_else(relatedness == 0, NA, relatedness)) 
  })
