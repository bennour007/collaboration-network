

################################################################################
##############RELATEDNESS MEASURED BY YEAR WITH COSINE SIMILARITY###############
################################################################################

# Load the libraries
pacman::p_load(dplyr, combinat, igraph, ggraph, Matrix, text2vec)


wos_sc_n <- wos_sc %>% 
  group_by(rowid, publication_year) %>% 
  mutate(n_subjects = n())


subjects_pairs <- wos_sc_n %>% 
  mutate(sc_pairs = list(subject_category)) %>% 
  unnest(sc_pairs) %>% 
  ungroup() %>%
  # filter(if_any(n_subjects,  function(x) x == 1))
  filter(subject_category != sc_pairs )
  
  



# Step 3: Count co-occurrences of each subject category pair
co_occurrences <- subjects_pairs %>%
  group_by(publication_year, subject_category, sc_pairs) %>%
  summarise(co_occurrence_count = n(), .groups = 'drop')


cooc_list <- co_occurrences %>%
  pull(publication_year) %>% 
  unique() %>% 
  map(., function(x) filter(co_occurrences, publication_year == {{x}} ))



# Function to create a sparse matrix from co-occurrence data
createSparseMatrix <- function(df) {
  # Ensure the subject_category and sc_pairs are factors
  df$subject_category <- factor(df$subject_category)
  df$sc_pairs <- factor(df$sc_pairs)
  
  # Create the sparse matrix
  sparse_matrix <- sparseMatrix(
    i = as.integer(df$subject_category),
    j = as.integer(df$sc_pairs),
    x = df$co_occurrence_count,
    dims = c(nlevels(df$subject_category), nlevels(df$sc_pairs))
  )
  diag(sparse_matrix) <- 0
  
  rownames(sparse_matrix) <- levels(df$subject_category)
  colnames(sparse_matrix) <- levels(df$sc_pairs)
  
  return(sparse_matrix)
}


tidy_relatedness <- cooc_list %>% 
  map(createSparseMatrix) %>% 
  map(function(x){
    x <- sim2(x, method = "cosine", norm = "l2") 
    diag(x) <- 0
    x <- x %>% as.matrix() %>% 
      as_tibble() %>%  
      mutate(sc_1 = colnames(x))%>% 
      select(sc_1, everything()) %>% 
      pivot_longer(2:last_col(), names_to = 'sc_2', values_to = 'relatedness') 
      # mutate(relatedness = if_else(relatedness == 0, NA, relatedness)) NOT SURE ABOUT THIS
  })

tidy_relatedness[[2]] %>% 
  write_csv('tmp.csv')

graph <- graph_from_data_frame(d = tidy_relatedness[[1]], directed = FALSE, vertices = NULL)

# Add edge attribute for relatedness
E(graph)$weight <- tidy_relatedness[[1]]$relatedness

# Step 4: Visualize the graph
# Basic plot
plot(graph, edge.width = E(graph)$weight)

# For a more sophisticated visualization, you can use various igraph plotting parameters:
plot(
  graph,
  edge.width = E(graph)$weight,  # Use relatedness as edge width
  vertex.size = 2,  # Adjust vertex size
  vertex.label.cex = 0.8,  # Adjust vertex label size
  layout = layout_nicely(graph)  # Choose a layout
)
