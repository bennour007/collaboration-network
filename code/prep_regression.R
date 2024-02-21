pacman::p_load(tidyverse)

all_merged_regions <- all_merged %>% 
  mutate(
    City = case_when(
      Region %in% c("Eotvos Lorand University", "Eotvos Lorand Research Network",
                        "Corvinus University Budapest", "Obuda University", "Semmelweis University",
                        "University of Veterinary Medicine Budapest", "Pazmany Peter Catholic University",
                        "Budapest Metropolitan University", "Budapest University of Technology & Economics",
                        "Budapest Business University", "Moholy-Nagy University of Art & Design Budapest",
                        "HUN-REN Veterinary Medical Research Institute", "HUN-REN Research Centre for Natural Sciences",
                        "HUN-REN Wigner Research Centre for Physics", "HUN-REN Centre for Social Sciences",
                        "HUN-REN Research Centre for Astronomy & Earth Sciences", "HUN-REN Centre for Ecological Research",
                        "HUN-REN Centre for Energy Research", "HUN-REN Institute of Experimental Medicine",
                        "HUN-REN Centre for Economic & Regional Studies", "HUN-REN Institute for Computer Science & Control",
                        "HUN-REN Alfred Renyi Institute of Mathematics", "HUN-REN Research Centre for the Humanities",
                        "HUN-REN Research Centre for Linguistics") ~ "Budapest",
      Region == "University of Istvan Szechenyi" ~ "Gyor",
      Region %in% c("University of Debrecen", "HUN-REN Institute for Nuclear Research") ~ "Debrecen",
      Region == "University of Pecs" ~ "Pecs",
      Region == "University of Miskolc" ~ "Miskolc",
      Region == "University of Pannonia" ~ "Veszprem",
      Region == "University of Nyiregyhaza" ~ "Nyiregyhaza",
      Region == "HUN-REN Institute of Earth Physics & Space Science" ~ "Sopron",
      Region == "HUN-REN Centre for Agricultural Research" ~ "Martonvasar",
      Region == "HUN-REN Biological Research Center" ~ "Szeged",
      Region == "HUN-REN Balaton Limnological Research Institute" ~ "Tihany",
      TRUE ~ "Unknown" # Default case if none of the above matches
    ),
    NUTS3 = case_when(
      City == "Budapest" ~ "HU110",
      City == "Gyor" ~ "HU221",
      City == "Debrecen" ~ "HU321",
      City == "Pecs" ~ "HU231",
      City == "Miskolc" ~ "HU312",
      City == "Veszprem" ~ "HU222",
      City == "Nyiregyhaza" ~ "HU323",
      City == "Sopron" ~ "HU221",
      City == "Martonvasar" ~ "HU212", # Assuming it falls under Fejér's NUTS3 code for example
      City == "Szeged" ~ "HU333",
      City == "Tihany" ~ "HU222", # Assuming it falls under Veszprém's NUTS3 code as an example
      TRUE ~ "Unknown" # Fallback option
    )
  )


#### read eurostat vars and join them with the data 



# Set the directory containing the CSV files
directory_path <- "data/eurostat"

# List all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
# Read all CSV files and store them in a list
eurostat_data_list <- map(csv_files, function(x){ read_csv(x) %>% select(- freq) %>% janitor::clean_names()}) 

eurostat_data_long <- eurostat_data_list %>% 
  map(
    function(x){
      x %>% 
        mutate(
          across(
            x2013:x2022,
            as.numeric
          )
        ) %>% 
        pivot_longer(x2013:x2022, names_to = 'year', values_to = 'es_value') 
    }
  ) 

eurostat_data_long[[1]] <- eurostat_data_long[[1]]%>% 
  mutate(indic_ur = case_when(
    indic_ur == "TE1026V" ~ "num_students",
    indic_ur == "TE1026I" ~ "share_students",
    indic_ur == "DE1001V" ~ "population",
    TRUE ~ indic_ur # Keeps the original value if none of the conditions above are met
  ))

eurostat_data_long[[3]] <- eurostat_data_long[[3]] %>% 
  mutate(indic_ur = case_when(
    indic_ur == "DE1001V" ~ "population",
    TRUE ~ indic_ur # Keeps the original value if none of the conditions above are met
  ))


all_merged_regions %>% 
  left_join(eurostat_data_long[[1]], by = c())



all_merged_regions %>% 
  ggplot() + 
  geom_histogram(
    aes(x = log(KCI))
  )
