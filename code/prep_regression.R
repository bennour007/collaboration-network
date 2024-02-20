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
      City == "Martonvásár" ~ "HU212", # Assuming it falls under Fejér's NUTS3 code for example
      City == "Szeged" ~ "HU333",
      City == "Tihany" ~ "HU222", # Assuming it falls under Veszprém's NUTS3 code as an example
      TRUE ~ "Unknown" # Fallback option
    )
  )






