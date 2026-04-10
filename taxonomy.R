library(here)
library(readr)
library(dplyr)

taxonomy <- read_csv(here::here("data", "ebird_taxonomy_v2022.csv")) %>% 
  select(taxon_order = TAXON_ORDER,
         scientific_name = SCI_NAME,
         species = PRIMARY_COM_NAME) |> 
  mutate(species = str_replace(species, "Black-crowned Night-Heron", "Black-crowned Night Heron"))

# Change Black-crowned Night-Heron to Night Heron to agree with name from
# Hawkears

#taxonomy1 <- taxonomy |> 
  
# Merge the taxonomy into the summary
hawk_summary_with_taxonomy <- left_join(hawk_summary, taxonomy, by = "species") |> 
  arrange(taxon_order)

rm(taxonomy)
