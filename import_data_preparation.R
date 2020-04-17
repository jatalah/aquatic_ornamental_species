library(tidyverse)
library(taxize)

import_raw <-
  read_csv(
    'data/Summary Ornamental Fish Imports 2015 - 19_original records.csv',
    col_types = cols(`No Scientific Name` = col_character())
  ) %>% 
  mutate(taxa = str_to_sentence(tolower(`Scientific Name`)),
         taxa = str_replace(taxa, 'species', replacement = "sp"),
         taxa = str_replace(taxa, 'spec', replacement = "sp"),
         taxa_length = str_count(taxa, '\\w+'), # count number of words
         taxa = if_else(taxa_length==1, paste(taxa," sp"), taxa), # add sp to single word taxa
         sp = str_detect(taxa, '\\bsp\\b|\\bspp\\b'), # detect sp or spp
         var = str_detect(taxa, '\\bvar\\b'), # detect var
         cf = str_detect(taxa, '\\bcf\\b'), # detect cf 
         sp_3_words = if_else(sp == "TRUE"  & taxa_length>2, "Y","N"), # detect >3 word and sp
         taxa = if_else(sp_3_words=="Y", str_remove(taxa, '\\bsp\\b|\\bspp\\b'), taxa),
         taxa = str_replace(taxa, '\\bvar\\b',replacement = ""), # remove 'var'
         taxa = str_replace(taxa, '\\bvariety\\b',replacement = ""), # remove "variety'
         taxa = str_replace(taxa, '\\baff.\\b',replacement = ""), # remove aff.
         taxa = str_replace(taxa, '\\b .aff. \\b',replacement = " "), # remove .aff.
         taxa = str_replace(taxa, '\\bcf\\b',replacement = ""), # remove cf
         taxa = str_replace(taxa,  '\\bspp\\b',replacement = "sp"), # replace spp for sp
         taxa = str_replace(taxa, "Colisa", "Trichogaster"),#
         taxa = str_trim(taxa),
         taxa = str_squish(taxa),
         year = as.numeric(str_sub(EXCEL,2,5))) %>%  
  filter(
    taxa != "Not known" & # remove others genus levels notations and unknowns
      taxa != "Hybrid cichlid" &
      taxa != "Scleractinia (pcs)" &
      taxa != "Clavularia anthelia" &
      taxa != "Missing any listing of fish" &
      taxa != "Total sp"
  ) %>% 
  mutate(taxa = as.character(taxa)) 

nc <- read_csv('data/cleaned_data/nc.csv')

import_raw_nc <-
  left_join(import_raw, nc, by = 'taxa') %>%
  mutate(taxa = if_else(is.na(new), taxa, new)) %>%
  select(-new,-`No Scientific Name`) %>%
  write_csv('data/cleaned_data/import_data_raw.csv')

# get unique names in dataset ------------
unique_names_freq <-
  import_raw_nc %>%
  select(taxa,  `Scientific Name`) %>%
  group_by(taxa) %>%
  filter (!duplicated(taxa)) %>%
  ungroup() %>%
  mutate(taxa = as.character(taxa)) %>% 
  write_csv('data/cleaned_data/distinct_taxa.csv')

# match taxa with gbif database ----------------
gbif_import_raw <- get_gbifid_(unique_names_freq$taxa, rows = 1)

gbif_import_raw_df <-
  bind_rows(gbif_import_raw, .id = "taxa") %>% 
  write_excel_csv('data/cleaned_data/gbif_import_raw_df.csv')
  
genus_rank <- 
  gbif_import_raw_df %>% 
  filter(rank == 'genus') %>% 
  filter(! str_detect(taxa, '\\bsp\\b')) %>% 
  filter(str_detect(taxa, " "))
  
# Check taxa classsified outside Animalia---
non_anim_taxa <- 
  gbif_import_raw_df %>% 
  filter(kingdom != "Animalia") 

non_an_gbif <- get_gbifid_(non_anim_taxa$taxa) %>%  bind_rows(.id = "taxa")
valid <- (c("5963147" ,"8335743","2395890","2262479", "8896033", "2370187", "2259724"))
non_an_changed <- non_an_gbif %>% filter(usagekey %in%valid )

# check for arthropods outside decapoda----
non_arthropoda <- 
  gbif_import_raw_df %>%
  filter(phylum == "Arthropoda" & order != "Decapoda")

insect_gbif <- get_gbifid_(non_arthropoda$taxa) %>%  bind_rows(.id = "taxa") %>% filter(kingdom =="Animalia")
valid_insect <- (c("7727995" ,"2263819"))
insect_changed <- insect_gbif %>% filter(usagekey %in%valid_insect )


gbif_import_clean <- 
  gbif_import_raw_df %>% 
  filter(!(phylum == "Arthropoda" & order != "Decapoda")) %>% 
  filter(kingdom == "Animalia") %>% 
  bind_rows(insect_changed) %>% 
  bind_rows(non_an_changed) %>% 
  mutate(
    taxa_new = if_else(is.na(species), paste0(genus, " sp."), species),
    taxa_new = if_else(is.na(genus), family, taxa_new),
    taxa_new = if_else(is.na(family), order, taxa_new)
  ) %>% 
  write_excel_csv('data/cleaned_data/gbif_import_clean.csv')

# explore species taxonomy matching ---
gbif_import_clean %>% 
  group_by(status) %>% 
  summarise(n = n())

gbif_import_clean %>% 
  group_by(rank) %>% 
  summarise(n = n())

gbif_import_raw_df %>% 
  group_by(matchtype) %>% 
  summarise(n = n())

gbif_import_clean %>% 
  filter(matchtype == "FUZZY") %>% 
  select(canonicalname, genus, species) %>% 
  print() %>% 
  write_excel_csv('data/cleaned_data/fuzzy.csv')

gbif_import_clean %>% 
  filter(status == "SYNONYM") %>% 
  select( canonicalname, genus, species) %>% 
  print() %>% 
  write_excel_csv('data/cleaned_data/synonyms.csv')

gbif_import_clean %>% 
  filter(status == "DOUBTFUL") %>% 
  select(canonicalname, genus, species)

# resolve synomnyms------
one_gbif <-
  gbif_import_clean %>%
  group_by(taxa_new) %>%
  mutate(n = n()) %>%
  ungroup() %>% 
  filter(n==1)


syno_gbif <- 
  gbif_import_clean %>% 
  group_by(taxa_new) %>%
  mutate(n = n()) %>%
  filter(n>1 & status == "ACCEPTED") %>% 
  slice(1) %>% 
  ungroup()



others_gbif <-
  gbif_import_clean %>%
  filter(
    taxa_new %in% c(
      "Sturisomatichthys panamense",
      "Oliotius oligolepis",
      "Ambastaia sidthimunki"
    )
  ) %>%
  group_by(taxa_new) %>% 
  slice(1)

gbif_all <- bind_rows(one_gbif, syno_gbif, others_gbif)

# merge with import data------
import_all <- 
  import_raw_nc %>% 
  select(-c(PDF, EXCEL,Notes, taxa_length:sp_3_words)) %>% 
  left_join(., gbif_import_clean, by = 'taxa') %>% 
  mutate(
    taxa_new = if_else(is.na(species), paste0(genus, " sp."), species),
    taxa_new = if_else(is.na(genus), family, taxa_new),
    taxa_new = if_else(is.na(family), order, taxa_new)
  ) %>% 
  write_excel_csv('data/cleaned_data/import_taxonomy.csv')


ornamental <- 
  import_all %>% 
  group_by(taxa_new, year) %>%
  summarise(Quantity = sum(Quantity, na.rm = T), freq = n()) %>%
  ungroup() %>%
  left_join(gbif_all, by = "taxa_new") %>%
  dplyr::select(taxa_new,
                year,
                Quantity,
                freq,
                # scientificname, # this is the scientific name of the submitted name
                kingdom,
                phylum,
                class,
                order,
                family,
                genus,
                species) %>% 
  rename(taxa = taxa_new,
         Frequency = freq) %>% 
  rename_all(.funs = Hmisc::capitalize) %>%
  mutate(Quantity  = if_else(Quantity < 1, 1, Quantity)) %>% # Pseudochromis fridmani had quantity 0 and a note in the original spreadsheet: "has 0 on the pdf. Is this right". 
  write_excel_csv('data/cleaned_data/ornamental_data.csv') 


# Check for missing values-------
ornamental %>%
  summarise_all(~sum(is.na(.)))


distinct(ornamental, Taxa) %>% nrow() #865 distinct taxa
distinct(ornamental, Species) %>% nrow() 
distinct(ornamental, Genus) %>% nrow() 

# number of new_taxa
ornamental %>%
  filter(str_detect(Taxa, '\\bsp\\b|\\bspp\\b')) %>% 
  distinct(Taxa) 


# number of species level---
ornamental %>%
  drop_na(Species) %>% 
  distinct(Species)


ornamental %>% 
    summarise_at(vars(Kingdom:Species), n_distinct)

ornamental %>% 
  filter(is.na(Genus)) # 3 taxa at order level

ornamental %>% 
  filter(is.na(Family))

# Get downstream data for taxa recorded at genus level-----
genus_key <- 
  gbif_all %>% 
  filter(is.na(species)) %>% 
  drop_na(genus) %>% 
  distinct(genuskey)

genus_downstream <- gbif_downstream(key = genus_key$genuskey, "species")