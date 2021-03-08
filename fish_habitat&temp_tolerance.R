library(rfishbase)
library(tidyverse)

# read data -----------
ornamental <- 
  read_csv('data/cleaned_data/ornamental_data.csv')


# top 20 taxa---------
top20 <- read_csv('data/cleaned_data/top_20_taxa.csv')

top20common_names <- sci2comm(sci= top20$Taxa)

top20common_names_df <- 
top20common_names %>%
  flatten_df() %>%
  t() %>%
  as_tibble(rownames = 'Taxa') %>%
  rename(common_name = V1) %>% 
  mutate(common_name = str_to_sentence(common_name))

ornamental %>% distinct(Taxa)

fish <- ornamental %>% filter(Phylum =="Chordata") 

fish_genus <- 
  fish %>% 
  filter(is.na(Species)) %>% 
  distinct(Genus) %>% 
  write_csv('data/cleaned_data/fish_genus.csv')

non_fish <-  
  ornamental %>% filter(Phylum !="Chordata") %>% 
  distinct(Taxa, Phylum, Family) %>% 
  mutate(Habitat = if_else(Phylum == "Cnidaria" | Phylum =="Echinodermata" | Phylum =="Arthropoda" | Phylum =="Polychaeta", "Marine", "NA")) %>% 
  write_excel_csv('data/cleaned_data/non_fish_taxa.csv')

non_fish %>% 
  count(Phylum)

# Find FB accepted names----
fish_unique <- 
  fish %>% 
  distinct(Species) %>% 
  drop_na()

fish_names <- synonyms(fish_unique$Species)

fb_spp_names <- 
  fish_names %>% 
  filter(Status == "synonym") %>% 
  select(synonym, Species) %>% 
  rename(Species = synonym, FB_name = Species) %>% 
  left_join(fish_unique, .) %>% 
  mutate(FB_name = if_else(is.na(FB_name), Species, FB_name ))

fb_species_data <- species(species_list = fb_spp_names$FB_name)

# get habitat and common names---------
fb_species <-
  species(
    fb_spp_names$FB_name,
    fields = c(
      "Species",
      "SpecCode",
      "FBname" ,
      "Fresh",
      "Brack",
      "Saltwater",
      "DemersPelag" ,
      "Vulnerability",
      "Vulnerability",
      "Aquarium",
      "AquariumFishII",
      "Comments"
    )
  ) %>% 
  rename(FB_name = "Species") %>% 
  full_join(fb_spp_names, by = "FB_name") %>%
  mutate(habitat = if_else(Fresh==-1,"Freshwater", "Marine"),
         habitat = if_else(Saltwater ==-1, "Marine", habitat),
         habitat = if_else(Fresh == "0" & Brack == "-1" &  Saltwater =="0", "Brackish", habitat),
         habitat = if_else(Species =="Dermogenys pusilla" | Species =="Poecilia latipinna", "Freshwater", habitat),
         habitat = if_else(Fresh == "-1" & Brack == "-1" &  Saltwater =="-1", "Anodromus", habitat)) %>%
  write_csv('data/cleaned_data/fb_species_data.csv')

fb_species %>% 
  count(habitat)


# Anodromus fish 
fb_species %>% 
  filter(Fresh == "-1" & Brack == "-1" &  Saltwater =="-1") %>% 
  select(FB_name)

# Dermogenys pusilla = Fresh 
# Monodactylus argenteus  = Marine
# Poecilia latipinna  = Freshwater   
# Scatophagus argus = marine  
# Selenotoca multifasciata = marine

fb_species %>% 
  filter(Fresh == "0" & Brack == "-1" &  Saltwater =="0") %>% 
  select(FB_name)

# Join data ---
genus_hab <- read_csv('data/cleaned_data/fish_genus_habitat.csv')
ornamental_hab <- 
  left_join(ornamental, genus_hab, by = 'Genus') %>% 
  left_join(fb_species, by = "Species") %>% 
  mutate(habitat = ifelse(is.na(habitat), Habitat, habitat),
         habitat = if_else(Phylum != 'Chordata', "Marine", habitat)) %>% 
  select(-FB_name, - Habitat) %>% 
  write_csv('data/cleaned_data/ornamental_habitat_data.csv')

# Habitat summary statistics ----
ornamental_hab %>% 
  group_by(habitat) %>% 
  summarise_at(vars(Quantity, Frequency), sum) %>% 
  ungroup() %>% 
  mutate(Total_q = sum(Quantity),
         Total_f = sum(Frequency),
         perc_q = Quantity/Total_q*100,
         perc_f = Frequency/Total_f*100)

# Taxa by habitat --
ornamental_hab %>% 
  distinct(Taxa, habitat) %>% 
  count(habitat) %>% 
  mutate(percent = n/865*100)
  
# summary for fish ---
ornamental_hab %>% 
  filter(Phylum =="Chordata") %>% 
  group_by(habitat) %>% 
  summarise_at(vars(Quantity, Frequency), sum) %>% 
  ungroup() %>% 
  mutate(Total_q = sum(Quantity),
         Total_f = sum(Frequency),
         perc_q = Quantity/Total_q*100,
         perc_f = Frequency/Total_f*100)

# Taxa by habitat --
ornamental_hab %>% 
  filter(Phylum =="Chordata") %>% 
  distinct(Taxa, habitat) %>% 
  count(habitat) %>% 
  mutate(percent = n/865*100)


# Plot quantity fish by habitat----
ornamental_hab %>%
  filter(!is.na(Species)) %>%
  drop_na(habitat) %>% 
  group_by(habitat) %>%
  top_n(150, wt = Quantity) %>%
  group_by(Species, FBname, habitat) %>% 
  summarise_at(vars(Quantity, Frequency),mean) %>% 
  ungroup() %>% 
  drop_na(FBname) %>% 
  ggplot(aes(
    fill = Frequency,
    y = Quantity,
    x = fct_reorder(FBname, Quantity)
  )) +
  geom_bar(stat = "identity")  +
  geom_col() +
  coord_flip() +
  labs(y = "Number of fish",
       x = 'Species',
       title = "Fish import data (2015 - 2019)") +
  theme_minimal() +
  facet_wrap( ~ habitat, scales = 'free') +
  # scale_y_sqrt(breaks = c(10, 50, 100, 250,  500, 1e3, 2e3,3e3, 5e3)) +
  # scale_fill_brewer(palette = 'OrRd',
  #                      trans = 'log10',
  #                      name = 'Import \n frequency') +
  scale_fill_distiller(palette = 'Reds', trans ='log10',
                       name = 'Mean import \n Quantity', direction = 1)
  # scale_fill_gradientn(colours = heat.colors(10),
  #                      name = 'Import \n frequency',
  #                      trans = 'log10',
  #                      labels = scales::comma) 



fish_all %>%
  filter(!is.na(Species)) %>%
  drop_na(habitat) %>% 
  group_by(habitat) %>%
  top_n(150, wt = Quantity) %>%
  group_by(Species, FBname, EnvTemp) %>% 
  summarise_at(vars(Quantity, Frequency),mean) %>% 
  ungroup() %>% 
  drop_na(FBname) %>% 
  ggplot(aes(
    fill = Frequency,
    y = Quantity,
    x = fct_reorder(FBname, Quantity)
  )) +
  geom_bar(stat = "identity")  +
  geom_col() +
  coord_flip() +
  labs(y = "Number of fish",
       x = 'Species',
       title = "Fish import data (2015 - 2019)") +
  theme_minimal() +
  facet_wrap( ~ EnvTemp, scales = 'free') +
  # scale_y_sqrt(breaks = c(10, 50, 100, 250,  500, 1e3, 2e3,3e3, 5e3)) +
  # scale_fill_brewer(palette = 'OrRd',
  #                      trans = 'log10',
  #                      name = 'Import \n frequency') +
  scale_fill_distiller(palette = 'Reds', trans ='log10',
                       name = 'Import \n frequency', direction = 1)

# introductions-----
introduc_fb <- 
  introductions(fb_spp_names$FB_name)

introduc_fb1 <- 
  introduc_fb %>% 
  drop_na(autoctr) %>% 
  group_by(Species, Reason) %>% 
  summarise(n = n()) %>% 
  group_by(Species) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  left_join(fb_species, by = "Species") %>%  
  write_csv('data/cleaned_data/fb_fish_intro.csv')


# library(RColorBrewer) 
# colourCount = length(unique(introduc_fb1$Reason))
# getPalette = colorRampPalette(brewer.pal(4, "Spectral"))

# Plot number of introductions by reason 
introduc_fb1 %>%
  drop_na(Reason) %>% 
  filter(habitat != "Brackish") %>%
  group_by(habitat) %>% 
  top_n(100, wt = total) %>%
  ggplot(aes(fill=Reason, y = n, x = fct_reorder(FBname, total))) +
  geom_bar(position = "fill", stat = "identity")  +
  geom_col() +
  coord_flip() +
  labs(y = "Number of introduction",
       x = 'Fish species',
       title = "Fish introductions") +
  theme_minimal() +
  facet_wrap(~ habitat, scales = 'free') +
  scale_y_sqrt() +
  # scale_fill_manual(values = rev(getPalette(colourCount)))
  scale_fill_viridis_d(option = 'A')



# temperature tolerance ------
# check Symphysodon aequifasciatus is repeated ----
temp_pref <-
  stocks(
    fb_spp_names$FB_name,
    fields = c(
      "Species",
      "TempMin",
      "TempMax",
      "TempRef",
      "EnvTemp"
    )
  ) %>%
  rename(FB_name = "Species") %>% 
  full_join(fb_spp_names, by = "FB_name") %>% 
  write_csv('data/cleaned_data/fb_temp_pref.csv')

fb_temp_refs <- temp_pref %>% filter(!is.na(TempRef))
temp_pref %>% select(Species) %>% filter (duplicated(Species))
sum(is.na(temp_pref$TempMin))

table(temp_pref$EnvTemp)


# Top 20 info-----
top20_synonyms <-synonyms(top20$Taxa)

top_20_temp_pref <- 
  stocks(
    top20_synonyms$Species,
    fields = c(
      "Species",
      "TempMin",
      "TempMax",
      "TempRef",
      "EnvTemp"
    )
  ) 

x <- common_names(top20_synonyms$Species)


introduc_top20 <- 
  introductions(top20_synonyms$Species)


.# # Disease references ------
# refs_all <- 
#   references() %>% 
#   data.frame()
# 
# temp_pref %>% 
#   filter(!is.na(TempRef)) %>% 
#   rename(RefNo = "TempRef" ) %>% 
#   select(Species, RefNo) %>% 
#   left_join(refs_all, by = 'RefNo') %>% 
#   full_join(temp_pref, ., by = "Species") %>% 
#   write_csv('data/cleaned_data/fb_temp_pref_refs.csv')



temp_pref %>% 
  filter(duplicated(Species))


# Table temperature tolerance-----
fish_temp_tolerance_plot <- 
  full_join(temp_pref, fb_species, by = 'Species') %>% 
  ungroup() %>%  
  drop_na(TempMin, TempMax) %>%
  group_by(habitat) %>%
  top_n(-50, wt = TempMin) %>%
  ungroup() %>%
  mutate(TempMin = as.numeric(TempMin),
         TempMax = as.numeric(TempMax)) %>%
  ggplot(aes(x = fct_reorder(Species, TempMin, .desc = TRUE))) +
  geom_linerange(aes(ymin = TempMin , ymax = TempMax, color = TempMin), size = 3) +
  coord_flip() +
  scale_color_viridis_c(option = "A", guide = F) +
  labs(
    y = Temperature ~ range ~ degree ~ C,
    parse = T,
    x = 'Fish species',
    title = "Temperature tolerance for top 100 fish"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = 'italic')) +
  facet_wrap( ~ habitat, scales = 'free')

print(fish_temp_tolerance_plot)

ggsave(
  plot = fish_temp_tolerance_plot,
  filename = "figures/fish_temp_tolerance_plot.pdf",
  dpi = 300,
  width = 24,
  height = 24,
  units = 'cm'
)



