library(rfishbase)
library(tidyverse)

ornamental <- read_csv('data/cleaned_data/ornamental_data.csv')

fish <- 
  ornamental %>% 
  filter(phylum =="Chordata") %>% 
  ungroup() %>% 
  rename(Species = species)


fish_unique <- 
  fish %>% 
  distinct(Species)

# temperature tolerance ------
fish_names <- synonyms (fish_unique$Species)
fish_names_spp <- species(fish_unique$Species)
fb_validate_names <- validate_names(fish_unique$Species)



# check Symphysodon aequifasciatus is repeated ----
temp_pref <-
  stocks(
    fish_unique$Species,
    fields = c(
      "Species",
      "TempMin",
      "TempMax",
      "TempRef",
      "EnvTemp"
    )
  ) %>% 
  write_csv('data/cleaned_data/fb_temp_pref.csv')

# get habitat and common names---------

fb_species <-
  species(
    fish_unique$Species,
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
  )%>% 
  # mutate(habitat = if_else(Fresh==-1,"Freshwater", if_else(Brack==-1,"Brackish", "Marine")),
  #        FBname = if_else(is.na(FBname), Species, FBname)) %>% 
  write_csv('data/cleaned_data/fb_species_data.csv')


# Join data ---
fish_all <- 
  full_join(fish, fb_species) %>% 
  full_join(temp_pref) %>% 
  mutate(habitat = if_else(Fresh==-1,"Freshwater", if_else(Brack==-1,"Brackish", "Marine")),
         FBname = if_else(is.na(FBname), Species, FBname)) %>% 
  ungroup() %>% 
  write_csv('data/cleaned_data/fb_data_all.csv')



# Plot quantity fish by habitat----
fish_all %>%
  # filter(habitat != "Brackish") %>%
  drop_na(habitat) %>% 
  group_by(habitat) %>%
  top_n(75, wt = Quantity) %>%
  group_by(Species, habitat) %>% 
  summarise_at(vars(Quantity,freq),sum) %>% 
  ungroup() %>% 
  left_join(fb_species, by = "Species") %>% 
  ggplot(aes(
    fill = freq,
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
                       name = 'Import \n frequency', direction = 1)
  # scale_fill_gradientn(colours = heat.colors(10),
  #                      name = 'Import \n frequency',
  #                      trans = 'log10',
  #                      labels = scales::comma) 


# introductions-----
introduc_fb <- 
  introductions(fish_unique$Species)

introduc_fb1 <- 
  introduc_fb %>% 
  drop_na(autoctr) %>% 
  group_by(Species, Reason) %>% 
  summarise(n = n()) %>% 
  group_by(Species) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  left_join(fb_species, by = "Species") %>%  
  mutate(habitat = if_else(Fresh==-1,"Freshwater", if_else(Brack==-1,"Brackish", "Marine")),
         FBname = if_else(is.na(FBname), Species, FBname)) %>% 
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



temp_pref %>% 
  filter(duplicated(Species))

temp_pref %>% 
  distinct(Species) 

fish_all %>%
  drop_na(TempMin, TempMax, habitat) %>% 
  group_by(habitat) %>% 
  summarise(n = n())


fish_all %>%
 filter(habitat == "Brackish")

### Plot introductions

introduc_fb1 %>% 
  filter(habitat != "Brackish") %>%
  group_by(habitat) %>% 
  top_n(75, wt = n_intro) %>%
  # ungroup() %>% 
  ggplot(aes(x = fct_reorder(FBname, n_intro), y = n_intro, fill = reason)) +
  geom_col() +
  coord_flip() +
  labs(
    y = "Number of introduction",
    x = 'Fish species',
    title = "Fish introductions"
  ) +
  theme_minimal() +
  facet_wrap( ~ habitat, scales = 'free') +
  scale_y_sqrt()


# Table temperature tolerance-----
full_join(temp_pref, fb_species, by = 'Species') %>% 
  mutate(habitat = if_else(Fresh==-1,"Freshwater", if_else(Brack==-1,"Brackish", "Marine")),
         FBname = if_else(is.na(FBname), Species, FBname)) %>% 
  ungroup() %>%  
  filter(habitat != "Brackish") %>%
  drop_na(TempMin, TempMax) %>%
  group_by(habitat) %>%
  top_n(-50, wt = TempMin) %>%
  ungroup() %>%
  mutate(TempMin = as.numeric(TempMin),
         TempMax = as.numeric(TempMax)) %>%
  ggplot(aes(x = fct_reorder(FBname, TempMin, .desc = TRUE))) +
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



