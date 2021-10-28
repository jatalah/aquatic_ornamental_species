library(tidyverse)
library(ggpubr)
library(robis)
library(obistools)

top20_occ <- readRDS('data/occurence_records/occ_data_5000.RDS')

top20_occ$occ_data[[11]] <-
  bind_rows(
    top20_occ$occ_data[[11]],
    c(
      decimalLatitude = 33.49782,
      decimalLongitude = -82.12797
    )
  )

# plot occurrence maps---
top20_occ_maps <- 
  top20_occ %>% 
  mutate(
    maps = map2(
      .x = Taxa,
      .y = occ_data,
      ~ plot_map(.y, zoom = F) + ggtitle(.x) + theme_void() + theme(title = element_text(face = 'italic'))
    )
  )

# Plot and save the global occurrence maps ----------
figure_S2 <-
  ggarrange(plotlist = top20_occ_maps$maps,
            nrow = 4,
            ncol = 5)
figure_S2
ggsave(
  figure_S2,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_S2.png',
  width = 12,
  height = 6
)

ggsave(
  figure_S2,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_S2.svg',
  width = 12,
  height = 8
)

ggsave(
  figure_S2,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_S2.tiff',
  dpi = 900,
  width = 12,
  compression = 'lzw',
  height = 8
)

