library(tidyverse)
library(ggpubr)
library(raster)
library(dismo)
library(RColorBrewer)
source('scripts/theme_javier.R')
source('C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/data_analyses/scripts/nz_plot_func.R')
# read models and results ---
sdms_current <- readRDS('outputs/SMDs/sdm_current_res.RDS')

sdms_current <- 
  sdms_current %>% 
  mutate(
    nz_pred_map = map2(
  .x = Taxa,
  .y = nz_pred_df,
  ~ nz_plot_func(data = .y) + ggtitle(.x) + theme_void() + theme(title = element_text(face = 'italic'))
))

# read top-20 taxa order and habitat suitability-----
top20 <- 
  read_csv('outputs/top_20_taxa.csv') %>% 
  mutate(rank = 1:20)

prop_suit <- read_csv('tables/prop_suitability_current.csv')


nz_pred_maps <-
  ggarrange(
    plotlist = sdms_current$nz_pred_map,
    nrow = 4,
    ncol = 5,
    common.legend = T, 
    legend = 'right', 
    labels = scales::percent(prop_suit$prop/100,accuracy = 0.1),
    label.x = 0.25, 
    label.y = 0.7,
    font.label = list(face = 'plain', size = 12)
  )
nz_pred_maps

# Save prediction maps------
ggsave(nz_pred_maps,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_5.png',
       device = 'png',
       width = 12.5,
       height = 10)

ggsave(nz_pred_maps,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_5.svg',
       width = 12.5,
       height = 10)

ggsave(nz_pred_maps,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_5.tiff',
       dpi = 900,
       width = 12.5,
       compression = 'lzw',
       height = 10)
