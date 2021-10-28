library(tidyverse)
library(ggpubr)
library(leaflet)
source('C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/data_analyses/scripts/theme_javier.R', echo=TRUE)

# read models and results ---
sdms_current_marine <- readRDS('outputs/SMDs/sdm_current_res_marine.RDS')
source('C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/data_analyses/scripts/nz_plot_func.R', echo=TRUE)

# NZ prediction plots -------------
figure_s4 <-
  ggarrange(
    plotlist = sdms_current_marine$nz_pred_map,
    nrow = 4,
    ncol = 5,
    common.legend = T, 
    legend = 'right', 
    labels = scales::percent(prop_suit$prop/100,accuracy = NULL),
    label.x = 0.25, 
    label.y = 0.7,
    font.label = list(face = 'plain', size = 12)
  )
figure_s4

# Save prediction maps------
ggsave(figure_s4,
       filename = 'figures/figure_s4.tiff',
       device = 'tiff',
       width = 12.5,
       height = 10)

sdms_current_marine$nz_pred_map[[1]]
