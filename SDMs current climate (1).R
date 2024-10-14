library(tidyverse)
library(sdmpredictors)
library(rgbif)
library(raster)
library(dismo)  
library(ggpubr)
library(RColorBrewer)
library(RStoolbox)
library(robis)
library(obistools)
library(ENMeval)
source('scripts/nz_plot_func.R')
source('scripts/theme_javier.R')

# read occurrence records----
top20_occ <- readRDS('data/occurence_records/occ_data_5000.RDS')


# get list of layers-----------
climate_layers <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5", # Maximum temperature
      "WC_bio6", # Minimum temperature
      "WC_bio12" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )

# saveRDS(climate_layers, 'outputs/climate_current.RDS')
climate_layers <- readRDS('outputs/climate_current.RDS')

# layers_correlation(c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12"))

# define model and NZ extent
nz_extent <-
  extent(
    165,
    179,
    -48,
    -34.00
  )
nz_env <- crop(climate_layers, nz_extent)

# Fit MaxEnt models without CV----
sdms_current <-
  top20_occ %>%
  mutate(
    model_extent = map(occ_data, ~ extent(
      min(.x$decimalLongitude, na.rm = T) - 10,
      max(.x$decimalLongitude, na.rm = T) + 10,
      min(.x$decimalLatitude, na.rm = T) - 10,
      max(.x$decimalLatitude, na.rm = T) + 10
    )),
    croped_layers = map(model_extent, ~ crop(climate_layers, .x)),
    occu_df = map(
      occ_data,
      ~ cbind.data.frame(.x$decimalLongitude, .x$decimalLatitude)
    ),
    maxent_models = map2(
      .x = occu_df,
      .y = croped_layers ,
      ~ maxent(
        x = .y,
        p = .x ,
        removeDuplicates = T
      )
    ),
    relimp_plots = map(maxent_models, plot),
    response_plots = map(maxent_models, ~ response(.x)),
    nz_pred = map(maxent_models, ~ predict(.x, nz_env)),
    nz_pred_df = map(nz_pred, fortify),
    nz_pred_map = map2(
      .x = Taxa,
      .y = nz_pred_df,
      ~ nz_plot_func(data = .y) + ggtitle(.x) + theme_void() + theme(title = element_text(face = 'italic'))
    ),
    res = map(
      .x = maxent_models,
      ~ .x@results %>% as_tibble(rownames = "variable")
    ),
    var_imp = map(maxent_models, var.importance)
  )

saveRDS(sdms_current, 'outputs/sdm_current_res.RDS')
# sdms_current <- readRDS('outputs/sdm_current_res.RDS')

# save model only------
sdms_current_model <- 
  sdms_current %>% 
  dplyr::select(Taxa, maxent_models)

saveRDS(sdms_current_model, 'outputs/sdm_current_model.RDS')