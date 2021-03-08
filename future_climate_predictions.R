library(tidyverse)
library(raster)
library(dismo)  
source('nz_plot_func.R')
source('theme_javier.R')


# Use Maxext model to predict ----
sdms_current_model <- readRDS('outputs/sdm_current_model.RDS')

sdm_future_pred <-
  sdms_current_model %>%
  mutate(
    nz_pred_mr26_2050 = map(maxent_models, ~ predict(.x, nz_mr26_2050)),
    nz_pred_mr85_2050 = map(maxent_models, ~ predict(.x, nz_mr85_2050)),
    nz_pred_mr26_2070 = map(maxent_models, ~ predict(.x, nz_mr26_2070)),
    nz_pred_mr85_2070 = map(maxent_models, ~ predict(.x, nz_mr85_2070)),
    nz_pred_df_mr26_2050 = map(nz_pred_mr26_2050, fortify),
    nz_pred_df_mr85_2050 = map(nz_pred_mr85_2050, fortify),
    nz_pred_df_mr26_2070 = map(nz_pred_mr26_2070, fortify),
    nz_pred_df_mr85_2070 = map(nz_pred_mr85_2070, fortify),
    nz_pred_he26_2050 = map(maxent_models, ~ predict(.x, nz_he26_2050)),
    nz_pred_he85_2050 = map(maxent_models, ~ predict(.x, nz_he85_2050)),
    nz_pred_he26_2070 = map(maxent_models, ~ predict(.x, nz_he26_2070)),
    nz_pred_he85_2070 = map(maxent_models, ~ predict(.x, nz_he85_2070)),
    nz_pred_df_he26_2050 = map(nz_pred_he26_2050, fortify),
    nz_pred_df_he85_2050 = map(nz_pred_he85_2050, fortify),
    nz_pred_df_he26_2070 = map(nz_pred_he26_2070, fortify),
    nz_pred_df_he85_2070 = map(nz_pred_he85_2070, fortify),
    nz_pred_cc26_2050 = map(maxent_models, ~ predict(.x, nz_cc26_2050)),
    nz_pred_cc85_2050 = map(maxent_models, ~ predict(.x, nz_cc85_2050)),
    nz_pred_cc26_2070 = map(maxent_models, ~ predict(.x, nz_cc26_2070)),
    nz_pred_cc85_2070 = map(maxent_models, ~ predict(.x, nz_cc85_2070)),
    nz_pred_df_cc26_2050 = map(nz_pred_cc26_2050, fortify),
    nz_pred_df_cc85_2050 = map(nz_pred_cc85_2050, fortify),
    nz_pred_df_cc26_2070 = map(nz_pred_cc26_2070, fortify),
    nz_pred_df_cc85_2070 = map(nz_pred_cc85_2070, fortify)
  )

saveRDS(sdm_future_pred, 'outputs/sdm_future_pred.RDS')