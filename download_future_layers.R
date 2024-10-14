library(sdmpredictors)

# download and save climate layers-------
# 1. MIROC-ESM layers----
layers_mr26_2050 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_mr26_2050", # Maximum temperature
      "WC_bio6_mr26_2050", # Minimum temperature
      "WC_bio12_mr26_2050" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )

layers_mr85_2050 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_mr85_2050", # Maximum temperature
      "WC_bio6_mr85_2050", # Minimum temperature
      "WC_bio12_mr85_2050" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


layers_mr26_2070 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_mr26_2070", # Maximum temperature
      "WC_bio6_mr26_2070", # Minimum temperature
      "WC_bio12_mr26_2070" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


layers_mr85_2070 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_mr85_2070", # Maximum temperature
      "WC_bio6_mr85_2070", # Minimum temperature
      "WC_bio12_mr85_2070" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


# Crop the NZ extent ----
nz_extent <-
  extent(
    165,
    179,
    -48,
    -34.00
  )
nz_mr26_2050 <- crop(layers_mr26_2050, nz_extent)
nz_mr85_2050 <- crop(layers_mr85_2050, nz_extent)
nz_mr26_2070 <- crop(layers_mr26_2070, nz_extent)
nz_mr85_2070 <- crop(layers_mr85_2070, nz_extent)


names(nz_mr26_2050) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_mr85_2050) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_mr26_2070) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_mr85_2070) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")

# get layer summaries----
summary(nz_mr26_2050)
summary(nz_mr85_2050)
summary(nz_mr26_2070)
summary(nz_mr85_2070)



# 2. HadGEM2-ES layers----
layers_he26_2050 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_he26_2050", # Maximum temperature
      "WC_bio6_he26_2050", # Minimum temperature
      "WC_bio12_he26_2050" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )

layers_he85_2050 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_he85_2050", # Maximum temperature
      "WC_bio6_he85_2050", # Minimum temperature
      "WC_bio12_he85_2050" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


layers_he26_2070 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_he26_2070", # Maximum temperature
      "WC_bio6_he26_2070", # Minimum temperature
      "WC_bio12_he26_2070" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


layers_he85_2070 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_he85_2070", # Maximum temperature
      "WC_bio6_he85_2070", # Minimum temperature
      "WC_bio12_he85_2070" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )



# Crop the NZ extent ----
nz_extent <-
  extent(
    165,
    179,
    -48,
    -34.00
  )
nz_he26_2050 <- crop(layers_he26_2050, nz_extent)
nz_he85_2050 <- crop(layers_he85_2050, nz_extent)
nz_he26_2070 <- crop(layers_he26_2070, nz_extent)
nz_he85_2070 <- crop(layers_he85_2070, nz_extent)


names(nz_he26_2050) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_he85_2050) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_he26_2070) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_he85_2070) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")

# get layer summaries----
summary(nz_he26_2050)
summary(nz_he85_2050)
summary(nz_he26_2070)
summary(nz_he85_2070)


# 3. CCSM4 layers----
layers_cc26_2050 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_cc26_2050", # Maximum temperature
      "WC_bio6_cc26_2050", # Minimum temperature
      "WC_bio12_cc26_2050" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )

layers_cc85_2050 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_cc85_2050", # Maximum temperature
      "WC_bio6_cc85_2050", # Minimum temperature
      "WC_bio12_cc85_2050" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


layers_cc26_2070 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_cc26_2070", # Maximum temperature
      "WC_bio6_cc26_2070", # Minimum temperature
      "WC_bio12_cc26_2070" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )


layers_cc85_2070 <-
  load_layers(
    layercodes = c(
      "WC_alt" , # Altitude from clim_layers_names
      "WC_bio5_cc85_2070", # Maximum temperature
      "WC_bio6_cc85_2070", # Minimum temperature
      "WC_bio12_cc85_2070" # Annual precipitation
    ) ,
    equalarea = FALSE,
    rasterstack = TRUE,
    datadir = paste0(getwd(),"/data/climatic layers/")
  )



# Crop the NZ extent ----
nz_extent <-
  extent(
    165,
    179,
    -48,
    -34.00
  )
nz_cc26_2050 <- crop(layers_cc26_2050, nz_extent)
nz_cc85_2050 <- crop(layers_cc85_2050, nz_extent)
nz_cc26_2070 <- crop(layers_cc26_2070, nz_extent)
nz_cc85_2070 <- crop(layers_cc85_2070, nz_extent)


names(nz_cc26_2050) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_cc85_2050) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_cc26_2070) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")
names(nz_cc85_2070) <- c("WC_alt", "WC_bio5", "WC_bio6", "WC_bio12")


# Summarise layers--------
summary(nz_cc26_2050)
summary(nz_cc85_2050)
summary(nz_cc26_2070)
summary(nz_cc85_2070)

summary(nz_env)
