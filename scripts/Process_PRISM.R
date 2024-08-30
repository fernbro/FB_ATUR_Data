library(tidyverse)
library(terra)
library(sf)



# function to load and process rasters for given years
load_process_raster <- function(year) {
  raster_path <- file.path(lai_dir, paste0("Lai_", year, "_EOM.tif"))
  if (file.exists(raster_path)) {
    raster <- rast(raster_path) / 10
    lcmap_mask <- terra::resample(lcmap, raster, "near")
    raster <- mask(raster, lcmap_mask)
    return(raster)
  } else {
    warning(paste("Raster file for year", year, "not found. Skipping."))
    return(NULL)
  }
}

# list to store all rasters
lai_rast_list <- list()
# loop through each year from 2001 to 2021 with above function
for (year in 2001:2021) {
  raster <- load_process_raster(year)
  if (!is.null(raster)) {
    lai_rast_list[[as.character(year)]] <- raster
  }
}



for (year in seq_along(lai_rast_list)) {
  raster <- lai_rast_list[[year]]
  lai_list <- vector(mode = "list", length = 12)
  for(m in 1:12){
    lai_list[[m]] <- terra::extract(x = raster, y = lsu, layer = m, fun = mean, 
                                    ID = T)
  }
  lai_month_df <- dplyr::bind_rows(lai_list)
  lai_df_list[[year]] <- lai_month_df
}