library(tidyverse)
library(terra)
library(sf)

# goal:
# read in daily prism rasters for a variable
# by looping through a list ? of file names (identified thru their path and sequence i.e. dates in their names)
# extract value from each raster to each point in a specified shapefile, creating an output that is a time series dataframe
# then we can use that time series to temporally aggregate up to any scale

# i also want to freshly download the data because idk what version kang got.

# function to load and process rasters for given years

# file naming conventions: (it's a lot of data btw ...)

# DAILY DATA (2000-2024)
# "../../Data/PRISM_20240829/Max_VPD"

  # folders by year
    # PRISM_vpdmax_stable_4kmD2_20000101_bil.bil

# "../../Data/PRISM_20240829/Mean_temperature"

  # folders by year
    # PRISM_tmean_stable_4kmD2_20000101_bil.bil

# "../../Data/PRISM_20240829/Precipitation"

  # folders by year
    # PRISM_ppt_stable_4kmD2_20000101_bil.bil

# function for processing data for a given set of points, between dates

# terra::extract()

# sample: 20000101, 20000110, "ppt", usp_alluvial

usp_alluvial <- st_read("data/Alluvial_well_locations.shp")

usp_al2 <- st_transform(usp_alluvial, "epsg:4269")
# turn into vector before extracting??

test_prism <- rast("../../Data/PRISM_20240829/vpdmax/2000/PRISM_vpdmax_stable_4kmD2_20000801_bil.bil")

prism_path <- "../../Data/PRISM_20240829/"

process_prism <- function(years, # YYYY:YYYY
                          variable_name, # "vpdmax", "tmean", "ppt"
                          points # vector object already read in
                          ){
  
  points_reproj <- st_transform(points, "epsg:4269")  # reproject points to raster CRS: NAD83 (PRISM)
  
  
  file_list <- list()
  
        for(i in 1:length(years)) {
        
            file_list[[i]] <- list.files(path = paste0(prism_path,
                                                   variable_name, "/",
                                                   as.character(years[i])),
                                     pattern = "\\.bil$", # list of files for a given year and variable
                                      full.names = T)

        }
  
  file_paths <- do.call(c, file_list)
  prism_rasts <- lapply(file_paths, terra::rast)
  
  extraction <- list()
            
            for(r in seq_along(prism_rasts)){
          
                  prism_file <- prism_rasts[[r]]
                  extraction[[r]] <- terra::extract(x = prism_file, y = points_reproj) %>%  # extracts
                                        select(-ID)
                  colnames(extraction[[r]]) <- str_sub(file_paths[r], -16, -9)
              }
            
  extracted_values <- do.call(cbind, extraction)
  extracted_values$name <- points$name
        
  # pivot but grouped by name....
  extraction <- extracted_values %>% 
    group_by(name) %>% 
    pivot_longer(cols = -name, names_to = "date", values_to = variable_name) %>% 
    ungroup() %>% 
    mutate(date = as.POSIXct(date, tryFormats = "%Y%m%d"))
  
  return(extraction)
  
}

test <- process_prism(2000:2001, "ppt", usp_alluvial)

ggplot(test, aes(x = date, y = ppt))+
  geom_line()+
  facet_wrap(~name)

extract(test_prism, usp_al2)
names(test_prism) <- paste0("ppt", "_", str_sub("PRISM_vpdmax_stable_4kmD2_20000801_bil.bil",
                                                -16, -9))


# loop thru each file:
# extract to points, add to data frame
# info to carry: attributes from points, raster value, date
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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