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

prism_path <- "../../Data/PRISM_20240829/"

process_prism <- function(years, # YYYY:YYYY
                          variable_name, # "vpdmax", "tmean", "ppt"
                          points # vector object already read in: needs a 'name' attribute
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

# Example:
# alluvial_prism_ppt <- process_prism(2000:2024, "ppt", usp_alluvial)
# write_csv(alluvial_prism_ppt, "data/USP_AlluvialWells_PRISM_ppt.csv")

swrc_inst <- st_read("data/SWRC/SWRC_DAP_Instrumentation.shp")
ars_inst <- subset(swrc_inst, siteTypeSt == "Raingage" &
                   name %in% c("wg_rg001", "wg_rg400", "wg_rg417", "wg_rg418"))
ars_ppt <- process_prism(2000:2024, "ppt", ars_inst) # 50 or more warnings???
ars_tmean <- process_prism(2000:2024, "tmean", ars_inst)
ars_vpdmax <- process_prism(2000:2024, "vpdmax", ars_inst)

write_csv(ars_ppt, "data/ARS_Gages_PRISM_ppt.csv")
write_csv(ars_tmean, "data/ARS_Gages_PRISM_tmean.csv")
write_csv(ars_vpdmax, "data/ARS_Gages_PRISM_vpdmax.csv")

# usp_gen <- st_read("data/General_USPWHIP_well_locations.shp")
# 
# usp_gen_ppt <- process_prism(2000:2024, "ppt", usp_gen)
# usp_gen_tmean <- process_prism(2000:2024, "tmean", usp_gen)
# usp_gen_vpdmax <- process_prism(2000:2024, "vpdmax", usp_gen)
# 
# write_csv(usp_gen_ppt, "data/USP_RegionalWells_PRISM_ppt.csv")
# write_csv(usp_gen_tmean, "data/USP_RegionalWells_PRISM_tmean.csv")
# write_csv(usp_gen_vpdmax, "data/USP_RegionalWells_PRISM_vpdmax.csv")
