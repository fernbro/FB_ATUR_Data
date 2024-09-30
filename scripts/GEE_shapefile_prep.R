library(sf)
library(terra)

sprnca <- st_read("data/SPRNCA/Stromberg_reaches.shp")
crs(sprnca)

sprnca_sub <- sprnca[,c("geometry", "ReachID")]

st_write(sprnca_sub, "data/SPRNCA/SPRNCA_Reaches.shp")

for(i in seq(1, 14, 1)){
  tmp <- subset(sprnca_sub, ReachID == i)
  st_write(tmp, paste0("data/SPRNCA/SPRNCA_Reaches/SPRNCA_Reach",
                       as.character(i), ".shp"))
}


# library(sp)
# library(rgdal)
# 
# #read shp
# singlepoly = readOGR("C:/Users/Geography/Desktop/test_cnn", layer = "samples1_exchange")
# #get the names of the attribute table
# names(singlepoly)
# 
# #select the column of the attribute table that will determine the split of the shp
# unique <- unique(singlepoly$id)
# 
# #create new polygons based on the determined column
# for (i in 1:length(unique)) {
#   tmp <- singlepoly[singlepoly$id == unique[i], ]
#   writeOGR(tmp, dsn=getwd(), unique[i], driver="ESRI Shapefile",
#            overwrite_layer=TRUE)
# }