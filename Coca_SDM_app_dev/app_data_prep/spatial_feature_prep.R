# Land Mask Prep

# Goal: make a coastline shapefile for the relevant study area:


####  Packages  ####
library(tidyverse)
library(sf)
library(rnaturalearth)

# # Support Functions:
# source(here::here("R/app_support_functions.R"))

res_shapepath <- gmRi::cs_path("res", "Shapefiles")


# 1. Loading land mask
bbox <- c(xmin = -78.5, ymin = 34.5, xmax = -54.5, ymax = 48.25)
land_sf <- sf::st_read(paste0(res_shapepath, "ne_50m_land/ne_50m_land.shp")) %>%
  sf::st_crop(bbox)
land_sf <- sf::st_transform(land_sf, crs = 32619)

# 2. Land Mask with country Borders
# Load countries with borders
canada_sf <- ne_states("canada", returnclass = "sf")
us_sf <- ne_states("united states of america", returnclass = "sf")
north_america <- bind_rows(list(mutate(canada_sf, country = "Canada"),
                                mutate(us_sf, country = "United States")))

na_bbox <- c(xmin = -79, ymin = 33.5, xmax = -54, ymax = 49)
north_cropped <- st_crop(north_america, na_bbox)
north_projected <-  sf::st_transform(north_cropped, crs = 32619)
ggplot(north_projected) + geom_sf(fill = "gray60", color = "white", size = 0.25) +
  coord_sf(xlim = c(-182000, 1570000), ylim = c(3870000, 5385000), expand = F, crs = 32619) 

# # Save it
# sf::write_sf(north_cropped, here::here("COCA_SDM_app_dev/dev/scratch_data", "nw_atlantic_countries_crs4326.geojson"))
# sf::write_sf(north_projected, here::here("COCA_SDM_app_dev/dev/scratch_data", "nw_atlantic_countries_crs32619.geojson"))



####  2. Hague Line  ####
# From: https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/mfr46413.pdf
# "Eastern U.S.-Canada Boundary Line Drawn by World Court

# Build Turning Points:
hague <- data.frame(
  "lat_deg" = c(44, 42, 42, 40),
  "lat_min" = c(11, 53, 31, 27),
  "lat_sec" = c(12, 14, 08, 05),
  "lon_deg" = c(67, 67, 67, 65)*-1,
  "lon_min" = c(16, 44, 28, 41)*-1,
  "lon_sec" = c(46, 35, 05, 59)*-1,
  "point" = c("A", "B", "C", "D"),
  "shape" = "Hague Line"
) %>% 
  mutate(lon_decdeg = lon_deg + (lon_min/60) + (lon_sec/3600),
         lat_decdeg = lat_deg + (lat_min/60) + (lat_sec/3600))


# Make it an SF object
hague_sf <- st_as_sf(hague, coords = c("lon_decdeg", "lat_decdeg"), remove = FALSE, crs = 4326)

# Make it a linestring?
hague_line <- hague_sf %>% 
  group_by(shape) %>% 
  st_cast("LINESTRING")

# Make linestring an sfc
hague_line <- st_linestring(cbind(hague$lon_decdeg, hague$lat_decdeg)) %>% st_sfc(crs = 4326) 

# # Dumb way to make sf linestring
# hague_sf %>% select(shape) %>% 
#   st_drop_geometry() %>% 
#   slice(1) %>% 
#   st_set_geometry(hague_line)

# Make sf for the line
hague_line_sf <- data.frame(shape = "Hague Line") %>% 
  st_set_geometry(hague_line)


# Plot
ggplot() +
  geom_sf(data = hague_line_sf) +
  geom_sf(data = hague_sf)

# Transform it:
hague_wgs <- hague_line_sf
hague_32619 <- st_transform(hague_wgs, crs = 32619)

# sf::write_sf(hague_wgs,   here::here("COCA_SDM_app_dev/dev/scratch_data", "hagueline_crs4326.geojson"))
# sf::write_sf(hague_32619, here::here("COCA_SDM_app_dev/dev/scratch_data", "hagueline_crs32619.geojson"))




####   Hex Grid  ####


