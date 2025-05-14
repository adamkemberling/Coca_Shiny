# Testing Map  Display options for COCA Data
# More species than the fishviz displays, only maps



####  Packages  ####
{
  library(here)
  library(sf)
  library(tidyverse)
  library(gmRi)
}


####  Save model knot locations  ####
# I'm pulling out the minimal steps from 01_vast_projection_preprocessing.R

# Paths to Box Assets 
mills_path <- cs_path(box_group = "mills")
project_box_path <- str_c(mills_path, "Projects/COCA19_Projections/")
projection_res_path <- paste0(project_box_path, "projections/")

# I hate nested dataframes, so I'm just going to use a list at each step:
read_rds_func <- function(file_name) {
  out <- readRDS(paste0(file_name))
  return(out)}


# Loads all Files in the folder that end with _mean.rds
fpaths     <- list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)
fnames     <- str_remove(list.files(projection_res_path, pattern = "_mean.rds", full.names = FALSE, ), ".rds")
proj_files <- setNames(fpaths, fnames)



# Read each dataset into a list
proj_data <- map(proj_files, read_rds_func)

# Isolate just the estimated density information
density_estimates <- proj_data %>% map(~pluck(.x, "Dens"))



#### Pull Out Node Coordinates as Their Own Dataset  ####

# These are the original node locations, the ones we matched everything to using st_nearest
density_estimates$Butterfish_full_CMIP6_SSP1_26_mean


# # ^ This is the key to matching data to the correct spatial feature
# # This is a key for lat/lon positioning that matches to rolling_dens
# unique_pts %>% ggplot(aes(Lon, Lat, color = pt_id)) + geom_point() + theme_void()


# Get the locations as their own thing
unique_pts <- density_estimates$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  distinct(Lon, Lat) %>% 
  arrange(Lon, Lat) %>% 
  mutate(pt_id = row_number())






##### Save Unique Locations  ####

# write_csv(unique_pts, here::here("Data/spatial/unique_location_coords.csv"))
# write_csv(unique_pts, here::here("COCA_SDM_app_dev/dev/scratch_data", "unique_location_coords.csv"))





####  Load the Unique Locations (knots)  ####


# Unique locations to map
# From coca_shiny/R/01_vast_projection_processing.R
pt_latlon <- read_csv(
  file = here::here("COCA_SDM_app_dev/dev/scratch_data", "unique_location_coords.csv"),
  col_types = cols(
    Lon = col_double(),
    Lat = col_double(),
    pt_id = col_double()
  ))








#### Grid Creation  ####


# Take the same data, and make a fishnet grid
# Will preserve the other columns if they exist
sf_meshify <- function(input_df, coords = c("Lon", "Lat"), length_km = 25, in_crs = 4326, trans_crs = 32619, square = T){
  
  # Make the dataframe an sf class using coordinates
  in_sf <- st_as_sf(input_df, coords = coords, crs = in_crs, remove = F) %>% 
    # Transform it to a crs that is projected in meters
    st_transform(crs = trans_crs)
  
  # If we are getting gaps we can buffer here, or up the mesh size
  
  
  # Use that data to define a grid with dimensions of length_km*length_km
  sf_grid <- st_make_grid(
    x = in_sf,
    cellsize = c(length_km*1000, length_km*1000), 
    what = "polygons", 
    square = square) %>% 
    # Make the grid an sf class
    st_as_sf() 
  
  # Use the original data to trim it so its just cells that overlap the points
  sf_out <- sf_grid %>% 
    st_filter(in_sf, .predicate = st_contains) %>%
    st_as_sf() 
  
  # Join the clipped grid to the dataset
  sf_out <- st_join(sf_out, in_sf, join = st_intersects)
  # Return the results  
  return(sf_out)
  
}



# Testing the function: squares
sq_grid <- sf_meshify(input_df = pt_latlon) 
sq_grid %>%  
  ggplot() +
  geom_sf()


# Testing the function: hexagons
hex_grid <- sf_meshify(input_df = pt_latlon, square = F, length_km = 35) 
hex_grid %>% 
  ggplot() +
  geom_sf()





#### Save Unique Geometries  ####

# we've saved the unique coordinates
# here is where we could tweak the way we plot them...
# Then we just load one copy of the grid to join before plot time


# st_write(hex_grid, dsn = here::here("Data/spatial/hex_grid.geojson"))
st_write(hex_grid, dsn = here::here("COCA_SDM_app_dev/dev/scratch_data", "hex_grid.geojson"))

