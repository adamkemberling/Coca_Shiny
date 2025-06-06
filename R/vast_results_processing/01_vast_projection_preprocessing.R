####
#### Results Processing Workflow  ####
####


####
# Goal:
# Data should be in its smallest, app-ready state after these steps
# spatial information can be kept isolated from a csv or other flat file

### 
# Preliminary stuff
# Taking VAST outputs and preparing them for mapping
# 1. Isolates density information from the rest of the outputs
# 2. Performs rolling averaging within model runs and for ensemble mean/5th/95th
# 3. Creates distinct location information so we can join in the simple feature info later


#### Bonus Stuff:
# unique_pts object is built in here


####  Packages  ####
library(tidyverse)
library(raster)
library(sf)
# library(terra)
library(rnaturalearth)
library(gmRi)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")


####  Shapefiles  ####

# A land shapefile..
land_sf <- ne_states(
  c("united states of america", "canada"), 
  returnclass = "sf") %>%
    st_transform(., crs = 32619)

# 32619 = WGS84 UTM zone 19N


####  Overview  ####

###
# Model Overview
###
# We've got MODELS!! Now, what to do with em :) Just as a quick recap, for each species
# we fit the vector auto-regressive spatio-temporal models to a dataset that includes 
# the spring and fall NOAA NEFSC bottom trawl data and Fisheries and Oceans Canada spring and summer bottom trawl data. 
# Within this model structure, we have included depth, seasonal average sea surface temperature and 
# seasonal average bottom temperature as habitat covariates 
# (i.e., things we think influence the true, unobserved biomass of a species at a given location/time).
# We have also included survey as a factor cathability covariate to account for systematic differences 
# between the NOAA NEFSC bottom trawl and the DFO bottom trawl. Nex, we have estimated unmeasured spatial variability, and spatio-temporal variability. Finally, we have accounted for a random walk autoregressive process in the model intercepts (i.e., average occurrence of the species at a given time step across the entire survey domain). 

# After fitting the VAST model, 

# Next Steps: 
# we then make our projections using two different climate scenarios run as part of the CMIP6 effort: 
# one scenario is the shared socio-economic pathway (SSP) 1_2.6 and the other is SSP5_8.5. Within each scenario, a number of different climate model members are available and we calculate the average, 5th, and 95th percentiles across the ensemble of individual runs. Projections are made for every season-year from 1985 to 2100 using the seasonal average bottom and sea surface temperatures from the CMIP6 efforts. There's some more detail on the climate data processing steps here (https://gulfofmaine.github.io/sdm_workflow/docs/environmental-data-collection.html), particularly how we used the delta method to try to correct for the well known-warm bias of climate models within the northwest Atlantic. 

# Once we have completed the projection process, we end up with a few RData files for each species that will all have the same naming convention:
# SpeciesShortName_VASTModelDescription_CMIP6_SSPX_YY_ClimateEnsembleStat.rds
# SpeciesShortName = Just what it seems, a shorthand for the species (e.g., Haddock, Cod, Lobster)
# VASTModelDescription = A description of the VAST model, mostly for Andrew. All of these should be "full".
# CMIP6 = Place holder in the event we run CMIP5. All of these will be CMIP6
# SSPX = First part of the SSP description, where "X" will change depending on the scenario used
# YY = Second part of the SSP description, where YY will change depending on the scenario used (e.g., SSP5_85)
# mean = A description of the ensemble member statistic used, which can be mean, 5th or 95th. For now, all of these will be "mean"



####____________####

####  Projections Folder  ####



# Paths to Box Assets 
mills_path <- cs_path(box_group = "mills")
project_box_path <- str_c(mills_path, "Projects/COCA19_Projections/")
projection_res_path <- paste0(project_box_path, "projections/")








####____________####
#### 1. Load Projection Datasets  ####


# For each species/climate scenario/stat, we stored a number of different results including:

# 1. Index: The projected overall biomass index, with columns for time, Region, Prob_0.5 (mean across the 100 projection simulations to represent uncertainty in fitted VAST model), Prob_0.1 (10th percentile across 100 simulations), Prob_0.9 (90th percentile across 100 simulations). We will likely just use Prob_0.5.
# 2. Dens: The projected density (kg per km2) at each grid cell, 
# with columns for the Lat, Lon of the grid cell, time, and then similar probability measures as index.
# 3. COG: Ignore for now
# 4. COG_True: The center of gravity, with columns for time, and then we have the Lon/Lat median, 
# 10th and 90th percentiles across the 100 projection simulations. As above, we are just going to use Lon_Prob_0.5 and Lat_Prob_0.5
# 5. EffArea: Ignore for now
# 6. EffArea_True: The effective area occupied, with columns for Time, Region, 
# and then the probability measures. Again, we will just use "Prob_0.5" for this work. 



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

# Check one of these objects to see its structure
names(proj_data$Butterfish_full_CMIP6_SSP1_26_mean)
names(proj_data$Plaice_full_CMIP6_SSP1_26_mean)









#### 2. Isolate the Biomass Density Information ####

# For now, we are mostly going to be using the results in the "Dens" object to make our maps 
# and then also for cropping with community footprints to get our summaries of change. 
# Let's pull out the density results as a new column

 # Isolate just the density information
density_estimates <- proj_data %>% map(~pluck(.x, "Dens"))


# What does that get us? # Grouped Dataframe
test <- density_estimates[[1]]
str(test)








#### 3. Perform a 5-year Rolling Mean of Predicted Density  ####

# This would be so much faster in python...
# Changes:
# added standard deviation measurement to rolling means
# sd in mean prediction, not sd of predictions or 5th/95th

# Rolling mean Function
calc_seasyr_rollmean_func<- function(df, window_size = 5) {

    if (FALSE) {df = density_estimates[[1]]}
    
    # Add the Year and Season as cols from datetime
    temp <- df %>%
        mutate(.,
            "Year" = format(Time, "%Y"),
            "Month" = format(Time, "%m"), 
            "Season" = ifelse(
              grepl("03", Month), "Spring", 
              ifelse(grepl("07", Month), "Summer", "Fall")))
    
    # 1. Rolling mean by lat, lon, season
    # This gets the season and location specific smooth
    temp <- temp %>%
        group_by(Lat, Lon, Season) %>%
        mutate(
          "Prob_0.5_SY_RM" = zoo::rollapplyr(
            Prob_0.5, 
            width = window_size, 
            FUN = mean, 
            align = "center", 
            partial = TRUE)) %>%
        ungroup() %>%
        group_by(Lat, Lon, Year) %>%
        mutate(
          "Prob_0.5_Y_RM" = mean(Prob_0.5_SY_RM, na.rm = T),
          "Prob_0.5_Y_Rsd" = sd(Prob_0.5_SY_RM, na.rm = T)) %>% 
        ungroup()

    # Return
    return(temp)

}




#  Calculate a rolling mean to smooth the Changes in Density through time
rolling_dens <- density_estimates %>% map(calc_seasyr_rollmean_func)





# Here is what we have for every year and season...
rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  filter(Year == 1985) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, color = Prob_0.5)) + 
  facet_wrap(~Season, nrow = 3) +
  theme_dark() +
  scale_color_distiller(
    palette = "RdYlBu", 
    trans = "log10", 
    labels = scales::label_log(base = 10)) +
  labs(color = "Density kg/km2")


# Or for one of the new species
rolling_dens$BlackSeaBass_full_CMIP6_SSP5_85_mean %>% 
  filter(Year == 1985) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, color = Prob_0.5)) + 
  facet_wrap(~Season, nrow = 3) +
  theme_dark() +
  scale_color_distiller(
    palette = "RdYlBu") +
  labs(color = "Density kg/km2")

rolling_dens$Redfish_full_CMIP6_SSP5_85_mean %>% 
  filter(Year == 1985) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, color = Prob_0.5)) + 
  facet_wrap(~Season, nrow = 3) +
  theme_dark() +
  scale_color_distiller(
    palette = "RdYlBu") +
  labs(color = "Density kg/km2")



#### 4.  Assign/Verify Location ID's  ####

# # Do the base coordinates differ from the projection coordinates? Yes
# # Are the lat and Lon values the same across projections? Yes!
# ssp1_pts <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat)
# ssp5_pts <- rolling_dens$Butterfish_full_CMIP6_SSP5_85_mean %>% distinct(Lon, Lat)
# sum(ssp1_pts != ssp5_pts) # We good!

# # Do the new species differ from the old species coordinates? yes...
# # Are they atleast consistent between themselves? yes...
old_pts_1 <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat) %>% arrange(Lon, Lat)
new_pts_1 <- rolling_dens$BlackSeaBass_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat) %>% arrange(Lon, Lat)
new_pts_2 <- rolling_dens$AtlanticMackerel_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat) %>% arrange(Lon, Lat)
sum(new_pts_1 != old_pts_1)
sum(new_pts_1 != new_pts_2)


# # Whats with these NAs
# nrow(old_pts_1); nrow(new_pts_1) # same number of rows
# # 197 mismatches, apparently
# new_pts_1[which(new_pts_1$Lon != old_pts_1$Lon),]  # 8 Longitude mismatches
# new_pts_1[which(new_pts_1$Lat != old_pts_1$Lat),]  # 1 79 Lat mismatches
# old_pts_1 %>% filter(is.na(Lon))
# new_pts_1 %>% filter(is.na(Lon))
# 
# 
# 
# # Friggin match them up...
# ggplot() +
#   geom_point(data = old_pts_1, aes(Lon, Lat, color = "old nodes"), alpha = 0.25, shape = 3) +
#   #geom_point(data = new_pts_1, aes(Lon, Lat, color = "new nodes"), alpha = 0.25, shape = 3) +
#   geom_point(data = new_pts_1[which(new_pts_1$Lon != old_pts_1$Lon),], aes(Lon, Lat, color = "Longitude Mismatch"), alpha = 0.5) +
#   geom_point(data = new_pts_1[which(new_pts_1$Lat != old_pts_1$Lat),], aes(Lon, Lat, color = "Latitude Mismatch"), alpha = 0.5)
# old_pts_1$Lon[1:5]
# new_pts_1$Lon[1:5]
# old_pts_1$Lat[1:5]
# new_pts_1$Lat[1:5]
# 
# new_pts_sf <- st_as_sf(new_pts_1, coords = c("Lon", "Lat"), crs = 4326, remove = F) %>% setNames(c("lon_new", "lat_new", "geometry"))
# old_pts_sf <- st_as_sf(old_pts_1, coords = c("Lon", "Lat"), crs = 4326, remove = F)
# 
# # If you order them ahead of time, and they are the same number of nodes 
# # in roughly the same place we should be able to just replace them...
# st_nearest_feature(new_pts_sf, old_pts_sf)



####  Perform Node Alignment  ####


# Perform node_matching for the stupid new species
match_nodes <- function(new_densities, old_densities){
  
  # Make sure the coordinates are aligned
  new_densities <- new_densities %>% arrange(Lon, Lat)
  new_pts <- new_densities %>% distinct(Lon, Lat) 
  new_pts_sf <- new_pts %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F)
  
  # Do so for the other points that were using for unique_pts
  old_densities <- old_densities %>% arrange(Lon, Lat)
  old_pts <- old_densities %>% distinct(Lon, Lat)
  old_pts_sf <- st_as_sf(old_pts, coords = c("Lon", "Lat"), crs = 4326, remove = F)
  
  # take coordinates from the nearest node in the old locations
  new_coords <- st_coordinates(old_pts_sf)[st_nearest_feature(new_pts_sf, old_pts_sf),]
  
  # Join them to new_pts
  new_pts <- rename(new_pts, lon_out = Lon, lat_out = Lat)
  new_pts$Lon <- new_coords[,1]
  new_pts$Lat <- new_coords[,2]
  
  # Now replace them and return the file
  out_df <- new_densities %>% 
    rename(lon_out = Lon, lat_out = Lat) %>% 
    left_join(new_pts) %>% 
    select(-c(lon_out, lat_out))
  
  return(out_df)
  
}



# Perform the switcheroo
rolling_dens <- map(
  .x = rolling_dens, 
  .f = ~match_nodes(.x, old_densities = rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean))



# double check
# # Do the new species differ from the old species coordinates? NO
# # Are they atleast consistent between themselves? yes...
old_pts_1 <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat) %>% arrange(Lon, Lat)
new_pts_1 <- rolling_dens$BlackSeaBass_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat) %>% arrange(Lon, Lat)
sum(new_pts_1 != old_pts_1)




#### Pull Out Node Coordinates as Their Own Dataset  ####


# # ^ This is the key to matching data to the correct spatial feature
# # This is a key for lat/lon positioning that matches to rolling_dens
# unique_pts %>% ggplot(aes(Lon, Lat, color = pt_id)) + geom_point() + theme_void()


# Get the locations as their own thing
unique_pts <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  distinct(Lon, Lat) %>% 
  arrange(Lon, Lat) %>% 
  mutate(pt_id = row_number())


# Pair the pt_id back into the main dataset
rolling_dens <- map(rolling_dens, ~left_join(.x, unique_pts, by = join_by(Lat, Lon)))




##### Save Unique Locations  #### 
# write_csv(unique_pts, here::here("Data/spatial/unique_location_coords.csv"))
# write_csv(unique_pts, here::here("COCA_SDM_app_dev/dev/scratch_data", "unique_location_coords.csv"))









#### 5. Saving Rolling Average Densities  ####

# Right now we have density estimates for all the ensemble means & (10th, 90th percentiles):
all_density_results <- rolling_dens %>% 
  bind_rows(.id = "VAST_id")


# Save here, and split the subroutines off into their own scripts
#write_csv(all_density_results, here::here("Data/projections/VAST_all_densities_all_species.csv"))
write_csv(all_density_results, here::here("COCA_SDM_app_dev/dev/projections/VAST_all_densities_all_species.csv"))









####_______________________####

####  Side Deliverable: Baseline Densities:  ####



####  Baseline (2010-2019) Average Densities   
# Once we have baselines without the rolling average, Carly needs them cropped to
# the different footprints


#  Starting point: density estimates
density_estimates[[1]]

# Ending structure we want to emulate: all_Density_results
all_density_results <- rolling_dens %>%
  bind_rows(.id = "VAST_id")

# Need: Year, Month, Season, pt_id
# We want the average across years 2010-2019
densities_baseline_preroll <- density_estimates %>%
  map_dfr(function(x){
    x <- x %>%
      mutate(
        Year = format(Time, "%Y"),
        Month = format(Time, "%m"),
        Season = ifelse(
          grepl("03", Month), "Spring",
          ifelse(grepl("07", Month), "Summer", "Fall"))) %>%
      filter(Year %in% c(2010:2019)) %>%
      mutate(Year = "2010-2019") %>%
      group_by(Year, Month, Season, Lat, Lon) %>%
      summarise(across(c("Prob_0.5", "Prob_0.1", "Prob_0.9"), ~mean(.x, na.rm = T)),
                .groups = "drop")
    
    # Perform the node matching
    match_nodes(x, old_densities = rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean)
    
  },.id = "VAST_id") %>%
  left_join(unique_pts, join_by(Lat, Lon))



# Save the baseline average densities
# write_csv(densities_baseline_preroll, here::here("Data/projections/VAST_baseline_2010to2019_densities_all_species.csv"))
write_csv(densities_baseline_preroll, here::here("COCA_SDM_app_dev/dev/projections/VAST_baseline_2010to2019_densities_all_species.csv"))






##### Validate all Data is within Study Area  ####

# We've given all the locations an ID that can be matched to a polygon geometry
# Doing so lets us do all the processing steps as a regular dataframe, 
# then add the geometry on at the end before plotting


# Study area outline shapefile
domain_use <- st_read(str_c(cs_path("mills", "Projects/sdm_workflow/data 2/supporting/region_shapefile"), "full_survey_region.shp"))
ggplot() + 
  geom_sf(data = domain_use) + 
  geom_point(data = unique_pts, aes(Lon, Lat)) +
  ggtitle("Study Area") + theme_bw()


# Invalid geometries eh... Have to turn off S2
st_make_valid(domain_use)

# turn off s2
sf_use_s2(FALSE)

# Unique locations as sf
dens_sf_test <- unique_pts %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F) 

# Points inside study area:
unique_pts_within <- st_join(x = dens_sf_test, y = st_make_valid(domain_use))

# st_join is Wayyyy faster than st_within for checking if they fall in an area
unique_pts_within %>% 
  ggplot() +
  geom_sf(data = domain_use) +
  geom_sf()













