####
#### Results Processing Workflow  ####
####

### 
# Preliminary stuff
# Taking VAST outputs and preparing them for mapping
###



####
# Goal:
# Data should be in its smallest, app-ready state after these steps
# spatial information can be kept isolated from a csv or other flat file
#



####  Packages  ####

library(tidyverse)
library(raster)
library(sf)
library(terra)
library(rnaturalearth)
library(gmRi)



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




####  Projects Folder  ####

###
# Example workflow -- mapping density
###
# Feel free to work with these projection files in whichever way works best for you to get to the end goal! 
# Below is just an example to show you what is in these rds files and how I have visualized some of 
# the projection data before. 



# Annoying box nonsense. We aren't going to be able to use `here` because the files would 
# overwhelm GitHub so we don't want them in the repo.
mills_path <- cs_path(box_group = "mills")
project_box_path <- str_c(mills_path, "Projects/COCA19_Projections/")
projection_res_path <- paste0(project_box_path, "projections/")



#### 1. Load the Projection Data  ####

# I hate nested dataframes, so I'm just going to use a list at each step:
read_rds_func <- function(file_name) {
    out <- readRDS(paste0(file_name))
    return(out)
}

# # Make table of the file names
# # Nest the data into that tibble, NO
# all_proj_res <- tibble(
#   "File_Path" = list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)) %>%
#   mutate("Data" = map(File_Path, read_rds_func))
# names(all_proj_res$Data[[1]])

# Don't make some confusing nest:
fpaths <- list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)
fnames <- str_remove(list.files(projection_res_path, pattern = "_mean.rds", full.names = FALSE), ".rds")
proj_files <- setNames(fpaths, fnames)

# Load the data into a list
proj_data <- map(proj_files, read_rds_func)

# Look at just one of these objects to see its structure
names(proj_data$Butterfish_full_CMIP6_SSP1_26_mean)


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








#### 2. Get Biomass Density  ####

# For now, we are mostly going to be using the results in the "Dens" object to make our maps 
# and then also for cropping with community footprints to get our summaries of change. 
# Let's pull out the density results as a new column



 # Isolate just the density information
density_estimates <- proj_data %>% map(~pluck(.x, "Dens"))


# What does that get us? # Grouped Dataframe
test <- density_estimates[[1]]
str(test)






#### 3. Rolling Mean Density  ####

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


# Check one
rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean


# Here is what we have for every year and season...
rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  filter(Year == 1985, Season == "Summer") %>% 
  ggplot(aes(Lon, Lat, color = Prob_0.5)) +
  geom_point() +
  scale_color_distiller(palette = "RdYlBu", trans = "log10", labels = scales::label_log(base = 10)) +
  labs(color = "Density kg/km2")











#### Location ID's  ####

# Do the base coordinates differ from the projection coordinates? Yes
# Are the lat and Lon values the same across projections? Yes!
ssp1_pts <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat)
ssp5_pts <- rolling_dens$Butterfish_full_CMIP6_SSP5_85_mean %>% distinct(Lon, Lat)
sum(ssp1_pts != ssp5_pts) # We good


# Get the locations as their own thing
unique_pts <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  distinct(Lon, Lat) %>% 
  mutate(pt_id = row_number())

# Pair the pt_id back into the main dataset
rolling_dens <- map(rolling_dens, ~left_join(.x, unique_pts, by = join_by(Lat, Lon)))




#### Save Unique Points  ####

# This is a key for lat/lon positioning that matches to rolling_dens
unique_pts %>% ggplot(aes(Lon, Lat, color = pt_id)) + geom_point()

# Save it 
write_csv(unique_pts, here::here("Data/spatial/unique_location_coords.csv"))



####  Streamline Density Information  ####


# Goals here:

# 1. Reshape + Organize for the bare min needed to plot
# 

# How many species do we have...

# 31 species
length(rolling_dens)/2 
# 2 SSP scenarios
# 657 locations
nrow(unique_pts) 
# 3 Seasons + annual average (do we need all?)
# Display periods:






####____________####
####  Summarization Steps  ####


#### 1. Trim to a defined area


# Study area outline shapefile
domain_use <- st_read(str_c(cs_path("mills", "Projects/sdm_workflow/data 2/supporting/region_shapefile"), "full_survey_region.shp"))
ggplot() + geom_sf(data = domain_use) + ggtitle("Study Area")


# This should be handled at step 1:
# give all the locations an ID that can be matched to a polygon geometry
# Do all the processing as a dataframe, then add the geometry on at the end

# Can we speed up or simplify the sf_within thing?
# Can we even use it?
# invalid geometries eh... Have to turn off S2
st_make_valid(domain_use)

# OR turn of s2
sf_use_s2(FALSE)


# Data as sf, plotted
dens_sf_test <- unique_pts %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F) 
dens_sf_test %>% st_transform(crs = 32619) %>% ggplot() + geom_sf()

# Points inside study area:
unique_pts_within <- st_join(x = dens_sf_test, y = st_make_valid(domain_use))

# st_join is Wayyyy faster than st_within
unique_pts_within %>% 
  ggplot() +
  geom_sf(data = domain_use) +
  geom_sf()


####  Summarize Time Steps  ####


# 1. Summarize baseline and projection years
# a. Subset to study area again to be sure
# b. Get mean over a period for baseline
# c. Get snapshots at specific points in time
# d. Get variance for standardizing?

filter_and_summarize <- function(
    density_in,        # Data to process
    spatial_domain,    # sf for doman
    unique_pts,        # subset of distinct coordinates
    baseline_years = seq(from = 2010, to = 2019), 
    projection_years = c(2050, 2100),   # Years to get a summary
    # For getting a seasonal average:
    run_seasons = FALSE,
    # For overlaying points in/out of area, not a huge issue for our area
    use_s2 = FALSE 
){      
  
  # Spatial overlay within our domain...
  sf_use_s2(use_s2)
  
  # Joins the points that are within the shape,
  # then it drops the sf functionality
  pts_keep <- unique_pts %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F) %>% 
    st_join(spatial_domain, join = st_within) %>%
    st_drop_geometry() %>%
    dplyr::select(-Region) 
  
  # Filter whether points are in domain
  dens_data <- density_in %>%
    filter(pt_id %in% pts_keep$pt_id)  %>% 
    mutate(#pt_id = as.character(pt_id),
           #pt_id = factor(pt_id),
           Year = as.numeric(Year))
  
  # Set whether to care about seasons
  if(run_seasons == TRUE){group_by_lvls <- c("Season", "pt_id")} 
  if(run_seasons == FALSE){group_by_lvls <- c("pt_id")} 
  
  # Get "average" across the baseline years..
  # This is what the different periods will compare against
  min_yr <-  min(baseline_years)
  max_yr <-  max(baseline_years)
  base_data <- dens_data %>%
    filter(between(Year, min_yr, max_yr)) %>% 
     group_by(across(all_of(group_by_lvls))) %>%
    summarise("baseline_mu" = mean(Prob_0.5_Y_RM),
              "baseline_sd"  = mean(Prob_0.5_Y_Rsd),
              .groups = "drop")  %>%
    # Make it longer using those two variables: mean and sd
    pivot_longer(
      cols = all_of(c("baseline_mu", "baseline_sd")),
      names_to = "var",
      values_to = "val")  %>%
    mutate(ref_period = str_c(min_yr, "-", max_yr))
  
  
  
  # Now the future scenarios...
  # Don't need standard deviation here, but already passed the code on
  projection_yrs <- setNames(projection_years, projection_years)
  projection_data <- map_dfr(projection_yrs, function(proj_yr){
    # Filter to that year and get mean, sd
    fut_data_temp <- dens_data %>%
      filter(
        format(Time, "%Y") == proj_yr) %>%
      group_by(across(all_of(group_by_lvls))) %>%
      summarise(mu_name = mean(Prob_0.5_Y_RM),
                sd_name = mean(Prob_0.5_Y_Rsd),
                .groups = "drop")
    
    # rename those stand-in cols
    proj_mu <- str_c("proj_", proj_yr, "_mu")
    proj_sd <- str_c("proj_", proj_yr, "_sd")
    fut_data_temp <- fut_data_temp %>% 
      rename(
        !!proj_mu := mu_name,
        !!proj_sd := sd_name
      )
    
    # Pivot "Wide" to "long"
    fut_data_temp <- fut_data_temp %>% 
      pivot_longer(
        cols = all_of(c(proj_mu, proj_sd)), 
        names_to = "var", 
        values_to = "val") %>% 
      mutate(ref_period = as.character(proj_yr))
    
    
  })
  
  # Bring em all together
  all_dens_data <- bind_rows(base_data, projection_data)
  return(all_dens_data)
  
}


# Does it work for one?
filt_name <- "Butterfish_full_CMIP6_SSP1_26_mean"
filt_test <- filter_and_summarize(
  density_in = rolling_dens[[filt_name]], 
  spatial_domain = domain_use, 
  unique_pts = unique_pts, 
  baseline_years = c(2010, 2019), 
  projection_years = c(2050, 2100), 
  run_seasons = F, 
  use_s2 = F)

# Get the species and SSP scenario from the file names
# Join to unique_pts (just to test, we can do this step in app quickly)
filt_test %>% 
  mutate(file_name = filt_name,
         file_name = str_replace(file_name, "_full_", "-")) %>% 
  separate(file_name, into = c("species", "scenario"), sep = "-") %>% 
  left_join(unique_pts)




##### Bare Necessities  ####
bare_essentials <- imap_dfr(rolling_dens, function(dens_data, file_name){
  
  projection_summary <- filter_and_summarize(
    density_in = dens_data, 
    spatial_domain = domain_use, 
    unique_pts = unique_pts, 
    baseline_years = c(2010, 2019), 
    projection_years = c(2050, 2100), 
    run_seasons = F, 
    use_s2 = F)
  
  # add species and scenario details from list names
  summary_out <- projection_summary %>% 
    mutate(file_name = file_name,
           file_name = str_replace(file_name, "_full_", "-")) %>% 
    separate(file_name, into = c("species", "scenario"), sep = "-") %>% 
    mutate(
      species = str_replace_all(species, "_", " "),
      species = tolower(species),
      scenario = str_remove(scenario, "_mean")
    )
  
  # return the summary
  return(summary_out)
  
})



# How big is this beast?

# Save it
write_csv(bare_essentials, here::here("Data/projections/all_species_test.csv"))


# Do it again but with seasons
bare_essentials_sznz <- imap_dfr(rolling_dens, function(dens_data, file_name){
  
  projection_summary <- filter_and_summarize(
    density_in = dens_data, 
    spatial_domain = domain_use, 
    unique_pts = unique_pts, 
    baseline_years = c(2010, 2019), 
    projection_years = c(2050, 2100), 
    run_seasons = T, 
    use_s2 = F)
  
  # add species and scenario details from list names
  summary_out <- projection_summary %>% 
    mutate(file_name = file_name,
           file_name = str_replace(file_name, "_full_", "-")) %>% 
    separate(file_name, into = c("species", "scenario"), sep = "-") %>% 
    mutate(
      species = str_replace_all(species, "_", " "),
      species = tolower(species),
      scenario = str_remove(scenario, "_mean")
    )
  
  # return the summary
  return(summary_out)
  
})


# and save that
write_csv(bare_essentials_sznz, here::here("Data/projections/all_seasons_test.csv"))





####  Projections Wide  ####

## Start from saved files

# Unique locations to map
pt_latlon <- read_csv(
  file = here::here("Data/spatial/unique_location_coords.csv"),
  col_types = cols(
    Lon = col_double(),
    Lat = col_double(),
    pt_id = col_double()
  ))

# Annual Averages
annual_avg <- read_csv(
  file = here::here("Data/projections/all_species_test.csv"), 
  col_types = cols(
    pt_id = col_double(),
    var = col_character(),
    val = col_double(),
    ref_period = col_character(),
    species = col_character(),
    scenario = col_character()
  ))

# Seasonal Averages
seasonal_avg <- read_csv(
  file = here::here("Data/projections/all_seasons_test.csv"), 
  col_types = cols(
    Season = col_character(),
    pt_id = col_double(),
    var = col_character(),
    val = col_double(),
    ref_period = col_character(),
    species = col_character(),
    scenario = col_character()
  ))





####  Preparing Projection differences  ####

# 3. Get Diffs
# Standardize the difference by the BASE sd, by species

# Do the annual file
annual_wide <- annual_avg %>% 
  split(.$species) %>% 
  map_dfr(function(spec_i){
    # Prepare the wide dataframe
    spec_i %>% 
      mutate(var_type = case_when(
        str_detect(var, "mu") ~ str_c("mean_", ref_period),
        str_detect(var, "sd") ~ str_c("sd_", ref_period))) %>% 
      select(-all_of(c("ref_period", "var"))) %>% 
      pivot_wider(
        names_from = var_type,
        values_from = val
      ) %>% 
      # Get differences 
      mutate(
        diff_2050   = `mean_2050` - `mean_2010-2019`,
        diff_2050_z = diff_2050 / `sd_2010-2019`,
        diff_2100   = `mean_2100` - `mean_2010-2019`,
        diff_2100_z = diff_2100 / `sd_2010-2019`)
    
  })
  

# Trim excess columns
# Don't need variance on projections if we are only mapping the differences\
clean_annual_wide <- annual_wide %>% select(pt_id, species, scenario, `mean_2010-2019`, diff_2050, diff_2050_z, diff_2100, diff_2100_z)


# Do the seasonal file
seasonal_wide <- seasonal_avg %>% 
  split(.$species) %>% 
  map_dfr(function(spec_i){
    # Prepare the wide dataframe
    spec_i %>% 
      mutate(var_type = case_when(
        str_detect(var, "mu") ~ str_c("mean_", ref_period),
        str_detect(var, "sd") ~ str_c("sd_", ref_period))) %>% 
      select(-all_of(c("ref_period", "var"))) %>% 
      pivot_wider(
        names_from = var_type,
        values_from = val
      ) %>% 
      # Get differences 
      mutate(
        diff_2050   = `mean_2050` - `mean_2010-2019`,
        diff_2050_z = diff_2050 / `sd_2010-2019`,
        diff_2100   = `mean_2100` - `mean_2010-2019`,
        diff_2100_z = diff_2100 / `sd_2010-2019`)
    
  })


# Trim excess columns
# Don't need variance on projections if we are only mapping the differences\
clean_season_wide <- seasonal_wide %>% select(pt_id, species, scenario, season = Season, `mean_2010-2019`, diff_2050, diff_2050_z, diff_2100, diff_2100_z)




####  Export
write_csv(clean_annual_wide, here::here("Data/projections/annual_proj_wide.csv"))
write_csv(clean_season_wide, here::here("Data/projections/seasonal_proj_wide.csv"))





####__________________####
####__________________####
####__  Mapping Options  __####

# These steps have been refined and moved to their own scripts

# These can all start from rolling dens, or the summarized outputs
# the latter can be joined by pt_id to unique_pts
# OR they can be joined with the geometries we desire



#### Creating A Grid From Points?  ####

# Now, I think you could create a "mapping" function for the surfaces? 
# The "..." is where you might be able to easily add in other things to the plotting function. 
# Relying on some stuff Adam did for FishViz 
# (https://github.com/gulfofmaine/FishVis/blob/master/R/app_support_functions.R)


####  Option 1: Buffer + Square  ####

# What does this do?
# Takes the coordinates for the data and makes a rectangle around that centroid point
# Uses buffer and a desired area. Input area uses the units of coords
bSquare <- function(x, a, coords = c("x", "y")) {
    a <- sqrt(a) / 2 # Get square root of area to get a length for buffer distance
    x_temp <- sf::st_as_sf(x, coords = coords, crs = 4326, remove = FALSE)
    x <- st_transform(x_temp, crs = 32619) #%>% drop_na(Value)
    x <- x %>%
        mutate(., geometry = sf::st_buffer(geometry,
            dist = a,
            nQuadSegs = 1,
            endCapStyle = "SQUARE"
        ))
    return(x)
}



# Results of bsquare
test_bsquare <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  filter(Year == 1985) %>% 
  bSquare(a = 25000^2, coords = c("Lon", "Lat")) 
test_bsquare %>% ggplot() + geom_sf()


####  Option 2: Square/Hexagon st_make_grid  ####




# Take the same data, and make a fishnet grid
# Will preserve the other columns if they exist
sf_meshify <- function(input_df, coords = c("Lon", "Lat"), length_km = 25, in_crs = 4326, trans_crs = 32619, square = T){
  
  # Make the dataframe an sf class using coordinates
  in_sf <- st_as_sf(input_df, coords = coords, crs = in_crs, remove = F) %>% 
    # Transform it to a crs that is projected in meters
    st_transform(crs = trans_crs)
  
  # If we are getting gaps we can buffer here:
  
  
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
sf_meshify(input_df = unique_pts) %>%  
  ggplot() +
  geom_sf()


# Testing the function: hexagons
sf_meshify(unique_pts, square = F, length_km = 30) %>% 
  ggplot() +
  geom_sf()













####__________________####
####__________________####
####  Old Functions  ####
####  Plotting Function  ####
# Now a plotting function...

# What is this function doing:
# Take the density data
# Set the baseline period and the projection years'
# Spatial overlay the points that fall within study area
# Get an average over the entire baseline period
# Get the average for the specific projection years
# Then: should split the workflow here
# Plot the pieces
plot_raw_dens_func <- function(
    dens_data, # Expects a nested df as is
    baseline_years = seq(from = 2010, to = 2019), 
    projection_years = c(2055, 2075), # Years to get a summary
    domain = domain_use,  # Spatial area that data must be within
    ...){

    # For debugging, this won't run by itself. Have to go line by line inside the "if"
    if(FALSE){
        dens_data <- all_proj_res$Density[[15]]
        baseline_years <- seq(from = 2010, to = 2019)
        projection_years<- c(2055, 2075)
    }

    # Spatial overlay within our domain...
  
    # Joins the points that are within the shape,
    # then it drops the sf functionality
    pts_keep <- dens_data %>%
        ungroup() %>%
        distinct(Lon, Lat) %>%
        st_as_sf(., coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) %>%
        st_join(., domain, join = st_within) %>%
        filter(., Region == "All") %>%
        st_drop_geometry() %>%
        dplyr::select(-Region) 
      
    # Filter whther points are in domain
    dens_data<- dens_data %>%
        filter(Lat %in% pts_keep$Lat & Lon %in% pts_keep$Lon)


    # Going to want an "average" across the baseline years..
    base_data <- dens_data %>%
        filter(., format(Time, "%Y") >= min(baseline_years) & format(Time, "%Y") <= max(baseline_years)) %>%
        group_by(., Lat, Lon) %>%
        summarize(., "Baseline_Mean_Dens" = mean(Prob_0.5_Y_RM)) %>%
        pivot_longer(., -c(Lat, Lon), names_to = "Variable", values_to = "Value")
    
    # Now the future scenarios...
    for(i in seq_along(projection_years)){
        fut_data_temp <- dens_data %>%
            filter(., format(Time, "%Y") == projection_years[i]) %>%
            group_by(., Lat, Lon) %>%
            summarize(., "Temp_Name" = mean(Prob_0.5_Y_RM))
        colnames(fut_data_temp)[3] <- paste0("Projected_", projection_years[i], "_Mean_Dens")
        
        # "Wide" to "long"
        fut_data_temp <- fut_data_temp %>%
            pivot_longer(., -c(Lat, Lon), names_to = "Variable", values_to = "Value")
        
        if (i == 1) {
            fut_data_out <- fut_data_temp
        } else {
            fut_data_out <- bind_rows(fut_data_out, fut_data_temp)
        }   
    }

    # Bring em all together
    all_dens_data <- bind_rows(base_data, fut_data_out)

    # Create "grid cells"
    all_dens_grid <- bSquare(all_dens_data, a = 25000 * 25000, coords = c("Lon", "Lat"))
    
    # Plot em...
    dens_base <- ggplot() +
        geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
        geom_sf(data = subset(all_dens_grid, grepl("Baseline", Variable)), aes(fill = Value, color = Value, geometry = geometry)) +
        geom_sf(data = domain, fill = NA) +
        scale_fill_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
        scale_color_viridis_c(option = "viridis", na.value = "transparent", trans = "log10") +
        coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) + 
        facet_wrap(~Variable, ncol = 3) +
        theme_map()

    dens_diff <- all_dens_grid %>%
        ungroup() %>%
        distinct() %>%
        pivot_wider(names_from = Variable, values_from = Value) %>%
        mutate(
            Percent_Change_2055 = ((Projected_2055_Mean_Dens - Baseline_Mean_Dens) / Baseline_Mean_Dens) * 100,
            Percent_Change_2075 = ((Projected_2075_Mean_Dens - Baseline_Mean_Dens) / Baseline_Mean_Dens) * 100
        ) %>%
          pivot_longer(., -c(Lat, Lon, geometry), names_to = "Variable", values_to = "Value")

    change_lims <- c(min(dens_diff$Value), max(dens_diff$Value))
    
    pct_change1<- ggplot() +
        geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
        geom_sf(data = subset(dens_diff, grepl("Percent_Change_2055", Variable) ), aes(fill = Value, color = Value, geometry = geometry)) +
        geom_sf(data = domain, fill = NA) +
        scale_fill_distiller(palette = "PRGn", na.value = "transparent", direction = 1, oob = scales::oob_squish, limits = change_lims) +
        scale_color_distiller(palette = "PRGn", na.value = "transparent", direction = 1, oob = scales::oob_squish, limits = change_lims) +
        facet_wrap(~Variable, ncol = 3) +
        coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) +
        theme_map()
    
    pct_change2<- ggplot() +
        geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
        geom_sf(data = subset(dens_diff, grepl("Percent_Change_2075", Variable) ), aes(fill = Value, color = Value, geometry = geometry)) +
        geom_sf(data = domain, fill = NA) +
        scale_fill_distiller(palette = "PRGn", na.value = "transparent", direction = 1, oob = scales::oob_squish, limits = change_lims) +
        scale_color_distiller(palette = "PRGn", na.value = "transparent", direction = 1, oob = scales::oob_squish, limits = change_lims) +
        facet_wrap(~Variable, ncol = 3) +
        coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) +
        theme_map()
    
    plot_out<- dens_base + pct_change1 + pct_change2 
    
    # Return it
    return(plot_out)
}

# Let's map that to each of the species...
all_proj_res <- all_proj_res %>% mutate(., "Dens_Panel_Plot" = map(Density, plot_raw_dens_func))

# How'd we do?
which(grepl("Lobster", all_proj_res$File_Path))    
all_proj_res$Dens_Panel_Plot[[16]]
all_proj_res$Dens_Panel_Plot[[4]]





# ANDREW NOTE: MIGHT WANT TO MASK OUT THE OUTER REACHES OF THE PREDICTED VALUES AS THESE ARE 
#SLIGHTLY BEYOND THE MODELING DOMAIN

###
# Cropping density to community footprints
###
foot_stack <- raster::stack(paste0(project_box_path, "data/All VTR safe fishing footprints by community and gear type 2011-2015.grd"))

# What's in here??
foot_stack

# Dimensions of raster have the number of longitudes (nrow), latitudes (ncol) and then nlayers is the number of unique community and gear type footprints. Generally, we use the "All" combined footprint, which basically kept any unique cell "fished" across the different gear types.
names(foot_stack[[1]])

# Get the stack index for names with "All"
all_ind <- which(grepl("All", names(foot_stack)))
names(foot_stack)[all_ind]

# Weirdness..
test_ind <- which(grepl("LUBEC", names(foot_stack)))
test_stack<- foot_stack[[test_ind]]

# 126 unique communities. The names are a bit of a nightmare. The easiest thing I think will be to create a table for the selected communities that maps from the community names for the landings to the community names here. I think we have something similar that does this for all the communities...so let me know if you get to that point and you are going nuts!

# Let's reduce the raster stack to just these "All" gear footprints for each of the communities
all_foot_stack<- foot_stack[[all_ind]]

# Plot one?
plot(all_foot_stack[[65]])

# Beautiful :) So, a few things...the "fill" here doesn't really make sense because we are using this "All" gear type. So, we can reclassify this so that we just have binary "fished" and "not-fished" cells.
m <- c(0, Inf, 1,  -Inf, 0, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
all_foot_stack_bin<- raster::reclassify(all_foot_stack, rclmat) 

plot(all_foot_stack_bin[[1]])

# Check that they are in the same spatial projection (gross)
ggplot() +
    geom_tile(data = as.data.frame(all_foot_stack_bin[[1]], xy = TRUE), aes(x = x, y = y)) +
    geom_point(data = all_proj_res$Density[[1]][all_proj_res$Density[[1]]$Time == as.Date("1985-03-16"),], aes(x = Lon, y = Lat), color = "red")

# Now, the overlay. There are a ton of different ways to do this as you probably remember from before. Running out of steam a bit, but happy to help out as you get here!

# This is going to amount to overlaying the spatial footprints with the density data. What about one at a time?
test_bin <- all_foot_stack_bin[[1]] 
test_poly <- rasterToPolygons(test_bin, fun = NULL, n = 4, na.rm = TRUE, digits = 12, dissolve = FALSE) %>%
    st_as_sf()
test_dens <- all_proj_res$Density[[1]] %>%
    filter(., Time == "1985-03-16") %>%
    st_as_sf(., coords = c("Lon", "Lat"), crs = st_crs(test_poly))

# t <- st_distance(test_poly, test_dens) # Distance between every polygon and each point, rows are the polygons and columns are the points...
# Get four nearest points for each "fished" cell
t2 <- st_nn(test_poly, test_dens, k = 4) # 4 nearest neighbor cells

# List to dataframe and then keep all unique
t3 <- unique(unlist(t2))

# Average values for those rows
dens_summ <- test_dens[t3, ] %>%
    summarize("Mean_Prob_0.5" = mean(Prob_0.5))

# Alright...so how can we make this into a function? We need the raster layer and the projected density, then we change the time of the projected density and thats about it.

dens_foot_summary<- function(dens, foot_stack = all_foot_stack_bin, stat = "Prob_0.5", ...){
    if (FALSE) {
        foot_stack = all_foot_stack_bin
        dens = all_proj_res[1, ]$Density[[1]]
        years = list("baseline" = c("2015", "2019"), "Future_2055" = "2055", "Future_2075" = "2075")
        stat = "Prob_0.5"
    }

    # Checks
    if(class(years) != "list"){
        stop("years should be provided as a list")    
        }
    if (all(is.numeric(years))) {
        stop("years should be character and not numeric")
    }

    out <- data.frame("Community" = gsub(".All.JGS.SAFE.PROPORTION", "", names(foot_stack)), "Years" = names(years)) %>%
        pivot_wider(., names_from = Years, values_from = Years, values_fn = function(x) as.numeric(NA))

    for(i in 1:nlayers(foot_stack)){
        # Check
        if (all(getValues(foot_stack[[i]] == 0)) | all(is.na(getValues(foot_stack[[i]])))) {
             out[i, 2:ncol(out)] <- NA
             next()
        }
        
        # Convert footprint raster layer to polygon
        foot_poly <- rasterToPolygons(foot_stack[[i]], fun = NULL, n = 4, na.rm = TRUE, digits = 12, dissolve = FALSE) %>%
            st_as_sf()

        # Get coordinates of projection grid cells -- these are the same no matter what time period
        dens_sf_unique <- dens %>%
            distinct(., "Lon", "Lat") %>%
            st_as_sf(., coords = c("Lon", "Lat"), crs = st_crs(foot_poly), remove = FALSE)         
         # Get the nearest four density cells to each of the "fished" footprint cells
        dens_near_foot <- st_nn(foot_poly, dens_sf_unique, k = 4)
        
        # Convert that to dataframe and then keep all unique density rows
        dens_rows_keep <- unlist(dens_near_foot)
        
        # Let's get the lat/longs of those...
        dens_keep <- dens_sf_unique[dens_rows_keep, ]
        
        # Now, go back to our original dens_sf and get observations that have the same lon/lat, which will also work for however many "dates" there are (i.e., three seasons in a year, or multiple seasons/years)
        dens_sf <- dens %>%
                mutate(., "Year" = format(Time, "%Y")) %>%
                st_as_sf(., coords = c("Lon", "Lat"), crs = st_crs(foot_poly), remove = FALSE)

        dens_sf_keep <- dens_keep %>%
            dplyr::select(., geometry) %>%
            st_join(., dens_sf, join = st_equals)

        # With that, we can then get the different stats for the different time ranges specified in years...
        for (j in seq_along(years)) {
            col_use <- which(colnames(out) == names(years)[j])
            if (length(years[[j]]) == 1) {
                out[i, col_use] <- dens_sf_keep %>%
                    dplyr::filter(., Year == years[[j]]) %>%
                    st_drop_geometry() %>%
                    summarize_at(., .vars = {{ stat }}, mean, na.rm = TRUE)
            } else {
                out[i, col_use] <- dens_sf_keep %>%
                    dplyr::filter(between(Year, years[[j]][1], years[[j]][2])) %>%
                    st_drop_geometry() %>%
                    summarize_at(., .vars = {{ stat }}, mean, na.rm = TRUE)
            }
        }
    }
    # Return it
     return(out)
}


# ####__####
# # Now that we have a function, 
# # going to want to have a dataframe that has...species, scenario, projected data, 
# #and then the dataframe with the average biomass for each community
# get_species_name<- function(file_path, root = paste0(projection_res_path, "/")){
#     if(FALSE){
#         file_path = all_proj_res$File_Path[[1]]
#         root = paste0(projection_res_path, "/")
#     }
# 
#     t1 <- gsub(root, "", file_path)
#     t2 <- strsplit(t1, split = "_full_")
#     
#     spp_out <- t2[[1]][1]
#     return(spp_out)
#     
# }
# 
# get_scenario_name<- function(file_path, root = paste0(projection_res_path, "/")){
#     if(FALSE){
#         file_path = all_proj_res$File_Path[[1]]
#         root = paste0(projection_res_path, "/")
#     }
# 
#     t1 <- gsub(root, "", file_path)
#     t2 <- strsplit(t1, split = "_full_")
#     
#     scen_out <- gsub(".rds", "", t2[[1]][2])
#     return(scen_out)
#     
# }
# 
# # Get these...
# tic()
# all_proj_res <- all_proj_res %>%
#     mutate(.,
#         "Species" = map_chr(File_Path, get_species_name), "Scenario" = map_chr(File_Path, get_scenario_name),
#         "Community_Overlay" = map(Density, dens_foot_summary, years = list("baseline" = c("2015", "2019"), "Future_2055" = "2055", "Future_2075" = "2075"))
#     
#     )
# toc()
# 
# # 


