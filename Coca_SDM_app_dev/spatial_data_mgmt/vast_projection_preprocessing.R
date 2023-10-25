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



####____________####
####____________####
####  Projections Folder  ####



# Paths to Box Assets 
mills_path <- cs_path(box_group = "mills")
project_box_path <- str_c(mills_path, "Projects/COCA19_Projections/")
projection_res_path <- paste0(project_box_path, "projections/")



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
fpaths <- list.files(projection_res_path, pattern = "_mean.rds", full.names = TRUE)
fnames <- str_remove(list.files(projection_res_path, pattern = "_mean.rds", full.names = FALSE, ), ".rds")
proj_files <- setNames(fpaths, fnames)



# Read each dataset into a list
proj_data <- map(proj_files, read_rds_func)

# Check one of these objects to see its structure
names(proj_data$Butterfish_full_CMIP6_SSP1_26_mean)









#### 2. Isolate the Biomass Density Information ####

# For now, we are mostly going to be using the results in the "Dens" object to make our maps 
# and then also for cropping with community footprints to get our summaries of change. 
# Let's pull out the density results as a new column

 # Isolate just the density information
density_estimates <- proj_data %>% map(~pluck(.x, "Dens"))


# What does that get us? # Grouped Dataframe
test <- density_estimates[[1]]
str(test)








#### 3. Calculate a Rolling Mean of Predicted Density  ####

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











#### 4.  Assign/Verify Location ID's  ####

# Do the base coordinates differ from the projection coordinates? Yes
# Are the lat and Lon values the same across projections? Yes!
ssp1_pts <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% distinct(Lon, Lat)
ssp5_pts <- rolling_dens$Butterfish_full_CMIP6_SSP5_85_mean %>% distinct(Lon, Lat)
sum(ssp1_pts != ssp5_pts) # We good!


# Get the locations as their own thing
unique_pts <- rolling_dens$Butterfish_full_CMIP6_SSP1_26_mean %>% 
  distinct(Lon, Lat) %>% 
  mutate(pt_id = row_number())

# Pair the pt_id back into the main dataset
rolling_dens <- map(rolling_dens, ~left_join(.x, unique_pts, by = join_by(Lat, Lon)))

# ^ This is the key to matching data to the correct spatial feature
# This is a key for lat/lon positioning that matches to rolling_dens
unique_pts %>% ggplot(aes(Lon, Lat, color = pt_id)) + geom_point() + theme_void()

##### Save Unique Locations  #### 
# write_csv(unique_pts, here::here("Data/spatial/unique_location_coords.csv"))






####____________####
####____________####

####  Summarize Density Information  ####


# Goals here:

# 1. Reshape + Organize for the bare min needed to plot


# How many species do we have...

# 31 species
length(rolling_dens)/2 

# 2 SSP scenarios


# 657 locations
nrow(unique_pts) 

# 3 Seasons + annual average (do we need all?)
# Display periods:







####  Summarization Steps  ####




##### 1. Trim to a defined area  ####


# Study area outline shapefile
domain_use <- st_read(str_c(cs_path("mills", "Projects/sdm_workflow/data 2/supporting/region_shapefile"), "full_survey_region.shp"))
ggplot() + geom_sf(data = domain_use) + ggtitle("Study Area") + theme_bw()


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
dens_sf_test <- unique_pts %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F) 
dens_sf_test %>% st_transform(crs = 32619) %>% ggplot() + geom_sf()

# Points inside study area:
unique_pts_within <- st_join(x = dens_sf_test, y = st_make_valid(domain_use))

# st_join is Wayyyy faster than st_within
unique_pts_within %>% 
  ggplot() +
  geom_sf(data = domain_use) +
  geom_sf()




####_________________####
####  PREPARING SUMMARIES  ####




##### 2.   Summarize for Climate Change Thresholds  ####


# Get Average Conditions During Decades when Regional SST crosses milestones

# a. Subset to study area again to be sure
# b. Get mean over a period for baseline
# c. Get mean over specific decades for SSP5
# d. Get variance for standardizing?


# milestone framing of 0.5 to 4C of warming
# For Joint Survey Area:
# 0.5  2030-2040
# 1    2040-2050
# 1.5  2050-2060
# 2    2060-2070
# 3    2080-2090
# 4    2090-2100


# Milestone Update



# Spatial overlay option
sf_use_s2(use_s2)


# Function to perform it on one dataset
temp_horizon_summarize <- function(
    density_in,        # Data to process
    spatial_domain,    # sf for domain
    unique_pts,        # subset of distinct coordinates
    baseline_years = seq(from = 2010, to = 2019), 
    horizons,          # DF with decades to summarize
    # For getting a seasonal average:
    run_seasons = FALSE ){      
  
  
  # Controls:
  # Set whether to care about seasons
  if(run_seasons == TRUE){group_by_lvls <- c("Season", "pt_id")} 
  if(run_seasons == FALSE){group_by_lvls <- c("pt_id")} 

  
  # Flag the locations that are within the domain
  pts_keep <- unique_pts %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F) %>% 
    st_join(spatial_domain, join = st_within) %>%
    st_drop_geometry() %>%
    dplyr::select(-Region) 
  
  # Filter based on whether points are in domain
  dens_data <- density_in %>%
    filter(pt_id %in% pts_keep$pt_id)  %>% 
    mutate(#pt_id = as.character(pt_id),
      #pt_id = factor(pt_id),
      Year = as.numeric(Year))
  
  
  # 1. Get the Baseline Conditions for the Ref Period:
  
  # Start and end years
  min_yr <-  min(baseline_years)
  max_yr <-  max(baseline_years)
  
  # Summarize the baseline
  baseline_conditions <- dens_data %>%
    filter(between(Year, min_yr, max_yr)) %>% 
    group_by(across(all_of(group_by_lvls))) %>%
    summarise(
      "baseline_mu" = mean(Prob_0.5_Y_RM),
      "baseline_sd"  = mean(Prob_0.5_Y_Rsd),
      .groups = "drop")  %>%
    # Make it longer using those two variables: mean and sd
    pivot_longer(
      cols = all_of(c("baseline_mu", "baseline_sd")),
      names_to = "var",
      values_to = "val")  %>%
    mutate(
      ref_period = str_c(min_yr, "-", max_yr),
      temp_horizon = "0C")
  
  
  # 3. Get "average" across the time horizons
  
  # Done in map() to hit each row in the horizons table
  projected_conditions <- horizons %>% 
    split(.$horiz) %>% 
    imap_dfr(
      # Get the average across the decadal periods
      function(x, y){
        
        # Filter to those years and get mean, sd
        fut_data_temp <- dens_data %>%
          filter(
            format(Time, "%Y") %in% seq(x$min, x$max, by = 1)) %>%
          group_by(across(all_of(group_by_lvls))) %>%
          summarise(mu_name = mean(Prob_0.5_Y_RM),
                    sd_name = mean(Prob_0.5_Y_Rsd),
                    .groups = "drop")
      
        # Rename those stand-in cols above
        proj_mu <- str_c("proj_", x$horiz, "C_mu")
        proj_sd <- str_c("proj_", x$horiz, "C_sd")
        
        # Do some r-eval wizardry
        fut_data_temp <- fut_data_temp %>% 
          rename(
            !!proj_mu := mu_name,
            !!proj_sd := sd_name)
        
        # Pivot "Wide" to "long"
        horizon_period = str_c(str_c(x$min, "-", x$max))
        fut_data_temp <- fut_data_temp %>% 
          pivot_longer(
            cols = all_of(c(proj_mu, proj_sd)), 
            names_to = "var", 
            values_to = "val") %>% 
          mutate(
            ref_period = horizon_period,
            temp_horizon = str_c(y,"C"))
        
        
      }
    )
  
  

  # Bring the baseline and projection period summaries all together
  all_dens_data <- bind_rows(baseline_conditions, projected_conditions)
  return(all_dens_data)
  
}



# # TESTING:
# # Run it for A species
# temp_horizon_summarize(
#   density_in = rolling_dens$Cod_full_CMIP6_SSP5_85_mean,
#   spatial_domain = domain_use, 
#   unique_pts = unique_pts, 
#   baseline_years = c(2010, 2019), 
#   horizons = horizon_yrs, 
#   run_seasons = F)


###### SSP1 Milestones  ####

# As table
ssp1_horizons <- data.frame(
  "horiz" = c(0.5),
  "min"   = c(2048),
  "max"   = c(2052)
)


# Run it for all seasons combined: SSP5
horizon_summ_ssp1 <- imap_dfr(
  rolling_dens[str_detect(names(rolling_dens), "SSP1")], 
  function(dens_data, file_name){
    
    # Run the summary
    projection_summary <- temp_horizon_summarize(
      density_in     = dens_data, 
      spatial_domain = domain_use, 
      unique_pts     = unique_pts, 
      baseline_years = c(2010, 2019), 
      horizons       = ssp1_horizons, 
      run_seasons = F)
    
    # add species and scenario details from the list's names
    summary_out <- projection_summary %>% 
      mutate(
        file_name = file_name,
        file_name = str_replace(file_name, "_full_", "-")) %>% 
      separate(file_name, into = c("species", "scenario"), sep = "-") %>% 
      mutate(
        species = str_replace_all(species, "_", " "),
        species = tolower(species),
        scenario = str_remove(scenario, "_mean"))
    
    # return the summary
    return(summary_out)
  })


# Run it grouped on seasons
horizon_summ_szns_ssp1 <- imap_dfr(
  rolling_dens[str_detect(names(rolling_dens), "SSP1")], 
  function(dens_data, file_name){
    
    # Run the summary
    projection_summary <- temp_horizon_summarize(
      density_in     = dens_data, 
      spatial_domain = domain_use, 
      unique_pts     = unique_pts, 
      baseline_years = c(2010, 2019), 
      horizons       = ssp1_horizons, 
      run_seasons    = T)
    
    # add species and scenario details from the list's names
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




###### SSP5 Milestones  ####

# Run it just for SSp5

# As table
ssp5_horizons <- data.frame(
  "horiz" = c(0.5, 1, 1.5, 2, 3, 4),
  "min"   = c(2034, 2041, 2054, 2064, 2077, 2095),
  "max"   = c(2038, 2045, 2058, 2068, 2081, 2099)
)

# Run it for all seasons combined: SSP5
horizon_summ_ssp5 <- imap_dfr(
  rolling_dens[str_detect(names(rolling_dens), "SSP5")], 
  function(dens_data, file_name){
  
  # Run the summary
  projection_summary <- temp_horizon_summarize(
    density_in     = dens_data, 
    spatial_domain = domain_use, 
    unique_pts     = unique_pts, 
    baseline_years = c(2010, 2019), 
    horizons       = ssp5_horizons, 
    run_seasons = F)
  
  # add species and scenario details from the list's names
  summary_out <- projection_summary %>% 
    mutate(
      file_name = file_name,
      file_name = str_replace(file_name, "_full_", "-")) %>% 
    separate(file_name, into = c("species", "scenario"), sep = "-") %>% 
    mutate(
      species = str_replace_all(species, "_", " "),
      species = tolower(species),
      scenario = str_remove(scenario, "_mean"))
  
  # return the summary
  return(summary_out)
})

# Run it grouped on seasons
horizon_summ_szns_ssp5 <- imap_dfr(
  rolling_dens[str_detect(names(rolling_dens), "SSP5")], 
  function(dens_data, file_name){
  
  # Run the summary
  projection_summary <- temp_horizon_summarize(
    density_in     = dens_data, 
    spatial_domain = domain_use, 
    unique_pts     = unique_pts, 
    baseline_years = c(2010, 2019), 
    horizons       = ssp5_horizons, 
    run_seasons    = T)
  
  # add species and scenario details from the list's names
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



# Join the two scenario's horizon summaries into one file
horizons_out <- bind_rows(horizon_summ_ssp1, horizon_summ_ssp5)
horizons_out_szns <- bind_rows(horizon_summ_szns_ssp1, horizon_summ_szns_ssp5)


# Drop the models that have different features that resulted in NA's for now
horizons_out <- drop_na(horizons_out)

# Do it again for Seasons
horizons_out_szns <- drop_na(horizons_out_szns)


##### 2b.  Saving Horizon Summaries  ####

# Save the decadal milestone summaries
write_csv(horizons_out, here::here("Data/projections/Cmilestones_all_species_test.csv"))

# Save seasonal version
write_csv(horizons_out_szns, here::here("Data/projections/Cmilestones_all_seasons_test.csv"))





##### 3. Processing Comparisons at Discrete Points in Time  ####


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




###### Key Yearly Projections  ####
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



##### 3b. Save Results for 2050 & 2100  ####


#---

# Save the focal year summaries
write_csv(bare_essentials, here::here("Data/projections/all_species_test.csv"))


# Save seasonal version
write_csv(bare_essentials_sznz, here::here("Data/projections/all_seasons_test.csv"))


















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












