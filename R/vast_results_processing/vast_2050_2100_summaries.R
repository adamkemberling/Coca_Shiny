##### Temperature Horizon Summaries  ####
# Take the VAST Projections for all years
# Prepare focused summaries for periods
# at 2050 & 2100


#### Libraries  ####
library(tidyverse)
library(raster)
library(sf)
library(terra)
library(rnaturalearth)
library(gmRi)



####  Master Data  ####
unique_pts <- read_sf(here::here("Data/spatial/unique_location_coords.csv"))

# Density data
all_density_results <- read_csv(here::here("Data/projections/VAST_all_densities_all_species.csv"))
rolling_dens <- all_density_results %>% split(.$VAST_id) 



####  Process Summaries  ####




##### Processing Comparisons at Discrete Points in Time  ####

# This sub-routine was done to get the baseline values contrasted 
# against 2050 and 2100 values


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
    use_s2 = FALSE ){      
  
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



#####  Save Results for 2050 & 2100  ####


#---

# Save the focal year summaries
write_csv(bare_essentials, here::here("Data/projections/all_species_test.csv"))


# Save seasonal version
write_csv(bare_essentials_sznz, here::here("Data/projections/all_seasons_test.csv"))


