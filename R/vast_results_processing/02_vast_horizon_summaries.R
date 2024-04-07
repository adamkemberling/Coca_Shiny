##### Temperature Horizon Summaries  ####
# Take the VAST Projections for all years
# Prepare focused summaries for periods
# at which temp crosses 1, 2, 3, 4C



#### Libraries  ####
library(tidyverse)
library(raster)
library(sf)
library(terra)
library(rnaturalearth)
library(gmRi)

# Turn off s2 for spatial overlay
sf_use_s2(FALSE)


####  Master Data  ####
unique_pts <- read_sf(here::here("Data/spatial/unique_location_coords.csv"))

# Density data
all_density_results <- read_csv(here::here("Data/projections/VAST_all_densities_all_species.csv"))
rolling_dens <- all_density_results %>% split(.$VAST_id) 


# Study area outline shapefile
domain_use <- st_read(str_c(cs_path("mills", "Projects/sdm_workflow/data 2/supporting/region_shapefile"), "full_survey_region.shp"))





####  Process Summaries  ####



##### Summarize for Climate Change Thresholds  ####


# Get Average Conditions During Decades when Regional SST crosses milestones

# a. Subset to study area again to be sure
# b. Get mean over a period for baseline
# c. Get mean over specific decades for SSP5
# d. Get variance for standardizing?



# For Joint Survey Area:
# milestone framing of 0.5 to 4C of warming


# SSP1 Milestones as table
ssp1_horizons <- data.frame(
  "horiz" = c(0.5),
  "min"   = c(2048),
  "max"   = c(2052)
)


# As table
ssp5_horizons <- data.frame(
  "horiz" = c(0.5, 1, 1.5, 2, 3, 4),
  "min"   = c(2034, 2041, 2054, 2064, 2077, 2095),
  "max"   = c(2038, 2045, 2058, 2068, 2081, 2099)
)


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



####  Rejoin the Scenarios  ####


# Join the two scenario's horizon summaries into one file
horizons_out <- bind_rows(horizon_summ_ssp1, horizon_summ_ssp5)
horizons_out_szns <- bind_rows(horizon_summ_szns_ssp1, horizon_summ_szns_ssp5)


# Drop the models that have different features that resulted in NA's for now
horizons_out <- drop_na(horizons_out)

# Do it again for Seasons
horizons_out_szns <- drop_na(horizons_out_szns)







#####  Saving Horizon Summaries  ####

# Save the decadal milestone summaries
write_csv(horizons_out, here::here("Data/projections/Cmilestones_all_species_test.csv"))

# Save seasonal version
write_csv(horizons_out_szns, here::here("Data/projections/Cmilestones_all_seasons_test.csv"))


