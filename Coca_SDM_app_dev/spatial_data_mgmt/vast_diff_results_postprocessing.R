# Vast Results Post-Processing
# This code was removed from pre-processing script because it was an offshoot that 
# did not need to stay with the other fundamental steps

# This script takes the vast results that have been prepared to
# focus on specific points in time ex. 2050, 2100
# And further prepares them by subtracting out the baseline
# period and dropping all additional information not needed for map making


####__________________####
# Inputs from: vast_projection_preprocessing.R
# Outputs:
# here::here("Data/projections/annual_proj_wide.csv")
# here::here("Data/projections/seasonal_proj_wide.csv")

####__________________####

#### Reshape Summaries for Baseline Comparison  ####

##### Start from saved files ####

# Unique locations to map
pt_latlon <- read_csv(
  file = here::here("Data/spatial/unique_location_coords.csv"),
  col_types = cols(
    Lon = col_double(),
    Lat = col_double(),
    pt_id = col_double()
  ))

#---

# Annual Averages: discrete years
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

# Seasonal Averages: discrete years
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


#---

# Annual Averages: discrete years
milestone_annual_avg <- read_csv(
  file = here::here("Data/projections/Cmilestones_all_species_test.csv"), 
  col_types = cols(
    pt_id = col_double(),
    var = col_character(),
    val = col_double(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    species = col_character(),
    scenario = col_character()
  ))

# Seasonal Averages: discrete years
milestone_seasonal_avg <- read_csv(
  file = here::here("Data/projections/Cmilestones_all_seasons_test.csv"), 
  col_types = cols(
    Season = col_character(),
    pt_id = col_double(),
    var = col_character(),
    val = col_double(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    species = col_character(),
    scenario = col_character()
  ))




#####  Get Difference from Baseline  ####

# Get Diffs
# Standardize the difference by the BASE sd, by species



#####  Milestone Summaries   ####

# Do the annual file
milestone_annual_wide <- milestone_annual_avg %>% 
  split(.$species) %>% 
  map_dfr(function(spec_i){
    # Prepare the wide dataframe
    spec_i %>% 
      dplyr::select(-temp_horizon) %>% 
      mutate(var_type = case_when(
        str_detect(var, "mu") ~ str_c("mean_", ref_period),
        str_detect(var, "sd") ~ str_c("sd_", ref_period))) %>% 
      dplyr::select(-all_of(c("ref_period", "var"))) %>% 
      pivot_wider(
        names_from = var_type,
        values_from = val) %>% 
      # Get differences 
      mutate(
        diff_0.5C   = `mean_2030-2040` - `mean_2010-2019`,
        diff_0.5C_z = diff_0.5C / `sd_2010-2019`,
        diff_1C     = `mean_2040-2050` - `mean_2010-2019`,
        diff_1C_z   = diff_1C / `sd_2010-2019`,
        diff_1.5C   = `mean_2050-2060` - `mean_2010-2019`,
        diff_1.5C_z = diff_1.5C / `sd_2010-2019`,
        diff_2C     = `mean_2060-2070` - `mean_2010-2019`,
        diff_2C_z   = diff_2C / `sd_2010-2019`,
        diff_3C     = `mean_2080-2090` - `mean_2010-2019`,
        diff_3C_z   = diff_3C / `sd_2010-2019`,
        diff_4C     = `mean_2090-2100` - `mean_2010-2019`,
        diff_4C_z   = diff_4C / `sd_2010-2019`,)
    
  }) 

# Trim projection columns
milestone_annual_wide <- 
  milestone_annual_wide %>% dplyr::select(-c(
    `mean_2030-2040`, `sd_2030-2040`,
    `mean_2040-2050`, `sd_2040-2050`,
    `mean_2050-2060`, `sd_2050-2060`,
    `mean_2060-2070`, `sd_2060-2070`,
    `mean_2080-2090`, `sd_2080-2090`,
    `mean_2090-2100`, `sd_2090-2100`
  ))



# Do the seasonal file
milestone_seas_wide <- milestone_seasonal_avg %>% 
  split(.$species) %>% 
  map_dfr(function(spec_i){
    # Prepare the wide dataframe
    spec_i %>% 
      dplyr::select(-temp_horizon) %>% 
      mutate(var_type = case_when(
        str_detect(var, "mu") ~ str_c("mean_", ref_period),
        str_detect(var, "sd") ~ str_c("sd_", ref_period))) %>% 
      dplyr::select(-all_of(c("ref_period", "var"))) %>% 
      pivot_wider(
        names_from = var_type,
        values_from = val) %>% 
      # Get differences 
      mutate(
        diff_0.5C   = `mean_2030-2040` - `mean_2010-2019`,
        diff_0.5C_z = diff_0.5C / `sd_2010-2019`,
        diff_1C     = `mean_2040-2050` - `mean_2010-2019`,
        diff_1C_z   = diff_1C / `sd_2010-2019`,
        diff_1.5C   = `mean_2050-2060` - `mean_2010-2019`,
        diff_1.5C_z = diff_1.5C / `sd_2010-2019`,
        diff_2C     = `mean_2060-2070` - `mean_2010-2019`,
        diff_2C_z   = diff_2C / `sd_2010-2019`,
        diff_3C     = `mean_2080-2090` - `mean_2010-2019`,
        diff_3C_z   = diff_3C / `sd_2010-2019`,
        diff_4C     = `mean_2090-2100` - `mean_2010-2019`,
        diff_4C_z   = diff_4C / `sd_2010-2019`,)
    
  }) 

# Trim projection columns
milestone_seas_wide <- 
  milestone_seas_wide %>% dplyr::select(-c(
    `mean_2030-2040`, `sd_2030-2040`,
    `mean_2040-2050`, `sd_2040-2050`,
    `mean_2050-2060`, `sd_2050-2060`,
    `mean_2060-2070`, `sd_2060-2070`,
    `mean_2080-2090`, `sd_2080-2090`,
    `mean_2090-2100`, `sd_2090-2100`
  ))



#####  Specific Year Summaries   ####



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
clean_season_wide <- seasonal_wide %>% 
  select(pt_id, species, scenario, season = Season, `mean_2010-2019`, diff_2050, diff_2050_z, diff_2100, diff_2100_z)




####  Export
write_csv(clean_annual_wide, here::here("Data/projections/annual_proj_wide.csv"))
write_csv(clean_season_wide, here::here("Data/projections/seasonal_proj_wide.csv"))