####  Vast Species x EPU Timeseries  ####

# For each species, there will be a trajectory of biomass change through time
# Which can be further summarized within EPU's and even further by season
# if we hate our lives enough



#### Libraries  ####
library(raster)
library(sf)
library(terra)
library(rnaturalearth)
library(gmRi)
library(tidyverse)


# Turn off s2 for spatial overlay
sf_use_s2(FALSE)


####  Master Data  ####
unique_pts <- read_sf(here::here("COCA_SDM_app_dev/dev/scratch_data", "unique_location_coords.csv"))


# Density data for all species - after 5-year rolling avg
all_density_results <- read_csv(here::here("COCA_SDM_app_dev/dev", "projections/VAST_all_densities_all_species.csv"))

# Density data for baseline period - no 5-year roll done
all_baseline_periods <- read_csv(here::here("COCA_SDM_app_dev/dev", "projections/VAST_baseline_2010to2019_densities_all_species.csv"))


####  Regional Boundaries  ####
res_path <- cs_path("res")


# # 1. Gulf of Maine
# trawl_regions <- get_timeseries_paths("nmfs_trawl_regions", box_location = "cloudstorage")
# trawl_gom <- read_sf(trawl_regions$gulf_of_maine$shape_path)
# 
# # 2. The Full survey area we use
# # Load all the strata and just filter out the crap ones
# trawl_full <- read_sf(trawl_regions$inuse_strata$shape_path)


# # 3. DFO Survey Area
# dfo_path <- cs_path(box_group = "mills", subfolder = "Projects/DFO_survey_data/strata_shapefiles")
# dfo_area <- read_sf(str_c(dfo_path, "MaritimesRegionEcosystemAssessmentBoundary.shp"))


# 4. Ecological Production Units
epu_sf  <- ecodata::epu_sf
gom_epu <- epu_sf %>% filter(EPU == "GOM")
gb_epu  <- epu_sf %>% filter(EPU == "GB")
ss_epu  <- epu_sf %>% filter(EPU == "SS")
mab_epu <- epu_sf %>% filter(EPU == "MAB")


# # 5. Entire VAST US+DFO Area
# trawl_crsbnd <- read_sf(str_c(dfo_path, "DFO_NMFS_CRSBND_area.geojson"))


# 6. Statistical Zones
stat_zones <- read_sf(str_c(res_path, "Shapefiles/Statistical_Areas/Statistical_Areas_2010_withNames.shp"))

# Restrict them to ones were using
our_zones <- c(511,512,513,514,521,537,538,539,612,613,614,615,616,621,622,625,626,631)
stat_zones <- stat_zones %>% filter(Id %in% our_zones)

# Label the groups we are thinking about clustering
# Split them into a list
zones <- stat_zones %>% 
  mutate(
    comm_zones = case_when(
      Id %in% c(511) ~ "Downeast Maine",
      Id %in% c(512) ~ "Midcoast Maine",
      Id %in% c(513) ~ "Southern Maine",
      Id %in% c(514) ~ "Northern MA",
      Id %in% c(521) ~ "Cape Cod",
      Id %in% c(537:539) ~ "Southern MA / RI",
      Id %in% c(612:616) ~ "NY / NJ",
      Id %in% c(621,622, 625, 626, 631) ~ "NC / VA / DE",
      TRUE ~ NA)) %>% 
  drop_na(comm_zones) %>% 
  split(.$comm_zones)







####  Overlay Function  ####


# 1. Define a function that determines what shapefile the points fall in
# To speed things up we can use the unique point ID's to filter
# Only need to perform this once for each area to know which are within each region:


# input 1: Unique locations as sf
unique_pts_sf <- unique_pts %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F)

# # input 2: shapefile to check
# shape_in_test <- gom_epu



#### a. Filter Density Output Based on Regional Overlap  ####


# Make a function to do it a bunch
filter_within <- function(unique_pts_sf, shape_in, region_title, plot_check = FALSE){
  
  domain_use <- st_make_valid(shape_in) %>% st_transform(st_crs(unique_pts_sf))
  
  # Overlay Points with study area:
  # Use drop_na to drop points that picked up no informaiton in join
  unique_pts_within <- st_join(x = unique_pts_sf, y = domain_use) %>% drop_na()
  
  # Verify the deed is done:
  if(plot_check){
    p <- unique_pts_within %>%
      ggplot() +
      geom_sf(data = domain_use) +
      geom_sf() +
      labs(title = region_title)
    return(p)
  }
  
  
  # Now we can use those points to filter and summarise
  # Can then average whatever is within
  # Already comes in as year month and season
  region_dat <- all_density_results %>% 
    filter(pt_id %in% unique_pts_within$pt_id) %>% 
    mutate(region = region_title) %>% 
    group_by(VAST_id, region, Year, Month, Season) %>% 
    summarise(
      across(.cols = c("Prob_0.1", "Prob_0.5", "Prob_0.9"), 
             .fns = ~mean(.x, na.rm = T), 
             .names = "mean_{.col}"),
              .groups = "drop") 
  
  # add species and scenario details from the list's names
  region_dat <- region_dat %>% 
    mutate(
      VAST_id = str_replace(VAST_id, "_full_", "-")) %>% 
    separate(VAST_id, into = c("species", "scenario"), sep = "-") %>% 
    mutate(
      species = str_replace_all(species, "_", " "),
      species = tolower(species),
      scenario = str_remove(scenario, "_mean"))
  return(region_dat)
  
}





#### b. Baseline Periods Average Overlays  ####

# Change the functiona little to just use the baseline densities
filter_within_baselines <- function(unique_pts_sf, shape_in, region_title, plot_check = FALSE){
  
  domain_use <- st_make_valid(shape_in) %>% st_transform(st_crs(unique_pts_sf))
  
  # Overlay Points with study area:
  # Use drop_na to drop points that picked up no informaiton in join
  unique_pts_within <- st_join(x = unique_pts_sf, y = domain_use) %>% drop_na()
  
  # Verify the deed is done:
  if(plot_check){
    p <- unique_pts_within %>%
      ggplot() +
      geom_sf(data = domain_use) +
      geom_sf() +
      labs(title = region_title)
    return(p)
  }
  
  
  # Now we can use those points to filter and summarise
  # Can then average whatever is within
  # Already comes in as year month and season
  region_dat <- all_baseline_periods %>% 
    filter(pt_id %in% unique_pts_within$pt_id) %>% 
    mutate(region = region_title) %>% 
    group_by(VAST_id, region, Year, Month, Season) %>% 
    summarise(
      across(.cols = c("Prob_0.1", "Prob_0.5", "Prob_0.9"), 
             .fns = ~mean(.x, na.rm = T), 
             .names = "mean_{.col}"),
      .groups = "drop") 
  
  # add species and scenario details from the list's names
  region_dat <- region_dat %>% 
    mutate(
      VAST_id = str_replace(VAST_id, "_full_", "-")) %>% 
    separate(VAST_id, into = c("species", "scenario"), sep = "-") %>% 
    mutate(
      species = str_replace_all(species, "_", " "),
      species = tolower(species),
      scenario = str_remove(scenario, "_mean"))
  return(region_dat)
  
}





#### c.  Proof of Concept:  ####
# Test one area
filter_within(unique_pts_sf = unique_pts_sf, shape_in = gom_epu, region_title = "GOM_epu", plot_check = T)

# Check it as a timeline
gom_dens <- filter_within(unique_pts_sf = unique_pts_sf, shape_in = gom_epu, region_title = "GOM_epu")
gom_dens %>% 
  filter(species == "lobster",
         scenario == "CMIP6_SSP5_85") %>% 
  ggplot(aes(Year, mean_Prob_0.5, color = Season)) +
  geom_line()


####  Process List of Regions ####



# For consistency sake, match the names from timeseries for the environmental conditions:
# Those are in the sdm_workflow repository: 
# sdm_workflow/CMIP6_processing/R/helper01_regional_timeseries_construction.R

# Repeat for all the regions: epu
region_list <- list(
  # EPUS
  "GOM_epu" = gom_epu,
  "GB_epu"  = gb_epu,
  "SS_epu"  = ss_epu,
  "MAB_epu" = mab_epu,
  
  # Statistical Zones
  "Downeast Maine" = zones$`Downeast Maine`,
  "Midcoast Maine" = zones$`Midcoast Maine`,
  "Southern Maine" = zones$`Southern Maine`,
  "Northern MA" = zones$`Northern MA`,
  "Cape Cod" = zones$`Cape Cod`,
  "Southern MA / RI" = zones$`Southern MA / RI`,
  "NY / NJ" = zones$`NY / NJ`,
  "NC / VA / DE" = zones$`NC / VA / DE`
  
  # # Large Survey areas
  # "Gulf of Maine"          = trawl_gom,
  # "US Survey Area"         = trawl_full,
  # "Canadian Survey Area"   = dfo_area,
  # "combined_surveys"       = trawl_crsbnd
)



# Run the regional summaries, pray that drop_na isnt an issue
all_regional_densities <- imap_dfr(
  region_list, ~filter_within(unique_pts_sf = unique_pts_sf, shape_in = .x, region_title = .y))



# Check it as a timeline
all_regional_densities %>% 
  filter(species == "cod",
         #scenario == "CMIP6_SSP5_85",
         region == "NY / NJ") %>% 
  group_by(Year, scenario) %>% 
  summarise(proj_dens = mean(mean_Prob_0.5, na.rm = T)) %>% 
  ggplot(aes(Year, proj_dens, color = scenario), linewidth = 1) +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2100)) +
  labs(y = "Projected Average Annual Density")



# Save these somewhere:
write_csv(all_regional_densities, here::here("COCA_SDM_app_dev/dev", "projections/annual_regional_species_projections.csv"))







####_________________####

#### VTR Footprint Summaries  ####


##### Community Fishing Footprints  ####

# Load the footprints
fprints <- read_rds(here::here("COCA_SDM_app_dev/dev", "scratch_data/Community_Footprints.RDS"))


# Format as sf
fprints <- fprints[["polygons"]] %>% setNames(fprints$names)
fprints <- imap(fprints, ~st_as_sf(.x) %>% 
                  st_union() %>% 
                  st_as_sf() %>%  
                  mutate(community_id = .y) %>% 
                  rename(geometry = x))

# Check one area
filter_within(unique_pts_sf = unique_pts_sf, shape_in = fprints$`PORTLAND, ME`, region_title = "Portland Maine", plot_check = T)
filter_within(unique_pts_sf = unique_pts_sf, shape_in = fprints$`STONINGTON, ME`, region_title = "Stonington Maine", plot_check = T)








# Run the regional summaries, pray that drop_na isnt an issue
community_footprint_densities <- imap_dfr(
  fprints, ~filter_within(unique_pts_sf = unique_pts_sf, shape_in = .x, region_title = .y))

# Run it for baseline conditions
community_baseline_densities <- imap_dfr(
  fprints, ~filter_within_baselines(unique_pts_sf = unique_pts_sf, shape_in = .x, region_title = .y))




# Save the files


# Save these somewhere:
write_csv(community_footprint_densities, here::here("COCA_SDM_app_dev/dev", "projections/annual_community_footprint_projections.csv"))
write_csv(community_baseline_densities, here::here("COCA_SDM_app_dev/dev", "projections/baseline_community_footprint_projections.csv"))




# Check it as a timeline
region_option <- "PORTLAND, ME"
region_option <- "STONINGTON, ME"
spec_option <- "lobster"

# Community Predictions
community_footprint_densities %>% 
  filter(species == spec_option,
         #scenario == "CMIP6_SSP5_85",
         region == region_option) %>% 
  group_by(Year, scenario) %>% 
  summarise(proj_dens = mean(mean_Prob_0.5, na.rm = T)) %>% 
  ggplot(aes(Year, proj_dens, color = scenario), linewidth = 1) +
  geom_line() +
  scale_x_continuous(limits = c(2019, 2100)) +
  labs(y = "Projected Average Annual Density", title = region_option, subtitle = spec_option)


