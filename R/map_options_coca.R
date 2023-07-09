# Testing Map  Display options for COCA Data
# More species than the fishviz displays, only maps



####  Packages  ####
library(here)
library(sf)
library(tmap)
library(terra)
library(tidyterra)
library(profvis)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(patchwork)
library(glue)
library(rcartocolor)


# Lets turn some fonts on to match the theme:
library(showtext)
font_add_google("Raleway", "raleway")
showtext_auto()


#### Spatial Polygons:  ####


# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))

# Bathymetry
# Add the bottom contours:
# Or make terra for ggplot using tidyterra
# bathy <- terra::rast("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")
bathy <- terra::rast(here::here("Data/spatial/etopo_bedrock_15arcsec.tiff"))



####  Theme Controls  ####
# Plotting map theme
theme_map <- function(guides = T, ...){
  list(
    # Theme options, with ellipse to add more
    theme(
      # Font across all text
      text = element_text(family = "raleway"),
      
      # Titles
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      
      # Grids and Axes
      panel.background = element_blank(), 
      panel.border = element_rect(color = "black", fill = "transparent"), 
      panel.grid.major = element_line(color = "gray80"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks=element_blank(),
      plot.margin = margin(t = 10, r = 2, b = 0.1, l = 2, unit = "pt"),
      legend.position = c(.725, .125), 
      legend.background = element_rect(color = "transparent", fill = "white", 
                                       size = 0.25),
      
      # Use ellipses for tweaks on the fly:
      ...))
}



# Divergent log10 transformation
# Make a custom transformation that handles log10 transformation in positive and negative values
divergent_l10_trans <- scales::trans_new(
  name = "signed_log",
  transform=function(x) sign(x)*log10(abs(x)),
  inverse=function(x) sign(x)*10^abs(x))




####  Projection Data Preparation  ####


# Unique locations to map
pt_latlon <- read_csv(
  file = here::here("Data/spatial/unique_location_coords.csv"),
  col_types = cols(
    Lon = col_double(),
    Lat = col_double(),
    pt_id = col_double()
  ))

# # Annual Averages Long
# annual_avg <- read_csv(
#   file = here::here("Data/projections/all_species_test.csv"), 
#   col_types = cols(
#     pt_id = col_double(),
#     var = col_character(),
#     val = col_double(),
#     ref_period = col_character(),
#     species = col_character(),
#     scenario = col_character()
#   ))
# 
# # Seasonal Averages Long
# seasonal_avg <- read_csv(
#   file = here::here("Data/projections/all_seasons_test.csv"), 
#   col_types = cols(
#     Season = col_character(),
#     pt_id = col_double(),
#     var = col_character(),
#     val = col_double(),
#     ref_period = col_character(),
#     species = col_character(),
#     scenario = col_character()
#   ))


# Annual Averages and the Baseline Difference
clean_annual_wide <- read_csv(
  file = here::here("Data/projections/annual_proj_wide.csv"),
  col_types = cols(
    pt_id = col_double(),
    species = col_character(),
    scenario = col_character(),
    `mean_2010-2019` = col_double(),
    diff_2050 = col_double(),
    diff_2050_z = col_double(),
    diff_2100 = col_double(),
    diff_2100_z = col_double()
  ))

# Seasonal Average and the Baseline Difference
clean_season_wide <- read_csv(
  file = here::here("Data/projections/seasonal_proj_wide.csv"),
  col_types = cols(
    pt_id = col_double(),
    species = col_character(),
    scenario = col_character(),
    season = col_character(),
    `mean_2010-2019` = col_double(),
    diff_2050 = col_double(),
    diff_2050_z = col_double(),
    diff_2100 = col_double(),
    diff_2100_z = col_double()
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






####__________________####
####__________________####
####  Maps  ####




####  User Selections  ####
# Do the filtering a user would do:


# These match the two user selection controls on the App
species_choice <- "cod"
decade_choice <-  "2050"

# Filter
annual_species_i <- clean_annual_wide %>% split(.$species) %>% pluck(species_choice)
seasonal_species_i <- clean_season_wide %>% split(.$species) %>% pluck(species_choice)


# At this point we can join back in the lat/lon or the grid
annual_i <- left_join(annual_species_i, hex_grid, by = join_by(pt_id))
seasonal_i <- left_join(seasonal_species_i, hex_grid, by = join_by(pt_id))
ssp1_i <- filter(annual_i, scenario == "CMIP6_SSP1_26") %>% st_as_sf()
ssp5_i <- filter(annual_i, scenario != "CMIP6_SSP1_26") %>% st_as_sf()







####__ A. Baseline Maps  ####

# There's two soo...
# Split scenarios-

# Sequential Color Maps
rcartocolor::display_carto_all(colorblind_friendly = T)



####__ B. SSP1 Difference Maps  ####

# First Scenario
(base_ssp1 <- ggplot() +
   geom_sf(data = ssp1_i, aes(fill = `mean_2010-2019`)) +
   geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
   geom_sf(data = hague_sf, color = "black", linewidth = 1) +
   scale_fill_carto_c(palette = "ag_GrnYl", trans = "log10", 
                      labels = scales::comma_format(accuracy = 1, suffix = " kg"),
                      direction = -1) +
   coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
   labs(
     title = str_c("Atlantic ", str_to_title(species_choice), ": Baseline Period Biomass Density"),
     subtitle = "Shared Socio-Economic Pathway: SSP 1-2.6\nYears: 2010-2019") +
   theme_map() +
   guides(
     fill = guide_colorsteps(
       title = "Average Annual Biomass Density",
       title.position = "top",
       title.hjust = 0.5, 
       barwidth = unit(5, "in"),
       barheight = unit(0.6, "cm"),  #show.limits = T,
       direction = "horizontal",
       frame.colour = "black", 
       ticks.colour = "black")) +
   theme(legend.position = "bottom"))




####  Limit Setting  ####

# limits need to match against something
# Round to next power of 10
roundup_tens <- function(x) 10^ceiling(log10(x))
# What is the highest 10^x difference observed
max_diff <- roundup_tens(max(abs(annual_i$diff_2050)))
# What is the max exponent for limits in base 10 terms
max_exp <- log10(max_diff)
# limits to divergent scale
shared_lims <- max_diff * c(-1, 1)

# Breaks for divergent 10^x scales
shared_breaks <- c(
  -1*10^rev(seq(1,max_exp)), 
  0, 
  10^seq(1,max_exp)
  ) 




# Plot
(diff_ssp1 <- ggplot() +
   geom_sf(data = ssp1_i, aes(fill = diff_2050)) +
   geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
   geom_sf(data = hague_sf, color = "black", linewidth = 1) +
   scale_fill_carto_c(palette = "Geyser", 
                      trans = divergent_l10_trans,
                      labels = scales::comma_format(accuracy = 1, suffix = " kg"),
                      breaks = shared_breaks,
                      limits = shared_lims,
                      direction = -1) +
   coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
   labs(
     title = str_c("Atlantic ", str_to_title(species_choice), ": Change in Biomass Density at ", decade_choice),
     subtitle = str_c("Shared Socio-Economic Pathway: SSP 1-2.6\nProjection Year: ", decade_choice) )+
   theme_map() +
   guides(
     fill = guide_colorsteps(
       title = "Difference from Baseline Period",
       title.position = "top",
       title.hjust = 0.5, 
       barwidth = unit(5, "in"),
       barheight = unit(0.6, "cm"),  #show.limits = T,
       direction = "horizontal",
       frame.colour = "black", 
       ticks.colour = "black")) +
   theme(#legend.background = element_rect(color = "black"),
         legend.position = "bottom",
         text = element_text(family = "raleway")))


####__ C. SSP5 Difference Maps  ####




# Second Scenario
(base_ssp5 <- ggplot() +
   geom_sf(data = ssp5_i, aes(fill = `mean_2010-2019`)) +
   geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
   geom_sf(data = hague_sf, color = "black", linewidth = 1) +
   scale_fill_carto_c(palette = "ag_GrnYl", trans = "log10", 
                      #labels = scales::label_log(base = 10), 
                      labels = scales::comma_format(accuracy = 1, suffix = " kg"),
                      direction = -1) +
   coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
   labs(
     title = str_c("Atlantic ", str_to_title(species_choice), ": Baseline Period Biomass Density"),
     subtitle = "Shared Socio-Economic Pathway: SSP 5-8.5\nYears: 2010-2019") +
   theme_map() +
   guides(
     fill = guide_colorsteps(
       title = "Average Annual Biomass Density",
       title.position = "top",
       title.hjust = 0.5, 
       barwidth = unit(5, "in"),
       barheight = unit(0.6, "cm"),  #show.limits = T,
       direction = "horizontal",
       frame.colour = "black", 
       ticks.colour = "black")) +
   theme(legend.position = "bottom"))



# Do z scale for this one
z_lims <- ceiling(max(abs(annual_i$diff_2050_z))) * c(-1, 1)
z_breaks <- seq(-3, 3, 1)
(diff_ssp5 <- ggplot() +
   geom_sf(data = ssp5_i, aes(fill = diff_2050_z)) +
   geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
   geom_sf(data = hague_sf, color = "black", linewidth = 1) +
   scale_fill_carto_c(palette = "Geyser", 
                      breaks = z_breaks,
                      limits = z_lims,
                      direction = -1) +
   coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
   labs(
     title = str_c("Atlantic ", str_to_title(species_choice), ": Change in Biomass Density at ", decade_choice),
     subtitle = "Shared Socio-Economic Pathway: SSP 5-8.5\nProjection Year: ", decade_choice) +
   theme_map() +
   guides(
     fill = guide_colorsteps(
       title = "Change from Baseline Period\n(z-standardized)",
       title.position = "top",
       title.hjust = 0.5, 
       barwidth = unit(5, "in"),
       barheight = unit(0.6, "cm"),  #show.limits = T,
       direction = "horizontal",
       frame.colour = "black", 
       ticks.colour = "black")) +
   theme(legend.position = "bottom"))



####  Whole Ensemble  ####



# Baseline + Difference
base_ssp1 | diff_ssp1
base_ssp5 | diff_ssp5














#####  Sandbox Code  ####



# ####  General Organization  ####
# 
# # Once a species is chosen we can split into parts:
# # 1. baseline
# # 2. projection for SSP1
# # 3. projection for SSP5
# 
# # These will be displayed as "change from baseline"
# 
# # 1. Isolate species
# spec_i <- filter(
#   annual_avg,
#   species == species_choice)
# 
# 
# 
# # # 2. Isolate baseline
# # base_i <- filter(
# #   spec_i,
# #   ref_period == "2010-2019")
# 
# 
# 
# 
# # 3. Get Diffs
# # means
# # This actually has most of it in one place, might be the way to go
# spec_i %>% 
#   filter(!str_detect(var, "sd")) %>% 
#   select(-var) %>% 
#   pivot_wider(
#     names_from = "ref_period",
#     values_from = "val"
#   ) %>% 
#   mutate(
#     diff_mu_2050 = `2050`-`2010-2019`,
#     diff_mu_2050 = `2100`-`2010-2019`
#   )
# 
# 
# # Can do some preamble to keep sd along
# # Standardize the difference by the BASE sd
# messy_wide <- spec_i %>% 
#   mutate(var_type = case_when(
#     str_detect(var, "mu") ~ str_c("mean_", ref_period),
#     str_detect(var, "sd") ~ str_c("sd_", ref_period))) %>% 
#   select(-all_of(c("ref_period", "var"))) %>% 
#   pivot_wider(
#     names_from = var_type,
#     values_from = val
#   ) %>% 
#   # Get differences 
#   mutate(
#     diff_2050   = `mean_2050` - `mean_2010-2019`,
#     diff_2050_z = diff_2050 / `sd_2010-2019`,
#     diff_2100   = `mean_2100` - `mean_2010-2019`,
#     diff_2100_z = diff_2100 / `sd_2010-2019`)
# 
# 
# 
# # Look at the carnage:
# glimpse(messy_wide)
# 
# 
# 
# # NOTES:
# # Two copies of the same information for baseline, can drop that here if we have it isolated
# # JUST kidding, its different for each of them because baseline epriod is different
# # Also don't need variance on projections if we are only mapping the differences\
# 
# wide_slim <- messy_wide %>% 
#   select(pt_id, species, scenario, `mean_2010-2019`, diff_2050, diff_2050_z, diff_2100, diff_2100_z)


