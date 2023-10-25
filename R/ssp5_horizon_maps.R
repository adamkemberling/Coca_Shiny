####  Maps for Threshold Displays
# These are all SSP5, but at different temperature horizons:



library(here)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(rcartocolor)
library(patchwork)
library(glue)
library(rcartocolor)
library(showtext)
library(patchwork)

# Unicode for degrees
deg_sym <- "\u00b0" 
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"


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



# Divergent log10 transformation - Handles log10 for +- values
divergent_l10_trans <- scales::trans_new(
  name = "signed_log",
  transform = function(x) sign(x)*log10(abs(x)),
  inverse = function(x) sign(x)*10^abs(x))



# Lets turn some fonts on to match the theme:

# Path to the directory containing the font file (replace with your actual path)
font_dir <- paste0(system.file("stylesheets", package = "gmRi"), "/GMRI_fonts/Avenir/")
font_add(
  family = "Avenir",
  file.path(font_dir, "LTe50342.ttf"),
  bold = file.path(font_dir, "LTe50340.ttf"),
  italic = file.path(font_dir, "LTe50343.ttf"),
  bolditalic = file.path(font_dir, "LTe50347.ttf"))
showtext_auto()





#####  Distribution Data  ####

# Distributions at Temperature Milestones

# Load the decadal milestone summaries
yrly_dist <- read_csv(
  here::here("Data/projections/Cmilestones_all_species_test.csv"),
  col_types = cols(
    var = col_character(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    species = col_character(),
    scenario = col_character(),
    pt_id = col_double(),
    val = col_double()
  ))

# Load seasonal version
seasonal_dist <- read_csv(
  here::here("Data/projections/Cmilestones_all_seasons_test.csv"),
  col_types = cols(
    Season = col_character(),
    var = col_character(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    species = col_character(),
    scenario = col_character(),
    pt_id = col_double(),
    val = col_double()
  ))


# Fix Species names:
name_fix <- tribble(
  ~"species",              ~"comname",
  "butterfish",            "butterfish",              
  "cod",                   "Atlantic cod",              
  "haddock",               "haddock",             
  "hagfish",               "hagfish",                
  "halibut",               "halibut",                
  "herring",               "herring",               
  "jonahcrab",             "Jonah crab",                
  "littleskate",           "little skate",                
  "lobster",               "American lobster",               
  "longfinsquid",          "longfin squid",                
  "monkfish",              "monkfish",                
  "northernsandlance",     "northern sandlance",               
  "oceanquahog",           "ocean quahog",                
  "pollock",               "pollock",                
  "reddeepseacrab",        "red deepsea crab",               
  "redhake",               "red hake",                
  "rockcrab",              "rock crab",               
  "scallop",               "scallop",              
  "scup",                  "scup",               
  "shortfinsquid",         "shortfin squid",               
  "silverhake",            "silver hake",              
  "smoothskate",           "smooth skate",               
  "spinydogfish",          "spiny dogfish",               
  "summerflounder",        "summer flounder",              
  "thornyskate",           "thorny skate",               
  "whitehake",             "white hake",               
  "windowpaneflounder",    "windowpane flounder",              
  "winterflounder",        "winter flounder",               
  "winterskate",           "winter skate",                
  "witchflounder",         "witch flounder",              
  "yellowtailflounder",    "yellowtail flounder"             
)

yrly_dist <- left_join(yrly_dist, name_fix)
seasonal_dist <- left_join(seasonal_dist, name_fix)



# Hexagonal grid
hex_grid <- read_sf(here::here("Data/spatial/hex_grid.geojson"))




####  User Selections  ####
# Do the filtering a user would do:
# These match the two user selection controls on the App
species_choice <- "Atlantic cod"
horizon_choice <- "4C"







####_________####
#### Whole Year Distribution Maps  ####

 

####  Map Making - Baseline  ####
map_baseline <- function(species_choice){
  
  # Filter the species
  yrly_species_i <- yrly_dist %>% filter(comname == species_choice)
  
  # Separate baseline from projection
  yrly_base <-  filter(yrly_species_i, var == "baseline_mu")
  
  # Add back in the lat/lon or the grid
  yrly_base <- left_join(yrly_base, hex_grid, by = join_by(pt_id)) %>% st_as_sf()
  
  # Limits and Breaks Standardization
  max_l10 <- 10^(round(log10(max(yrly_base$val)))+1)
  
  # Plot the baseline biomass
  bline_m <- ggplot() +
    geom_sf(data = yrly_base, aes(fill = val), color = "transparent") +
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    geom_sf(data = hague_sf, color = "black", linewidth = 1) +
    scale_fill_carto_c(
      palette = "ag_GrnYl", 
      trans = "log10", 
      labels = scales::comma_format(accuracy = 1, suffix = " kg"),
      direction = 1) +
    coord_sf(
      xlim = c(-182500, 1550000), 
      ylim = c(3875000, 5370000) , 
      expand = F, crs = 32619) +
    labs(
      title = str_c(str_to_title(species_choice), " | Baseline Biomass Density"),
      subtitle = "Years: 2010-2019") +
    theme_map() +
    guides(
      fill = guide_colorbar(
        title = "Average Annual Biomass Density / km2",
        title.position = "top",
        title.hjust = 0, 
        barheight = unit(2.5, "in"),
        barwidth = unit(0.6, "cm"), 
        direction = "vertical",
        frame.colour = "black", 
        ticks.colour = "black")) +
    theme(legend.position = "right")
  bline_m
  
}




####  Baseline Density Map ####
map_baseline(species_choice = "Atlantic cod")
map_baseline(species_choice = "American lobster")
map_baseline(species_choice = "haddock")
map_baseline(species_choice = "herring")






####  Projected Distribution  ####


# Map the projected biomass
map_projection <- function(species_choice, horizon){
  
  # Filter the species
  yrly_species_i <- yrly_dist %>% filter(comname == species_choice)
  
  # Separate baseline from projection
  yrly_base <-  filter(yrly_species_i, var == "baseline_mu")
  
  
  # Get the projected data based on the temp horizon
  yrly_proj <-  filter(
    yrly_species_i, 
    str_detect(var, "mu"), 
    temp_horizon == horizon)
  
  
  # Add back in the lat/lon or the grid
  yrly_proj <- left_join(
    yrly_proj, hex_grid, by = join_by(pt_id)) %>% 
    st_as_sf()
  
  
  # Get the year spread based on horizon:
  horizon_period <- switch(
    EXPR = horizon,
    "0.5C" = "2030-2040",
    "1C"   = "2040-2050",
    "2C"   = "2060-2070",
    "3C"   = "2080-2090",
    "4C"   = "2090-2100")
  
  
  # Make map
  proj_m <- ggplot() +
    geom_sf(data = yrly_proj, aes(fill = val), color = "transparent") +
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    geom_sf(data = hague_sf, color = "black", linewidth = 1) +
    scale_fill_carto_c(
      palette = "ag_GrnYl", 
      trans = "log10", 
      labels = scales::comma_format(accuracy = 1, suffix = " kg"),
      #limits = c(0.5, 10000), 
      direction = 1) +
    coord_sf(
      xlim = c(-182500, 1550000), 
      ylim = c(3875000, 5370000) , 
      expand = F, crs = 32619) +
    labs(
      title = str_c(str_to_title(species_choice), " | +", str_remove(horizon, "C"), deg_c, " Biomass Density"),
      subtitle = str_c("Estimated Years: ", horizon_period)) +
    theme_map() +
    guides(
      fill = guide_colorbar(
        title = "Average Annual Biomass Density / km2",
        title.position = "left",
        title.hjust = 0.5, 
        barheight = unit(2.5, "in"),
        barwidth = unit(0.6, "cm"), 
        direction = "vertical",
        frame.colour = "black", 
        ticks.colour = "black")) +
    theme(legend.position = "right",
          legend.title = element_text(angle = 90))
  proj_m
  
}



# Show the projections
map_projection(species_choice = "Atlantic cod", horizon = "0.5C")
map_projection(species_choice = "Atlantic cod", horizon = "1C")
map_projection(species_choice = "Atlantic cod", horizon = "2C")
map_projection(species_choice = "Atlantic cod", horizon = "3C")
map_projection(species_choice = "Atlantic cod", horizon = "4C")


# The 3/4 main ones
spec <- "herring"
map_baseline(spec)
map_projection(species_choice = spec, horizon = "0.5C") | map_projection(species_choice = spec, horizon = "4C")






####  Map the Distribution Change  ####


#### These Need to be split into gains and losses

# Function to map projection - baseline
map_difference <- function(species_choice, horizon, col_lims = c(-250, 250)){
  
  
  # Filter the species
  yrly_species_i <- yrly_dist %>% filter(comname == species_choice)
  
  # Separate baseline from projection
  yrly_base <-  filter(yrly_species_i, var == "baseline_mu")
  
  # Get the projected data based on the temp horizon
  yrly_proj <-  filter(
    yrly_species_i, 
    str_detect(var, "mu"), 
    temp_horizon == horizon)
  
  
  # Get difference before we add geometry  *****
  yrly_diff <- left_join(
    st_drop_geometry(yrly_base) %>% 
    select(-c(temp_horizon, scenario, var)),
    yrly_proj %>% select(pt_id, proj_val = val)) %>% 
    mutate(val_diff = proj_val - val) %>% 
    left_join(hex_grid, by = join_by(pt_id)) %>% st_as_sf()
  
  # Get the year spread based on horizon:
  horizon_period <- switch(
    EXPR = horizon,
    "0.5C" = "2030-2040",
    "1C"   = "2040-2050",
    "2C"   = "2060-2070",
    "3C"   = "2080-2090",
    "4C"   = "2090-2100")
  
  
  # Limits need to match against something:
  # Round to next power of 10
  roundup_tens <- function(x) 10^ceiling(log10(x))
  
  # What is the highest 10^x difference observed
  max_diff <- roundup_tens(max(abs(yrly_diff$val_diff)))
  
  # limits to divergent scale
  shared_lims <- max_diff * c(-1, 1)
  
  # What is the max exponent for limits in base 10 terms
  max_exp <- log10(max_diff)
  
  # Breaks for divergent 10^x scales
  lbreak <- -1*10^rev(seq(1,max_exp))
  rbreak <- 10^seq(1,max_exp)
  shared_breaks <- c(lbreak, 0, rbreak) 
  
  # Hide no change using alpha
  yrly_diff <- yrly_diff %>% 
    mutate(minor_change = case_when(
      between(val_diff, -10,10) ~ 0.6,
      between(val_diff, -100, -10) ~ 0.7,
      between(val_diff, 10, 100) ~ 0.7,
      between(val_diff, -1000, -100) ~ 0.9,
      between(val_diff, 100, 1000) ~ 0.9,
      TRUE ~ 1))
  #return(yrly_diff)
  
  
  # Make the map - Log Differences
  diff_m <- ggplot() +
    geom_sf(
      data = yrly_diff, 
      aes(
        fill = val_diff, 
        alpha = I(minor_change)
        ),
        color = "transparent") +
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    geom_sf(data = hague_sf, color = "black", linewidth = 1) +
    # Not right
    # scale_fill_carto_c(
    #   palette = "Geyser", 
    #   trans = divergent_l10_trans,
    #   labels = scales::comma_format(accuracy = 1, suffix = " kg"),
    #   breaks = shared_breaks,
    #   limits = shared_lims,
    #   direction = -1) +
    scale_fill_carto_c(
      palette = "Geyser", 
      labels = scales::comma_format(accuracy = 1, suffix = " kg"),
      limits = col_lims, 
      oob = oob_squish,
      direction = -1) +
    coord_sf(
      xlim = c(-182500, 1550000), 
      ylim = c(3875000, 5370000) , 
      expand = F, crs = 32619) +
    labs(
      title = str_c(str_to_title(species_choice), " | +", str_remove(horizon, "C"), deg_c, " Biomass Density"),
      subtitle = str_c("Estimated Years: ", horizon_period)) +
    guides(
      fill = guide_colorbar(
        title = "Change in Biomass Density / km2",
        title.position = "top",
        title.hjust = 0,
        barheight = unit(2.5, "in"),
        barwidth = unit(0.6, "cm"),
        direction = "vertical",
        frame.colour = "black",
        ticks.colour = "black")) +
    theme_map() +
    theme(legend.position = "right")
  
  # Review
  diff_m
  
  
  
  # Split the gains and losses into their own?

}



# Mapping the differences

map_difference(species_choice = "Atlantic cod", horizon = "0.5C", col_lims = c(-25,25))
map_difference(species_choice = "Atlantic cod", horizon = "2C", col_lims = c(-25,25))
map_difference(species_choice = "Atlantic cod", horizon = "4C", col_lims = c(-25,25))

map_difference(species_choice = "haddock", horizon = "0.5C")
map_difference(species_choice = "haddock", horizon = "2C")
map_difference(species_choice = "haddock", horizon = "4C")

map_difference(species_choice = "American lobster", horizon = "0.5C", col_lims = c(-75,75))
map_difference(species_choice = "American lobster", horizon = "2C", col_lims = c(-75,75))
map_difference(species_choice = "American lobster", horizon = "4C", col_lims = c(-75,75))

map_difference(species_choice = "herring", horizon = "0.5C", col_lims = c(-100,100))
map_difference(species_choice = "herring", horizon = "2C", col_lims = c(-100,100))
map_difference(species_choice = "herring", horizon = "4C", col_lims = c(-100,100))







####________#####
####  Seasonal Distributions  #####

seasonal_i <- left_join(seasonal_species_i, hex_grid, by = join_by(pt_id))
seasonal_species_i <- clean_season_wide %>% split(.$species) %>% pluck(species_choice)








# GMRI color palette using blue and yellow
bipalette <- c(
  "lowX_lowY"   = "#e8f7ff", 
  "lowX_midY"   = "#a0d5fa", 
  "lowX_highY"  = "#53b1f5", 
  "midX_lowY"   = "#e8c694", 
  "midX_midY"   = "#a0c694",
  "midX_highY"  = "#53b194", 
  "highX_lowY"  = "#e88a11", 
  "highX_midY"  = "#a08a11", 
  "highX_highY" = "#538a11"
)


# And the legend:
# Need to fix this garbage label table
vals <- c("low", "mid", "high")
label_tib <- tibble(
  "x" = vals, 
  "y" = vals) %>% 
  complete(x,y) %>% 
  mutate(value = str_c(x, "X_", y, "Y"),
         x = factor(x, levels = vals),
         y = factor(y, levels = vals))


# Legend Plot
leg <- ggplot(data = label_tib,
              aes(x=x, y=y, fill=value))+
  geom_point(pch=21,size=7,color="grey90")+
  #geom_tile()+
  #geom_text(aes(label=value),size=10)+
  scale_fill_manual(values=bipalette)+
  guides(fill="none") +
  labs(
    x = "\u2190 Fall Biomass \u2192", 
    y = "\u2190 Summer Biomass \u2192") +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(), 
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_text(face = "bold", size = 8),
    plot.background = element_rect(color = "black"))



