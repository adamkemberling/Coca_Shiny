# Globally Available Resources

####  Packages  ####
library(scales)
library(here)
library(shiny)
library(bslib)
library(here)
library(rnaturalearth)
library(sf)
library(bsicons)
library(rcartocolor)
library(showtext)
library(tidyverse)
library(ggtext)
library(geomtextpath)


# Delete before publishing:
library(gmRi)


# conflicted::conflict_prefer("filter", "dplyr")
# conflicted::conflict_prefer("col_factor", "readr")

#### Support Functions:  ####

# Support Code and Modules
source(here::here("Coca_SDM_app_dev/modules/mod-sidebar-inputs.R")) # Data filtering
source(here::here("Coca_SDM_app_dev/app_functions/coca_application_funs.R")) # Plotting functions
source(here::here("Coca_SDM_app_dev/modules/mod-distribution-map-card.R")) # Baseline and horizon maps
source(here::here("Coca_SDM_app_dev/modules/mod-difference-map-card.R")) # Change in Biomass Map
source(here::here("Coca_SDM_app_dev/modules/mod-projected-timeseries-card.R")) # Projected Density Timeseries
source(here::here("Coca_SDM_app_dev/modules/mod-preference-curve-card.R")) # Specied Preference Curve


# Path to the directory containing the font file (replace with your actual path)
font_dir <- paste0(system.file("stylesheets", package = "gmRi"), "/GMRI_fonts/Avenir/")
font_add(
  family = "Avenir",
  file.path(font_dir, "LTe50342.ttf"),
  bold = file.path(font_dir, "LTe50340.ttf"),
  italic = file.path(font_dir, "LTe50343.ttf"),
  bolditalic = file.path(font_dir, "LTe50347.ttf"))
showtext_auto()


####  App Theming  ####
custom_theme <- bs_theme(
  version          = 5,
  base_font        = "Avenir", 
  #heading_font     = font_google("Raleway"), 
  heading_font     = "Avenir",  
  fg               = "#00736D",
  bg               = "#FFFFFF",
  primary          = "#00608A", 
  secondary        = "#535353", 
  success          = "#407331", 
  info             = "#ABB400", 
  warning          = "#EACA00", 
  danger           = "#EA4F12", 
  font_scale       = 1.25, 
  `enable-shadows` = TRUE, 
  spacer           = "1.25rem"
  
)



####  Globally Available Small Resources  ####

# Fix Species names:
name_fix <- tribble(
  ~"species",              ~"comname",
  "atlanticmackerel",      "Atlantic mackerel",              
  "butterfish",            "butterfish",     
  "blackseabass",          "black sea bass",
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
  "redfish",               "acadian redfish",
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


# Year key for text and plot labeling
horizon_year_key_df <- tribble(
  ~"scenario",    ~"horizon",  ~"xmin",  ~"xmax",
  "CMIP6_SSP1_26",  "0C",      2010,     2019,
  "CMIP6_SSP1_26",  "0.5C",    2048,     2052,
  "CMIP6_SSP1_26",  "1C",      2048,     2052,
  "CMIP6_SSP1_26",  "1.5C",    2048,     2052,
  "CMIP6_SSP1_26",  "2C",      2048,     2052,
  "CMIP6_SSP1_26",  "3C",      2048,     2052,
  "CMIP6_SSP1_26",  "4C",      2048,     2052,
  "CMIP6_SSP5_85",  "0C",      2010,     2019,
  "CMIP6_SSP5_85",  "0.5C",    2034,     2038,
  "CMIP6_SSP5_85",  "1C",      2042,     2045,
  "CMIP6_SSP5_85",  "1.5C",    2054,     2058,
  "CMIP6_SSP5_85",  "2C",      2064,     2068,
  "CMIP6_SSP5_85",  "3C",      2077,     2081,
  "CMIP6_SSP5_85",  "4C",      2095,     2099)




# Hexagonal grid simple feature geometry
#hex_grid <- read_sf(here::here("Data/spatial/hex_grid.geojson"))
hex_grid <- read_sf(here::here("Coca_SDM_app_dev/app_ready_data/hex_grid.geojson"))


####_________________________####



#### VAST Ouptput Data  ####



##### A. Milestones Distributions    ####


# Load the decadal milestone summaries
horizon_projections <- read_csv(
  here::here("Data/projections/Cmilestones_all_species_test.csv"),
  col_types = cols(
    var = col_character(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    species = col_character(),
    scenario = col_character(),
    pt_id = col_double(),
    val = col_double()))  %>% 
  left_join(name_fix, by = join_by(species)) %>% 
  filter(str_detect(var, "_mu"))



# Split the Projection data by species for reactive selection
species_projection_list <- horizon_projections %>% 
  split(.$comname)



##### B.  Timeseries Datasets  ####
density_timeseries <- read_csv(
  file = here::here("Coca_SDM_app_dev/app_ready_data/projected_densities_timeseries.csv"),
  col_types = list(
    species = col_character(),
    comname = col_character(),
    scenario = col_character(),
    region = readr::col_factor(levels = c("Scotian Shelf", "Gulf of Maine", "Georges Bank", "Mid-Atlantic Bight")), 
    year = col_double(),
    season = col_character(),
    avg_dens = col_double())) %>% 
  split(.$comname)


##### C. Species Preference Information  ####
pref_data <- read_csv(
  file = here::here("Coca_SDM_app_dev/app_ready_data/preference_curve_data.csv"),
  col_types = list(
    comname = col_character(),
    variable = col_character(),
    val_z = col_double(),
    val_actual = col_double(),
    fit_exp = col_double())) %>% 
  split(.$comname)



#### D.  Environmental Conditions  ####
env_condition_data <- read_csv(
  file = here::here("Coca_SDM_app_dev/app_ready_data/projected_environmental_conditions.csv"),
  col_types = cols(
    scenario = col_character(),
    region = col_character(),
    variable = col_character(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    val = col_double())) %>% 
  mutate(region = factor(region, levels = c("Scotian Shelf", "Gulf of Maine", "Georges Bank", "Mid-Atlantic Bight")))




#### User Selection "_opts"  ####


# Names of species
species_opts <- sort(unique(horizon_projections$comname))
species_opts <- setNames(species_opts, str_to_title(species_opts))

#  SSP scenarios
scenario_opts <- c(
  "Fossil-Fueled Development" = "CMIP6_SSP5_85",
  "Sustainable Development" = "CMIP6_SSP1_26"
  )

# Temperatures Above Modern Climate
horizon_vals <- c(0.5, 1, 1.5, 2, 2, 3, 4)
horizon_opts  <-  str_c(horizon_vals, "C")
horizon_opts <- setNames(horizon_opts, str_c("+", horizon_vals, deg_c))







#### Species Graphic Assets  ####

# This could be text and any images/artwork associated with a species
# Not a huge deal, but would add to the baseline page


# Silhouette imagery can be taken from phylopic.org
# Done: www/phylopics



# Also need species baseline characteristics:
# Average temp preference
# Average depth preferences
# Average size/weight
# Modeled Preference Curves
# This should all be in some simple small csv (in theory)

