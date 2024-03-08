#####  Preference Curve Data Prep  ####
# Get the species preference information prepared for the app
# should be drop-in rady for filtering and plotting

library(raster)
library(sf)
library(gmRi)
library(tidyverse)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")


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


####
"
The individual preference curves for species have been processed independently and 
can be found here: `Mills_Lab/Projects/COCA19_projections/tables/`

These curves need to be transformed back to absolute temperature and depth units in celsius 
or meters for each of the covariate curves. This is done with the grand mean values from the 
environmental covariate inputs to the VAST training data.

Grand mean/sd of the covariates can be used to un-scale these relationships.    
**Depth** mean/sd = 123.47/100.2   
**SST**   mean/sd = 11.11/4.55   
**BT**    mean/sd = 7/2.72   

"

# Path to preference curve results
pref_path <- cs_path("mills", "Projects/COCA19_projections/tables")
pref_names <- list.files(pref_path, pattern = "full_covariate_effects.rds") %>% 
  str_remove_all("_full_covariate_effects.rds") %>% 
  tolower()  
pref_dat <- map(list.files(pref_path, pattern = "full_covariate_effects.rds", full.names = T), read_rds) %>% 
  setNames(pref_names)


# Grand means for re-scaling
rescale_df <- tribble(
  ~"Covariate",   ~"gmean",  ~"gsd",
  "Depth",         123.47,   100.2,
  "SST_seasonal",  11.11,    4.55,
  "BT_seasonal",   7,        2.72
)




# Put them all  in one table, fix the species names
pref_all <- pref_dat %>% 
  bind_rows(.id = "species") %>% 
  left_join(rescale_df) %>% 
  left_join(name_fix) %>% 
  pivot_wider(names_from = Lin_pred, values_from = c(fit, se, lower, upper), names_sep = "_") %>% 
  mutate(
    val_actual = (Value * gsd) + gmean,
    fit_exp = exp(fit_X1 + fit_X2),
    up_exp  = exp(upper_X1 + upper_X2),
    low_exp = exp(lower_X1 + lower_X2),
    variable = case_when(
      Covariate == "SST_seasonal" ~ "Surface Temperature",
      Covariate == "BT_seasonal" ~ "Bottom Temperature",
      TRUE ~ "Depth"))



# Inspect
glimpse(pref_all)
spec_prefs <- filter(pref_all, comname == "Atlantic cod")
# only need: val_actual, fit exp, comname

ggplot() +
  geom_line(data = spec_prefs, aes(val_actual, fit_exp, group = comname), linewidth = 1) +
  #geom_vline(data = cond_05, aes(xintercept = val, color = Region, linetype = scenario), linewidth = 0.8, key_glyph = draw_key_timeseries) +
  facet_wrap(.~variable, scales = "free_x") +
  scale_color_gmri() +
  guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5),
         color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(
    x = "Covariate Value", 
    title = "Species Preferences & Regional Conditions", 
    color = "Average Regional Condition",
    linetype = "SSP Scenario",
    y = "Predicted kg/km2")






####  Environmental Condition Data  ####

# These have been cropped for the EPUS in 
# github.com/adamkemberling/sdm_workflow/blob/main/CMIP6_processing/R/SDM_explainability.qmd

horizon_conditions <- read_csv(
  here::here("Data/env_data/regional_scenario_conditions_at_horizons.csv"),
  col_types = list(
    scenario = col_character(), 
    Region = col_character(), 
    variable = col_character(), 
    var = col_character(), 
    ref_period = col_character(), 
    temp_horizon = col_character(),
    val = col_double()
  )) %>% 
  rename_all(~tolower(.x))






####  Getting Regional Depths  ####
depth_dat <- raster(str_c(cs_path("res", "Bathy/ETOPO1"), "NEShelf_Etopo1_bathy.tiff"))
epus <- ecodata::epu_sf %>% split(.$EPU) %>% 
  setNames(c("Georges Bank", "Gulf of Maine", "Mid-Atlantic Bight", "Scotian Shelf"))

# Masking Function
mask_nc <- function(ras_obj, mask_shape, rotate = TRUE){
  
  # First Mask using the polygon, rotate if needed
  if(rotate == TRUE){
    m1 <- mask(rotate(ras_obj), mask_shape)
  } else {
    m1 <- mask(ras_obj, mask_shape)
  }
  
  # Then Crop
  m1 <- crop(m1, mask_shape)
  return(m1)
}

# Get depths
masked_depths <- map(epus, ~mask_nc(ras_obj = depth_dat, mask_shape = .x, rotate = F))

# Cool
plot(masked_depths$`Georges Bank`)

# Get average
mean_depths <- map_dfr(masked_depths, ~cellStats(.x, stat = mean, na.rm = T) %>% 
                         as_tibble(), 
                       .id = "region") %>% 
  mutate(variable = "Depth",
         val = value*-1) %>% 
  dplyr::select(- value)




####  Streamline ####
# This section is all about dropping what we don't need to plot


# Preference data:
# for preferences only need: val_actual, fit exp, comname
pref_slim <- pref_all %>% select(comname, val_z = Value, val_actual, fit_exp, variable)
# only need: val_actual, fit exp, comname




# For conditions we need: scenario, horizon
# Don't need baseline, or var
horizon_conditions <- filter(horizon_conditions, temp_horizon != "0C") %>% 
  select(-var)

# Tidy up the regios
horizon_conditions <- horizon_conditions %>% 
  mutate(
    region = case_when(
      region == "EPU_SS" ~ "Scotian Shelf",
      region == "EPU_GOM" ~ "Gulf of Maine",
      region == "EPU_GB" ~ "Georges Bank",
      region == "EPU_MAB" ~ "Mid-Atlantic Bight"
    ))


# Add in the depth data:
horizon_conditions <- horizon_conditions %>% 
  distinct(scenario, region, ref_period, temp_horizon) %>% 
  left_join(mean_depths) %>% 
  bind_rows(horizon_conditions)



####  Save Them  ####
write_csv(pref_slim, here::here("Coca_SDM_app_dev/app_ready_data/preference_curve_data.csv"))
write_csv(horizon_conditions, here::here("Coca_SDM_app_dev/app_ready_data/projected_environmental_conditions.csv"))






####_______________________####

####  Results in Practice:  ####

####  Load ^App-Ready Data  ####

# load the data 
pref_data <- read_csv(here::here("Coca_SDM_app_dev/app_ready_data/preference_curve_data.csv")) %>% split(.$comname)
env_condition_data <- read_csv(here::here("Coca_SDM_app_dev/app_ready_data/projected_environmental_conditions.csv"))

# Split the preference data by species
pref_data <- pref_slim %>% split(.$comname)

####  Input Filtering  ####

# Inputs:
species_option  <- "Atlantic cod"
scenario_option <- "CMIP6_SSP5_85"
horizon_option  <- "0.5C"


# a. conditions
cond_x <- filter(env_condition_data, 
                 scenario == str_remove(scenario_option, "CMIP6_"), 
                 temp_horizon == horizon_option)


# b. preferences
prefs_x <- pref_data %>% pluck(species_option) 

# build out regions
prefs_x <- bind_rows(list(
  mutate(prefs_x, region = "Gulf of Maine"),
  mutate(prefs_x, region = "Georges Bank"),
  mutate(prefs_x, region = "Scotian Shelf"),
  mutate(prefs_x, region = "Mid-Atlantic Bight"))) %>% 
  mutate(region = factor(region, levels = c("Scotian Shelf", "Gulf of Maine", "Georges Bank", "Mid-Atlantic Bight")))

# Join them:
curve_dat <- left_join(prefs_x, cond_x)





####  Plot Design  ####

# Should be able to get these things from data, so we don't need to pass more info
# To the plot function

curve_dat %>% glimpse()

deg_sym <- "\u00b0" 
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"


# Plotting function to do pref curves
plot_preference_curves <- function(pref_dat, reactive = F){
  # Set the data
  curve_dat <- pref_dat
  if(reactive){curve_dat <- pref_dat()}
  
  
  # Get scenario/horizon from the data so we don't need to feed so many inputs
  one_rec <-  curve_dat %>% drop_na() %>%  slice(1)
  species <- one_rec %>% pull(comname)
  scenario <- one_rec %>% pull(scenario)
  horizon <- one_rec %>% pull(temp_horizon)
  period <- one_rec %>% pull(ref_period)
  
  # Tidy text
  horizon <- str_c("+",str_sub(horizon, 1,-2), deg_c)
  
  # Clean up scenario text
  scenario <- ifelse(scenario == "SSP1_26", "SSP1-2.6", "SSP5-8.5")
  
  # Make the plot
  curve_dat %>% 
    ggplot() +
    geom_ribbon(aes(x = val_actual, ymin = 0, ymax = fit_exp), color = "transparent", alpha = 0.3, fill = gmri_cols("teal")) +
    geom_line(aes(val_actual, fit_exp, group = comname), linewidth = 1) +
    geom_vline(aes(xintercept = val, color = temp_horizon), linewidth = 1.2, key_glyph = draw_key_rect) +
    scale_color_manual(values = gmri_cols("orange"), na.translate = F) +
    facet_grid(region~variable, scales = "free") +
    scale_x_continuous(expand = expansion(add = c(0,0))) +
    scale_y_continuous(expand = expansion(mult = c(0,.4))) +
    #theme_plot() +
    labs(
      x = "Covariate Range", 
      title = "Species Preferences & Regional Conditions of a Future Climate", 
      subtitle = str_c(species, " | ", horizon, " Climate (", scenario, ": ", period, ")"),
      color = "Regional Conditions for Future Climate State:",
      y = "Predicted kg/km2")
}


plot_preference_curves(pref_dat = curve_dat)

####