# Plotting Preference Curves:
# Follows data prep of: 03_preference_curve_data_prep.R







####_______________________####

####  Using Preference Results in Practice:  ####

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
    geom_ribbon(aes(x = val_actual, ymin = 0, ymax = fit_exp), color = "transparent", alpha = 0.3, fill = gmri_cols("blue economy teal")) +
    geom_line(aes(val_actual, fit_exp, group = comname), linewidth = 1) +
    geom_vline(aes(xintercept = val, color = temp_horizon), linewidth = 1.2, key_glyph = draw_key_rect) +
    scale_color_manual(values = gmri_cols("orange"), na.translate = F) +
    facet_grid(region~variable, scales = "free") +
    scale_x_continuous(expand = expansion(add = c(0,0))) +
    scale_y_continuous(expand = expansion(mult = c(0,.4))) +
    #theme_plot() +
    theme()
  labs(
    x = "Covariate Range", 
    title = "Species Preferences & Regional Conditions of a Future Climate", 
    subtitle = str_c(species, " | ", horizon, " Climate (", scenario, ": ", period, ")"),
    color = "Regional Conditions for Future Climate State:",
    y = "Predicted kg/km2")
}


plot_preference_curves(pref_dat = curve_dat)

####