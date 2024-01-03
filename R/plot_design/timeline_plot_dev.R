#####  Timeline Plot Development
# What is change in overall biomass into the future
# For both SSP scenarios and for the different regions

library(sf)
library(gmRi)
library(tidyverse)
library(scales)
library(ggtext)
library(geomtextpath)
library(showtext)

# # Add fonts for figures
#font_add_google("Raleway", "raleway")

# Path to the directory containing the font file (replace with your actual path)
font_dir <- paste0(system.file("stylesheets", package = "gmRi"), "/GMRI_fonts/Avenir/")
font_add(
  family = "Avenir",
  file.path(font_dir, "LTe50342.ttf"),
  bold = file.path(font_dir, "LTe50340.ttf"),
  italic = file.path(font_dir, "LTe50343.ttf"),
  bolditalic = file.path(font_dir, "LTe50347.ttf"))
showtext_auto()



# Load themes etc.
source(here::here("Coca_SDM_app_dev/app_functions/coca_application_funs.R"))



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





####  Plot Design  ####


# Work from the app=ready data
density_timeseries <- read_csv(
  file = here::here("Coca_SDM_app_dev/app_ready_data/projected_densities_timeseries.csv"),
  col_types = list(
    species = col_character(),
    comname = col_character(),
    scenario = col_character(),
    region = readr::col_factor(levels = c("Scotian Shelf", "Gulf of Maine", "Georges Bank", "Mid-Atlantic Bight")), 
    year = col_double(),
    season = col_character(),
    avg_dens = col_double()
  ))




# User selections it could take
species_choice <- "American lobster"
scenario_choice <- "CMIP6_SSP5_85"
horizon_choice <- "3C"



####  Server Side Filtering  ####

# This section should mirror the user inputs from the app

# Filter the data as you would
in_data <- filter(
  density_timeseries, 
  comname == species_choice,
  scenario == scenario_choice) %>% 
  mutate(temp_horizon = "0.5C")




#### Does Function Work:
ssp_projected_timeseries(timeseries_data = in_data, reactive = F)




#####  Testing Space  ####




# Get the correct focal period for the horizon selected
# This would also be a server side thing



  

# Filter the limits out
horizon_lims <- filter(horizon_year_key_df, scenario == scenario_choice, horizon == horizon_choice)

# Make a dataframe that can facet
region_heights <- in_data %>% 
  # Get the max value in windows
  filter(year %in% seq(horizon_lims$xmin, horizon_lims$xmax, 1)) %>% 
  group_by(region) %>% 
  summarise(panel_height = max(avg_dens)) %>% 
  ungroup() %>% 
  bind_cols(., horizon_lims)





# Build the timeseries plots
# Add some columns for alpha and size
in_data <- in_data %>% 
  mutate(
    season_alpha = ifelse(season == "Annual Average", 1, 0.7),
    season_lw = ifelse(season == "Annual Average", 1.4, 0.8),
    season = factor(season, levels = c("Annual Average", "Spring", "Summer", "Fall")))



ggplot() +
  geom_rect(data = horizon_lims, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "gray90") +
  geom_text(data = region_heights, aes(x = mid_year, label = horizon_lab, y = panel_height), vjust = -1, size = 4, fontface = "bold") +
  geom_line(data = in_data, aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha))) +
  facet_wrap(~region, scales = "free_y") +
  scale_x_continuous(limits = c(2020, 2120),
                     expand = expansion(add = c(0,0))) +
  scale_y_continuous(
    limits = c(0,NA), 
    expand = expansion(mult = c(0, 0.2)),
    breaks = pretty_breaks()) +
  theme_plot() +
  labs(
    y = "Average Biomass Density kg/km2", 
    x = "Year", 
    title = str_to_title(species_choice),
    subtitle = "Projected Biomass Change Under SSP5-8.5 Ensemble Scenario")






####  Using colors for seasons:
ggplot() +
  geom_rect(data = horizon_lims, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "gray90") +
  geom_text(data = region_heights, aes(x = mid_year, label = horizon_lab, y = panel_height), vjust = -1.4, size = 4, fontface = "bold") +
  geom_line(data = filter(in_data, season != "Annual Average"), aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha), color = season), key_glyph = "timeseries") +
  geom_line(data = filter(in_data, season == "Annual Average"), aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha), color = season), key_glyph = "timeseries") +
  facet_wrap(~region, scale = "free_y") +
  scale_color_gmri() +
  scale_x_continuous(limits = c(2019, 2110),
                     expand = expansion(add = c(0,0))) +
  scale_y_continuous(
    limits = c(0,NA), 
    expand = expansion(mult = c(0, 0.2)),
    breaks = pretty_breaks()) +
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  theme_plot() +
  labs(
    y = "Average Biomass Density kg/km2", 
    x = "Year", 
    title = str_to_title(species_choice),
    color = "Seasonal Trajectory:",
    subtitle = "Projected Biomass Change Under SSP5-8.5 Ensemble Scenario")




#####  Include Baselines?  ####

# No the numbers in those periods are crazy
bline_means <- in_data %>% 
  # Get the max value in windows
  filter(year %in% seq(2010, 2019, 1),
         season != "Annual Average") %>% 
  group_by(region) %>% 
  summarise(bline_height = mean(avg_dens)) %>% 
  ungroup()


# Not Awful:
ggplot() +
  geom_rect(data = horizon_lims, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "gray90") +
  geom_labelhline(data = bline_means, aes(yintercept = bline_height), label = "Baseline", hjust = 0.95) +
  geom_text(data = region_heights, aes(x = mid_year, label = horizon_lab, y = panel_height), vjust = -1.4, size = 4, fontface = "bold") +
  geom_line(data = filter(in_data, season != "Annual Average"), aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha), color = season), key_glyph = "timeseries") +
  geom_line(data = filter(in_data, season == "Annual Average"), aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha), color = season), key_glyph = "timeseries") +
  facet_wrap(~region, scale = "free_y") +
  scale_color_gmri() +
  scale_x_continuous(limits = c(2020, 2135),
                     expand = expansion(add = c(0,0))) +
  scale_y_continuous(
    limits = c(0,NA), 
    expand = expansion(mult = c(0, 0.2)),
    breaks = pretty_breaks()) +
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  theme_plot() +
  labs(
    y = "Average Biomass Density kg/km2", 
    x = "Year", 
    title = str_to_title(species_choice),
    color = "Seasonal Trajectory:",
    subtitle = "Projected Biomass Change Under SSP5-8.5 Ensemble Scenario")










# # Is the highlight on the time period better with a line? no
# library(geomtextpath)
# in_data %>% 
#   mutate(
#     season_alpha = ifelse(Season == "Annual Average", 1, 0.5),
#     season_lw = ifelse(Season == "Annual Average", 1, 0.5)) %>% 
#   ggplot() +
#   geom_rect(data = horizon_lims, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "gray90") +
#   geom_textvline(data = region_heights, aes(xintercept = mid_year, label = horizon_lab, y = panel_height), vjust = -1, size = 4, fontface = "bold") +
#   geom_line(aes(Year, mean_Prob_0.5, group = Season, linewidth = I(season_lw), alpha = I(season_alpha))) +
#   facet_wrap(~region) +
#   scale_x_continuous(limits = c(2020, 2110)) +
#   scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.2))) +
#   theme_plot() +
#   labs(y = "Average Biomass Density kg/km2", x = "Year")



####  Should it be interactive  ####
