# App Functions for COCA SSP Projection App
library(scales)

# Unicode for degrees
deg_sym <- "\u00b0" 
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"



####  Raw Materials  ####

# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))

# Hexagonal grid
hex_grid <- read_sf(here::here("Data/spatial/hex_grid.geojson"))

# Plotting map theme
theme_map <- function(guides = T, ...){
  list(
    # Theme options, with ellipse to add more
    theme(
      # Font across all text
      text = element_text(family = "Avenir"),
      
      # Titles
      plot.title = element_text(hjust = 0, face = "bold", size = 16),
      plot.subtitle = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      
      # Grids and Axes
      panel.background = element_blank(), 
      panel.border = element_rect(color = "black", fill = "transparent"), 
      panel.grid.major = element_line(color = "gray80"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks=element_blank(),
      plot.margin = margin(t = 10, r = 2, b = 0.1, l = 2, unit = "pt"),
      legend.position = c(.725, .125), 
      legend.background = element_rect(color = "transparent", fill = "white", linewidth = 0.25),
      
      # Use ellipses for tweaks on the fly:
      ...))
}

# Plot theme for timeseries
theme_plot <- function(...){
  list(
    theme(
      # Titles
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      
      # Axes
      rect = element_rect(fill = "transparent", color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.ticks.y = element_line(), 
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(), 
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      # Panel/Grid Setup
      panel.grid = element_line(colour = NULL, linetype = 3, color = "gray80"), 
      panel.grid.major = element_line(colour = "black"), 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(), 
      # Facets
      strip.text = element_text(color = "white", 
                                face = "bold",
                                size = 11),
      strip.background = element_rect(
        color = "#00736D", 
        fill = "#00736D", 
        size = 1, 
        linetype="solid"),
      # Legend
      legend.position = "bottom"
      
    ) # close theme()
    
  )
  
  
}


####  Map Testing - Reactive  ####
# Code from static data with reactive df()

# Function to map the actual values
ssp_proj_map <- function(
    dist_df,          # simple features dataframe that is to be mapped
    max_l10 = NULL){         # Color scale limit
  
  # Add information for opacity
  density_sf <- mutate(.data = dist_df(), val = ifelse(val < 0.5, NA, val))
  
  # Get scenario/horizon from the data so we don't need to feed so many inputs
  one_rec<-  density_sf %>% slice(1)
  species<- one_rec %>% pull(comname)
  scenario <- one_rec %>% pull(scenario)
  horizon <- one_rec %>% pull(temp_horizon)
    
  # Year key for text and plot labeling
  horizon_year_key <- list(
    "CMIP6_SSP1_26" = c(
      "0C"   = "2010-2019",
      "0.5C" = "2048-2052"),
    "CMIP6_SSP5_85" = c(
      "0C"   = "2010-2019",
      "0.5C" = "2034-2038",
      "1C"   = "2041-2045",
      "1.5C" = "2054-2058",
      "2C"   = "2064-2068",
      "3C"   = "2077-2081",
      "4C"   = "2095-2099"))

  # Get the years sandwiching the horizon
  horizon_years <- horizon_year_key[[scenario]][[horizon]]

  # Clean up scenario text
  scenario <- ifelse(scenario == "CMIP6_SSP1_26", "SSP1-2.6", "SSP5-8.5")

  # Clean up horizon text
  horizon_choice <- str_c(str_remove(horizon, "C"), deg_c)

  # Prepare title text:
  plot_title <- str_c(species, " | Biomass Density with +", horizon_choice, " Climate")
  plot_subtitle = str_c(scenario, " Timeframe: ", horizon_years)
  
  
  # Plot the projected biomass
  projection_map <- ggplot() +
    geom_sf(data = density_sf, aes(geometry = geometry, fill = val), color = "transparent") +
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    geom_sf(data = hague_sf, color = "black", linewidth = 1) +
    scale_fill_carto_c(
      palette = "ag_GrnYl", 
      trans = "log10", 
      #limits = c(1, max_l10),
      limits = c(1, 10000),
      labels = scales::comma_format(accuracy = 1, suffix = " kg"), 
      na.value = "transparent",
      direction = 1) +
    coord_sf(
      xlim = c(-182500, 1550000), 
      ylim = c(3875000, 5370000) , 
      expand = F, crs = 32619) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle) +
    theme_map() +
    guides(
      fill = guide_colorbar(
        title = "Mean Annual Biomass Density / km2",
        title.position = "top",
        title.hjust = 0, 
        barheight = unit(2.5, "in"),
        barwidth = unit(0.6, "cm"), 
        direction = "vertical",
        frame.colour = "black", 
        ticks.colour = "black")) +
    theme(legend.position = "right")
  return(projection_map)
  
}





####  Map Testing - Static ####

# Function to map the actual values
ssp_proj_map_static <- function(
    dist_df,          # simple features dataframe that is to be mapped
    scenario,         # SSP scenario for label generation
    horizon_choice,   # The amount hotter than current climate being projected
    species_option,   # The species name
    max_l10 = NULL){         # Color scale limit
  
  
  
  # Year key for text and plot labeling
  horizon_year_key <- list(
    "CMIP6_SSP1_26" = c(
      "0C"   = "2010-2019",
      "0.5C" = "2048-2052"),
    "CMIP6_SSP5_85" = c(
      "0C"   = "2010-2019",
      "0.5C" = "2034-2038",
      "1C"   = "2041-2045",
      "1.5C" = "2054-2058",
      "2C"   = "2064-2068",
      "3C"   = "2077-2081",
      "4C"   = "2095-2099"))

  # Get the years sandwiching the horizon
  horizon_years <- horizon_year_key[[scenario]][[horizon_choice]]

  # Clean up scenario text
  scenario <- ifelse(scenario == "CMIP6_SSP1_26", "SSP1-2.6", "SSP5-8.5")

  # Clean up horizon text
  horizon_choice <- str_c(str_remove(horizon_choice, "C"), deg_c)

  # Prepare title text:
  plot_title <- str_c(species_option, " | Biomass Density with +", horizon_choice, " Climate")
  plot_subtitle = str_c(scenario, " Timeframe: ", horizon_years)
  
  # Add information for opacity
  density_sf <- mutate(dist_df, val = ifelse(val < 0.5, NA, val))
  
  # Plot the projected biomass
  ggplot() +
    geom_sf(data = density_sf, aes(fill = val), color = "transparent") +
    geom_sf(data = mutate(dist_df, val = ifelse(val < 0.5, NA, val)), aes(fill = val), color = "transparent") +
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    geom_sf(data = hague_sf, color = "black", linewidth = 1) +
    scale_fill_carto_c(
      palette = "ag_GrnYl", 
      trans = "log10", 
      limits = c(1, max_l10),
      labels = scales::comma_format(accuracy = 1, suffix = " kg"), 
      na.value = "transparent",
      direction = 1) +
    coord_sf(
      xlim = c(-182500, 1550000), 
      ylim = c(3875000, 5370000) , 
      expand = F, crs = 32619) +
    # labs(
    #   title = plot_title,
    #   subtitle = plot_subtitle) +
    theme_map() +
    guides(
      fill = guide_colorbar(
        title = "Mean Annual Biomass Density / km2",
        title.position = "top",
        title.hjust = 0, 
        barheight = unit(2.5, "in"),
        barwidth = unit(0.6, "cm"), 
        direction = "vertical",
        frame.colour = "black", 
        ticks.colour = "black")) +
    theme(legend.position = "right")
  
}





####  Map SSP Projection Change  ####

# Function to plot the differences
map_difference <- function(species_choice, ssp_base, ssp_projection, horizon, col_lims = c(-250, 250)){
  
  
  # Grab the baseline data
  yrly_base <-  ssp_base %>% st_drop_geometry()
  
  # Grab the projection Data
  yrly_proj <- ssp_projection %>% st_drop_geometry()
  
  # Get difference before we add geometry  *****
  yrly_diff <- left_join(
    yrly_base %>% select(-c(temp_horizon, scenario, var)),
    yrly_proj %>% select(pt_id, proj_val = val)) %>% 
    mutate(val_diff = proj_val - val) %>% 
    left_join(hex_grid, by = join_by(pt_id)) %>% 
    st_as_sf()
  
  # Get the year spread based on horizon:
  horizon_period <- switch(
    EXPR = horizon,
    "0C"   = "2010-2019",
    "0.5C" = "2034-2038",
    "1C"   = "2041-2045",
    "1.5C" = "2054-2058",
    "2C"   = "2064-2068",
    "3C"   = "2077-2081",
    "4C"   = "2095-2099")
  
  horizon <- str_c(str_remove(horizon, "C"), deg_c)
  
  
  # Limits need to match against something:
  # Round to next power of 10
  roundup_tens <- function(x) 10^ceiling(log10(x))
  
  # What is the highest 10^x difference observed
  max_diff <- roundup_tens(max(abs(yrly_diff$val_diff)))
  
  # limits to divergent scale
  shared_lims <- max_diff * c(-1, 1)
  
  # Hide no change using alpha
  yrly_diff <- yrly_diff %>% 
    mutate(minor_change = case_when(
      between(val_diff, -10,10) ~ 0.4,
      between(val_diff, -100, -10) ~ 0.6,
      between(val_diff, 10, 100) ~ 0.6,
      between(val_diff, -1000, -100) ~ 1,
      between(val_diff, 100, 1000) ~ 1,
      TRUE ~ 1))
  #return(yrly_diff)
  
  
  # Make the map - Log Differences
  diff_m <- ggplot() +
    geom_sf(
      data = yrly_diff, 
      aes(
        fill = val_diff, 
        alpha = I(minor_change)),
      color = "transparent") +
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    geom_sf(data = hague_sf, color = "black", linewidth = 1) +
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
      title = str_c(str_to_title(species_choice), " | Biomass Change at +", horizon),
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
  
  
}




#### Plot Species Preference Curves  ####

# This function requires:
# species preference curve information
# name of species to filter
# and the accommpanying environmental state
plot_prefs <- function(spec_prefs, species_option, cond_05){
  
  ggplot(spec_prefs) +
    # geom_ribbon(aes(val_actual, ymin = low_exp, ymax = up_exp), fill = "gray70") +
    geom_line(aes(val_actual, fit_exp, group = comname), linewidth = 1) +
    geom_vline(data = cond_05, aes(xintercept = val, color = Region, linetype = scenario), linewidth = 0.8, key_glyph = draw_key_timeseries) +
    facet_wrap(.~variable, scales = "free_x") +
    scale_color_gmri() +
    guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5),
           color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme_gmri(legend.box = "horizontal") +
    labs(
      x = "Covariate Value", 
      title = "Species Preferences & Regional Conditions", 
      color = "Average Regional Condition",
      linetype = "SSP Scenario",
      y = "Predicted kg/km2",
      subtitle = str_c(species_option, " | +0.5C"))
}

