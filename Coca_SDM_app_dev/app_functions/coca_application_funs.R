# App Functions for COCA SSP Projection App
library(scales)

# Unicode for degrees
deg_sym <- "\u00b0" 
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"


# Coordinates for EPU labels
labels_df <- tibble(
  x = c(280000, 700000, 420000, 945000),
  y = c(4200000, 4400000, 4950000, 4600000),
  hjust = c(0),
  vjust = c(0),
  angle = c(0),
  label = c("Mid-Atlantic\nBight", "Georges Bank", "Gulf of\nMaine", "Scotian Shelf"))



####  Raw Materials  ####

# Read the cropped land coverage:
# land_sf <- read_sf(here::here("Data/spatial/nw_atlantic_countries_crs32619.geojson"))
land_sf <- read_sf(here::here("COCA_SDM_app_dev/dev/scratch_data", "nw_atlantic_countries_crs32619.geojson"))

# Load the Hague Lines
# hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))
hague_sf <- read_sf(here::here("COCA_SDM_app_dev/dev/scratch_data", "hagueline_crs32619.geojson"))

# Hexagonal grid
# hex_grid <- read_sf(here::here("Data/spatial/hex_grid.geojson"))
hex_grid <- read_sf(here::here("COCA_SDM_app_dev/dev/scratch_data", "hex_grid.geojson"))

# Buffer?
# epu_buff <- read_sf(here::here("Data/spatial/EPU_Separated_Buffer2/EPU_Separated_Buffer2.shp"))
epu_buff <- read_sf(here::here("COCA_SDM_app_dev/dev/scratch_data", "EPU_Separated_Buffer2/EPU_Separated_Buffer2.shp"))


####  Plot Themes  ####

# Plotting map theme
theme_map <- function(fontfam = "Avenir", guides = T, ...){
  list(
    # Theme options, with ellipse to add more
    theme(
      # Font across all text
      text = element_text(family = fontfam),
      
      # Titles + Text
      plot.title = element_text(hjust = 0, face = "bold", size = 20),
      plot.subtitle = element_text(size = 18),
      legend.title = element_text(size = 16, lineheight = 1.75),
      legend.text = element_text(size = 14), 
      legend.spacing.y = unit(1.75, "lines"),
      
      # Grids and Axes
      panel.background = element_blank(), 
      panel.border = element_rect(color = "black", fill = "transparent"), 
      panel.grid.major = element_line(color = "gray80"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks=element_blank(),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"),
      #legend.position = c(.725, .125), 
      legend.background = element_rect(color = "transparent", fill = "white", linewidth = 0.25),
      
      # Use ellipses for tweaks on the fly:
      ...))
}

# Plot theme for timeseries
theme_plot <- function(fontfam = "Avenir", ...){
  list(
    theme(
      # Titles
      plot.title    = element_text(family = fontfam, hjust = 0, face = "bold", size = 16),
      plot.subtitle = element_text(family = fontfam,  size = 18),
      legend.title  = element_text(family = fontfam, size = 16),
      legend.text   = element_text(family = fontfam, size = 14),
      legend.spacing.x = unit(1, "lines"),
      
      # Axes
      rect         = element_rect(fill = "transparent", color = "black"),
      axis.line.y  = element_line(color = "black"),
      axis.ticks.y = element_line(), 
      axis.line.x  = element_line(color = "black"),
      axis.ticks.x = element_line(), 
      axis.text    = element_text(size = 12),
      axis.title   = element_text(size = 14),
      axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1),
      plot.margin = margin(t = 5,r = 5,b = 2,l = 5),
      
      # Panel/Grid Setup
      panel.grid         = element_line(colour = NULL, linetype = 3, color = "gray80"), 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor   = element_blank(), 
      panel.background   = element_rect(fill = "white"),
      panel.spacing = unit(0.5, "lines"),
      panel.spacing.y = unit(1, "lines"),
      
      # Facets
      strip.background = element_rect(
        fill      = "#00736D", 
        color = "white"),
      strip.text = element_text(
        color     = "white", 
        face      = "bold",
        size      = 12,
        family    = fontfam),
      # Using html text boxes for rounded corners
      # strip.text.x = element_textbox(
      #   color     = "white", 
      #   face      = "bold",
      #   size      = 12,
      #   family    = fontfam,
      #   fill      = "#00736D",
      #   box.color = "#00736D",
      #   halign    = 0.5,
      #   valign    = 0.5,
      #   linetype  = 1,
      #   r         = unit(5, "pt"),
      #   width     = unit(1, "npc"),
      #   height    = unit(2.25, "lines"),
      #   padding   = margin(2, 0, 1, 0),
      #   margin    = margin(3, 3, 3, 3)),
      # strip.text.y = element_textbox(
      #   color     = "white", 
      #   face      = "bold",
      #   size      = 12,
      #   family    = fontfam,
      #   fill      = "#00736D",
      #   box.color = "#00736D",
      #   halign    = 0.5,
      #   valign    = 0.5,
      #   linetype  = 1,
      #   r         = unit(5, "pt"),
      #   width     = unit(1, "npc"),
      #   height    = unit(3.5, "lines"),
      #   padding   = margin(2, 0, 1, 0),
      #   margin    = margin(3, 3, 3, 3),
      #   orientation = "right-rotated"),
      # strip.background = element_blank(),
      legend.position = "bottom"
      
    ) # close theme()
    
  )
  
  
}


####  Functions  ####

# Round to next power of 10
roundup_tens <- function(x) {10^ceiling(log10(x))}






#####_____________________#####
##### Static Application Displays  ####

####  Biomass Density Map  ####




# Function to map the actual values
ssp_proj_map <- function(
    dist_df,          # simple features dataframe that is to be mapped
    reactive = F,
    add_labels = FALSE){  # Color scale limit
  
  
  # Set the data
  density_sf <- dist_df
  if(reactive){density_sf <- dist_df()}
  
  # Hide values below some threshold
  density_sf <- density_sf %>%
    mutate(
      val = ifelse(val < 0.9, NA, val),
      val_alpha = ifelse(is.na(val), 0, 0.8))
  
  # Get scenario/horizon from the data so we don't need to feed so many inputs
  one_rec  <-  density_sf %>% slice(1)
  species  <- one_rec %>% pull(comname)
  scenario <- one_rec %>% pull(scenario)
  horizon  <- one_rec %>% pull(temp_horizon)
  
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
  plot_title <- str_c(str_to_title(species), " | Biomass Density with +", horizon_choice, " Climate")
  if(horizon == "0C"){
    plot_title <- str_c(str_to_title(species), " | Baseline Period (2010-2019) Biomass Density")}
  plot_subtitle = str_c(scenario, " Timeframe: ", horizon_years)
  
  
  # Plot the projected biomass
  projection_map <- ggplot() + 
    geom_sf(
      data = density_sf, 
      aes(geometry = geometry, fill = val, alpha = I(val_alpha)), color = "transparent")
  
  # Overlay the EPU bounds
  if(add_labels){projection_map <- projection_map + 
      geom_sf(data = epu_buff, fill = "transparent", color = "black", linewidth = 1)
    }
  
  # Finish the map
  projection_map <- projection_map + 
    geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
    scale_fill_carto_c(
      palette = "SunsetDark", 
      # palette = "ag_GrnYl", 
      trans = "log10", 
      limits = c(1, 10000),
      labels = scales::comma_format(accuracy = 1, suffix = " kg"), 
      na.value = "transparent",
      direction = 1) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle) +
    # # Right colorbar
    guides(
      fill = guide_colorbar(
        title = "Annual Mean\nBiomass Density\n(kg/km2)",
        title.position = "top",
        title.hjust = 0,
        title.vjust = 4,
        barheight = unit(3, "in"),
        barwidth = unit(1, "cm"),
        direction = "vertical",
        frame.colour = "black",
        ticks.colour = "black")) +
    theme_map(legend.position = "right")
  
  # EPU labels
  if(add_labels){projection_map <- projection_map + 
    geom_label(
      data = labels_df, 
      aes(x, y, label = label), 
      family = "Avenir", size = 5)
  }
  
  # Hague line
  if(add_labels == F){projection_map <- projection_map + 
    geom_text(
      data = data.frame(x = 880000, y = 4425000, label = "Hague\nLine"), 
      aes(x, y, label = label), size = 4, family = "Avenir") +
    geom_sf(data = hague_sf, color = "black", linewidth = 1, linetype = 3)}
  
  # Set CRS
  projection_map <- projection_map + coord_sf(
    xlim = c(-182500, 1550000), 
    ylim = c(3875000, 5370000) , 
    expand = F, crs = 32619)
  return(projection_map)
  
}



####  Biomass Density Change Map  ####



# Getting differences
get_difference <- function(base_dat, proj_dat){
  
  # Take info from baseline period and projection period
  # Remove things
  base_dat <- st_drop_geometry(base_dat) %>% select(-c(ref_period, temp_horizon, var))
  proj_dat <- st_drop_geometry(proj_dat) %>% select(-c(var, ref_period)) %>% rename(proj_val = val)
  
  # Join and get differences
  diff_dat <- left_join(base_dat, proj_dat) %>% 
    mutate(val_diff = proj_val - val)
  
  # Perform any calculations for data prep to keep out of mapping function
  
  
  # Add context for symbology
  diff_dat <- diff_dat %>% 
    mutate(
      # 1. Highlight cases of range expansion or decline
      range_shift = case_when(
        val < 1 & proj_val > 1 ~ "Range Gained",
        val > 1 & proj_val < 1 ~ "Range Lost",
        TRUE ~ NA),
      # Hide minor changes using alpha - should be done based off base biomass
      minor_change = case_when(
        between(val_diff, -5,5) ~ 0.3,
        between(val_diff, -10,10) ~ 0.4,
        between(val_diff, -100, 100) ~ 0.6,
        between(val_diff, -1000, 1000) ~ 1,
        val_diff > 1000 ~ 1,
        val_diff < -1000 ~ 1,
        TRUE ~ 0),
      # Fully Hide places that don't have measurable biomass in future state,
      val_diff = ifelse(proj_val < 1, NA, val_diff))
  
  
  # Join the polygons back in
  diff_dat <- diff_dat %>% 
    left_join(hex_grid, by = join_by(Lon, Lat, pt_id)) %>% 
    st_as_sf()
  return(diff_dat)
}




# Function to plot the differences
ssp_difference_map <- function(dist_df, reactive = F, range_shift = T){  # Color scale limit
  
  
  # Set the data
  density_sf <- dist_df
  if(reactive){density_sf <- dist_df()}
  
  # Get scenario/horizon from the data so we don't need to feed so many inputs
  one_rec <-  density_sf %>% slice(1)
  species <- one_rec %>% pull(comname)
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
  scenario <- ifelse(
    scenario == "CMIP6_SSP1_26", 
    "SSP1-2.6", 
    "SSP5-8.5")
  
  # Clean up horizon text
  horizon_choice <- str_c(str_remove(horizon, "C"), deg_c)
  
  # Prepare title text:
  plot_title <- str_c(str_to_title(species), " | Biomass Change with +", horizon_choice, " Climate")
  plot_subtitle = str_c(scenario, " Timeframe: ", horizon_years)
  
  
  # Limits need to match against something: set them based on quantile of absolute difference
  # Can we automatically set some scale for the color limits?
  col_lims <- c(-1,1) * quantile(abs(density_sf$val_diff), probs = c(0.85), na.rm = T)
  
  
  # Make the map - Log Differences
  diff_m <- ggplot() 
  
  
  # Range shift toggle
  if(range_shift){
    diff_m <- diff_m +
      geom_sf(
        data = density_sf, 
        aes(
          fill = val_diff, 
          alpha = I(minor_change),
          color = range_shift),
        linewidth = 0.8) +
      scale_color_manual(values = c(
        "Range Gained" = "#008080",
        "Range Lost" = "#CA562C"), 
        na.value = "transparent",
        na.translate = F) +
      guides(color = guide_legend(
        order = 2, 
        override.aes = (list(fill = "transparent", linetype = 1)))) +
      labs(color = "Range Shifts:")
    
    
  } else {
    # Behavior with no boundary color indication
    diff_m <- diff_m +
      geom_sf(
        data = density_sf, 
        aes(
          fill = val_diff, 
          alpha = I(minor_change)),
          color = "transparent",
        linewidth = 0.8) 
    }

    # Rest of map is unchanged
    diff_m <- diff_m +
      geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
      geom_sf(data = hague_sf, color = "black", linewidth = 1, linetype = 3) +
      geom_text(
        data = data.frame(x = 880000, y = 4425000, label = "Hague\nLine"), 
        aes(x, y, label = label), size = 4, family = "Avenir") +
      scale_fill_carto_c(
        palette = "Geyser", 
        labels = scales::comma_format(accuracy = 1, suffix = " kg"),
        limits = col_lims, 
        oob = oob_squish, 
        breaks = pretty_breaks(n = 5),
        na.value = "transparent",
        direction = -1) +
      coord_sf(
        xlim = c(-182500, 1550000), 
        ylim = c(3875000, 5370000) , 
        expand = F, crs = 32619) +
      theme_map() +
      guides(
        fill = guide_colorbar(
          order = 1,
          title = "Biomass Density\nChange (kg/km2)",
          title.position = "top",
          title.hjust = 0,
          title.vjust = 3,
          barheight = unit(3, "in"),
          barwidth = unit(1, "cm"),
          direction = "vertical",
          frame.colour = "black",
          ticks.colour = "black")) +
      theme(legend.position = "right")  +
      labs(
        title = str_c(str_to_title(species), " | Biomass Change at +", horizon_choice),
        subtitle = str_c("Estimated Years: ", horizon_years))
  
  # Review
  diff_m
  
  
}


#### Average Density Timelines  ####
ssp_projected_timeseries <- function(timeseries_data, reactive = F){
  
  # Set the data
  in_data <- timeseries_data
  if(reactive){in_data <- timeseries_data()}
  
  
  # Get scenario/horizon from the data so we don't need to feed so many inputs
  one_rec  <- in_data %>% slice(1)
  species  <- one_rec %>% pull(comname)
  scenario_choice <- one_rec %>% pull(scenario)
  horizon_choice  <- one_rec %>% pull(temp_horizon)
  
  # 2. Get the correct years for the temperature horizon:
  # Filter the limits out
  horizon_lims <- horizon_year_key_df %>% 
    filter(
      scenario == scenario_choice, 
      horizon == horizon_choice) %>% 
    mutate(mid_year = (xmin+xmax)/2,
           horizon_lab = str_c("+", str_remove(horizon_choice, "C"), deg_c))
  
  # Clean up scenario text
  scenario_choice <- ifelse(scenario_choice == "CMIP6_SSP1_26", "SSP1-2.6", "SSP5-8.5")
  
  
  # 2. Make a dataframe for labeling the horizon's time frame
  region_heights <- in_data %>% 
    # Get the max value in windows
    filter(year %in% seq(horizon_lims$xmin, horizon_lims$xmax, 1)) %>% 
    group_by(region) %>% 
    summarise(panel_height = max(avg_dens)) %>% 
    ungroup() %>% 
    bind_cols(., horizon_lims)
  
  # Add some columns for alpha and size
  in_data <- in_data %>% 
    mutate(
      season_alpha = ifelse(season == "Annual Average", 1, 0.7),
      season_lw    = ifelse(season == "Annual Average", 1.4, 0.8),
      season       = factor(season, levels = c("Annual Average", "Spring", "Summer", "Fall")))
  
  
  # Assemble the plot
  ggplot() +
    geom_rect(data = horizon_lims, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "gray90") +
    geom_text(data = region_heights, aes(x = mid_year, label = horizon_lab, y = panel_height), vjust = -1.4, size = 4, fontface = "bold") +
    geom_line(data = filter(in_data, season != "Annual Average"), aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha), color = season), key_glyph = "timeseries") +
    geom_line(data = filter(in_data, season == "Annual Average"), aes(year, avg_dens, group = season, linewidth = I(season_lw), alpha = I(season_alpha), color = season), key_glyph = "timeseries") +
    facet_wrap(~region, scale = "free_y") +
    scale_color_gmri() +
    scale_x_continuous(
      #limits = c(2020, 2115),
      limits = c(2020, 2105),
      breaks = pretty_breaks(),
      expand = expansion(add = c(0,0))) +
    scale_y_continuous(
      limits = c(0,NA), 
      expand = expansion(mult = c(0, 0.25)),
      breaks = pretty_breaks()) +
    guides(color = guide_legend(
      title.vjust = 0.5, nrow = 2, 
      override.aes = list(
        fill = "white", 
        linewidth = 1))) +
    theme_plot() +
    labs(
      y = "Average Biomass Density kg/km2", 
      x = "Year", 
      title = NULL,
      # title = str_to_title(species),
      color = str_c(str_to_title(species), " Seasonal Trajectory:"),
      subtitle = str_c("Projected Biomass Change Under ", scenario_choice, " Ensemble Scenario"))
  

}





#### Plot Species Preference Curves  ####

# This function requires: a dataframe with combined preferences with the environmental conditions
# species preference curve information
# name of species to filter
# and the accommpanying environmental state
plot_preference_curves <- function(pref_dat, reactive = F){
  
  # Set the data
  curve_dat <- pref_dat
  if(reactive){curve_dat <- pref_dat()}
  
  # Get scenario/horizon from the data so we don't need to feed so many inputs
  one_rec  <-  curve_dat %>% drop_na() %>%  slice(1)
  species  <- one_rec %>% pull(comname)
  scenario <- one_rec %>% pull(scenario)
  horizon  <- one_rec %>% pull(temp_horizon)
  period   <- one_rec %>% pull(ref_period)
  
  # Tidy text
  horizon <- str_c("+",str_sub(horizon, 1,-2), deg_c)
  
  # Clean up scenario text
  scenario <- ifelse(scenario == "SSP1_26", "SSP1-2.6", "SSP5-8.5")
  
  # Make the plot
  curve_dat <- curve_dat %>% mutate(temp_horizon = str_c("+",str_sub(temp_horizon, 1,-2), deg_c)) 
  
  # Get a distinct combo for labels so they only plot once
  dlabs <- filter(curve_dat, variable == "Depth") %>% 
    distinct(region, variable, val)
  vlabs <- filter(curve_dat, variable != "Depth") %>% 
    distinct(region, variable, val, temp_horizon)
  
  
  #Build the figure:
  curve_dat %>% 
    ggplot() +
    # Mark the preference curves
    geom_ribbon(aes(x = val_actual, ymin = 0, ymax = fit_exp), 
                color = "transparent", alpha = 0.3, fill = "#057872") +
    geom_line(aes(val_actual, fit_exp, group = comname), linewidth = 1) +
    # Label the regional averages
    geom_vline(
      data = dlabs, 
      aes(xintercept = val), 
      color = "black", linewidth = 1.2, show.legend = F) +
    geom_vline(
      data = vlabs, 
      aes(xintercept = val, color = temp_horizon), 
      linewidth = 1.2, key_glyph = draw_key_rect) +
    geom_label(
      data = dlabs, 
      aes(x = val, y = I(0.4), label = round(val)), 
      color = "black", show.legend = F, label.size = 1,
      label.padding = unit(0.7, "lines"), label.r = unit(0.5, "lines")) +
    geom_label(
      data = vlabs, 
      aes(x = val, y = I(.4), color = temp_horizon, label = round(val)), 
      key_glyph = draw_key_rect, label.size = 1,
      label.padding = unit(0.7, "lines"), label.r = unit(0.5, "lines")) +
    scale_color_manual(values = "#ea4f12", na.translate = F) +
    facet_grid(
      region~variable, 
      scales = "free",
      labeller = labeller(
        region = label_wrap_gen(10),
        variable = label_wrap_gen(10))) +
    scale_x_continuous(expand = expansion(add = c(0,0))) +
    scale_y_continuous(expand = expansion(mult = c(0,.2))) +
    guides(color = guide_legend(override.aes = list(fill = "#ea4f12"))) +
    theme_plot() +
    labs(
      title = "Habitat Preferences with Projected Climate Conditions", 
      subtitle = str_c(str_to_title(species), ":   ", horizon, " Climate (", scenario, ": ", period, ")"),
      color = "Regional Conditions for Future Climate of:",
      x = "Value", 
      y = "Biomass Density kg/km2")
}
