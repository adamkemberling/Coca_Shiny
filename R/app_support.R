# SDM App Support Functions




####  Themes  ####


# Unicode for degrees
deg_sym <- "\u00b0" 
deg_c <- "\u00b0C"
deg_f <- "\u00b0F"

# Plotting map theme
theme_map <- function(guides = T, ...){
  list(
    # Theme options, with ellipse to add more
    theme(
      
      # Titles
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      
      # Grids and Axes
      panel.background = element_blank(), 
      panel.border = element_rect(color = "black", fill = "transparent"), 
      panel.grid.major = element_line(color = "gray80"),
      #axis.text.x=element_blank(), 
      #axis.text.y=element_blank(), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks=element_blank(),
      plot.margin = margin(t = 10, r = 2, b = 0.1, l = 2, unit = "pt"),
      legend.position = c(.725, .125), 
      legend.background = element_rect(color = "transparent", fill = "white", 
                                       size = 0.25),
      
      # Use ellipses for tweaks on the fly:
      ...)#,
    
    # # Guide design
    # guides(
    #   fill = guide_colorbar(
    #     title.position = "top",
    #     title.hjust = 0.5, 
    #     barwidth = unit(2, "in"),
    #     barheight = unit(0.6, "cm"), 
    #     direction = "horizontal",
    #     frame.colour = "black", 
    #     ticks.colour = "black"),
    #   color = guide_colorbar(
    #     title.position = "top",
    #     title.hjust = 0.5,
    #     barwidth = unit(2, "in"),
    #     barheight = unit(0.6, "cm"),
    #     direction = "horizontal",
    #     frame.colour = "black",
    #     ticks.colour = "black"))
  )
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




fetch_appdata <- function(data_resource = NULL, testing = T){
  
  
  # 1. Path to the inputs for the app
  project_path <- ifelse(
    test = testing,
    # Copy of Fishviz Data:
    here::here("fishvis_data/Data"),
    # Path to box where Andrew is putting outputs
    project_path <- NULL)
  
  
  # 2. Append with data resource to make folder for the specific resource
  resource_folder <- str_c(project_path, data_resource, sep = "/")
  
  
  
  # 3. List the files and name them
  resource_files <- list.files(
    resource_folder, full.names = TRUE) %>% 
    setNames(list.files(resource_folder, full.names = FALSE))
  
  
  # Read them all
  fextension <- switch(
    EXPR = data_resource,
    "baseline_data"   = ".geojson",
    "ssp1"            = ".geojson",
    "ssp2"            = ".csv",
    "center_biomass"  = ".csv",
    "projected_data"  = ".geojson"
  )
  
  # Read spatial files
  if(fextension == ".geojson"){
    resource_files <- map(resource_files, read_sf)
  }
  
  # Read spatial files
  if(fextension == ".csv"){
    resource_files <- map(resource_files, read_csv, col_types = cols())
  }
  
  
  # Return the files
  return(resource_files)
  
}






# Convert points in sf polygon for plotting so the corners line up
bSquare <- function(x, a, coords = c("x", "y")) {
  a <- sqrt(a)/2
  x <- sf::st_as_sf(x, coords = coords, crs = 32619, remove = FALSE) %>% drop_na(Log_Biomass)
  x <- mutate(x, geometry = sf::st_buffer(geometry, 
                                          dist = a, 
                                          nQuadSegs=1, 
                                          endCapStyle = "SQUARE"))
  return(x)
}


# Baseline data prep:
# Cleans up geojson to minimum needs for map
# drops un-needed columns
baseline_prep <- function(baseline_dat){
  baseline_prepped <- baseline_dat %>% 
    mutate(Season = factor(Season, levels = c("Spring", "Summer","Fall"))) %>% 
    bSquare(25000*25000, coords = c("Lon", "Lat")) # 
  
  # Drop columns we don't need
  baseline_prepped <- baseline_prepped%>% 
    select(Lon, Lat, Season, Climate_Scenario, Species, Log_Biomass, geometry)
  
  return(baseline_prepped)
}