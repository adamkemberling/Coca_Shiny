# SDM App Support Functions



####  Raw Materials  ####

# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))



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



# Divergent log10 transformation
# Handles log10 transformation in positive and negative values
divergent_l10_trans <- scales::trans_new(
  name = "signed_log",
  transform = function(x) sign(x)*log10(abs(x)),
  inverse = function(x) sign(x)*10^abs(x))


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


# Function for mapping the baseline conditions
map_baseline <- function(species_choice, ssp_scenario){
  
  
  
  
  
}


# My bsquare
bSquare <- function(x, a, coords = c("x", "y")) {
  a <- sqrt(a) / 2 # Get square root of area to get a length for buffer distance
  x_temp <- sf::st_as_sf(x, coords = coords, crs = 4326, remove = FALSE)
  x <- st_transform(x_temp, crs = 32619) #%>% drop_na(Value)
  x <- x %>%
    mutate(., geometry = sf::st_buffer(geometry,
                                       dist = a,
                                       nQuadSegs = 1,
                                       endCapStyle = "SQUARE"
    ))
  return(x)
}

