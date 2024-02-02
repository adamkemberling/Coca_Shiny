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
library(showtext)


# Lets turn some fonts on to match the theme:

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
# Handles log10 transformation in positive and negative values
divergent_l10_trans <- scales::trans_new(
  name = "signed_log",
  transform = function(x) sign(x)*log10(abs(x)),
  inverse = function(x) sign(x)*10^abs(x))




####  Projection Data Preparation  ####


# Unique locations to map
pt_latlon <- read_csv(
  file = here::here("Data/spatial/unique_location_coords.csv"),
  col_types = cols(
    Lon = col_double(),
    Lat = col_double(),
    pt_id = col_double()
  ))


# Annual Averages and the Baseline Difference for 2050 & 2100
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


# st_write(hex_grid, dsn = here::here("Data/spatial/hex_grid.geojson"))




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



#####__________________####
#####  Seasonal Maps  ####



#####__________________####
#####  Bivariate Maps  ####





####  BiVariate Color Palettes  ####
# Bivariate colour bar
# Toool for making a palette
#https://observablehq.com/@benjaminadk/bivariate-choropleth-color-generator
# Good article:
# https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/ 

# Palette Option 1
pal <- c(
  "A2_A1" = "#e8e8e8",
  "A2_B1" = "#e89abf",
  "A2_C1" = "#e8006d",
  "B2_A1" = "#8bd7e8",
  "B2_B1" = "#8b9abf",
  "B2_C1" = "#8b006d",
  "C2_A1" = "#01bee8",
  "C2_B1" = "#019abf",
  "C2_C1" = "#01006d")

# Palette 2
pal<-c(
  "A2_A1"="#e8e8e8",
  "A2_B1"="#d3a7cb",
  "A2_C1"="#be64ac",
  "B2_A1"="#a6d9d9", 
  "B2_B1"="#a6a7cb",
  "B2_C1"="#a664ac",
  "C2_A1"="#5ac8c8",
  "C2_B1"="#5aa7c8", 
  "C2_C1"="#5a64ac")

# GMRI color palette using blue and yellow
gmri_bipal <- c(
  "A2_A1" = "#e8e8e8", # 3x1 # position on the legend row x col
  "A2_B1" = "#74a2b6", # 2x1
  "A2_C1" = "#005b84", # 1x1
  "B2_A1" = "#e4d680", # 3x2
  "B2_B1" = "#729664", # 2x2
  "B2_C1" = "#005549", # 1x2
  "C2_A1" = "#dfc000", # 3x3
  "C2_B1" = "#708700", # 2x3
  "C2_C1" = "#004c00"  # 1x3
)

gmri_bipal_2 <- c(
  "#e8f7ff", 
  "#a0d5fa", 
  "#53b1f5", 
  "#e8c694", 
  "#a0c694",
  "#53b194", 
  "#e88a11", 
  "#a08a11", 
  "#538a11"
)




# Table for building the legend
tib<-tibble(
  x1 = rep(c("A","B","C"),3),
  y2 = c(rep("A",3),
         rep("B",3),
         rep("C",3)),
  value = glue::glue("{y2}2_{x1}1")
)


# What a legend could look like
leg <- ggplot(data=tib,aes(x=y2,y=x1,fill=value))+
  #geom_point(pch=21,size=60,color="grey90")+
  geom_tile()+
  # geom_text(aes(label=value),size=10)+
  scale_fill_manual(values=gmri_bipal_2)+
  guides(fill="none") +
  labs(x = "\u2190                      Fall Biomass                     \u2192", 
       y = "\u2190                      Summer Biomass                     \u2192") +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(), panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_text(face = "bold", size = 16))

leg








####  Prep Data Across Projections  ####

# So here is the general setup
# Grading 2 Biomass variables (Summer & Fall)
# into 3 bins (1, 2, & 3) for low-medium-high
# These are benchmarked against the biomass during the baseline period, at a point in space



# Species Test


# Single Species Projections
species_1 <- seasonal_species_i %>% 
  filter(season %in% c("Summer", "Fall")) 



####  Bivariate Normalization  ####

# This step is where the data from two baselines (Fall/Spring) 
# Or SSP1 and SSP5 Projections
# OR 2 of whatever you are comparing.

# Then, need to be coded into 9 levels
# Use pt_id to scale for each location

# Get difference between projection and base and scale
bivar_season <- species_1 %>% 
  group_by(pt_id, species, season, scenario) %>% 
  summarise(
    decade_relative = case_when(
      diff_2050_z < -1  ~ "decline",
      diff_2050_z > 1   ~ "increase",
      TRUE ~ "similar"),
    .groups = "drop")



# From here I think we can pivot the seasons (after dropping spring), 
x_diffs <- bivar_season %>% 
  pivot_wider(values_from = "decade_relative", names_from = "season") 

# Lastly: Add the geometry back
# Add the geometry back on before plotting
x_diffs <- x_diffs %>% 
  left_join(hex_grid) %>% 
  st_as_sf()



# Use {glue} to pair the changes in two or more variables to match the
# palette color names
clean <- x_diffs %>% 
  mutate(
    # Summer Change: y axis, high medium low
    summer_lab = case_when(
      Summer == "decline" ~ "lowY",
      Summer == "similar" ~ "midY",
      Summer == "increase" ~ "highY"),
    # F Change: x axis, high medium low
    fall_lab = case_when(
      Summer == "decline" ~ "lowX",
      Summer == "similar" ~ "midX",
      Summer == "increase" ~ "highX"),
    bivar_lab = glue::glue("{fall_lab}_{summer_lab}")
  )

# Make the stupid names more clear:
# X and Y are the coordinates on the 3x3 legend, "low" is bottom row and left side 
gmri_bipal_2 <- c(
  "lowX_lowY" = "#e8f7ff", 
  "lowX_midY" = "#a0d5fa", 
  "lowX_highY" = "#53b1f5", 
  "midX_lowY" = "#e8c694", 
  "midX_midY" = "#a0c694",
  "midX_highY" = "#53b194", 
  "highX_lowY" = "#e88a11", 
  "highX_midY" = "#a08a11", 
  "highX_highY" = "#538a11"
)


####  Bivariate Legend  ####


# Build the legend:
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
  scale_fill_manual(values=gmri_bipal_2)+
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






#### SSP1 Bivariate  ####

(bivar_map_ssp1 <-  clean %>% 
   filter(str_detect(scenario, "CMIP6_SSP1")) %>% 
   ggplot() +
   geom_sf(aes(fill = bivar_lab), show.legend = F) +
   geom_sf(data = land_sf, color = "white", fill = "gray40") +
   geom_sf(data = hague_sf, linewidth = 1, color = "black") +
   coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
   theme_map() +
   scale_fill_manual(values = gmri_bipal_2) +
   labs(title = str_c("SSP1: ", species_choice, "\nBivariate Seasonal Effect Map for 2050")))

# Put Map and Legend Together
bivar_map_ssp1 + inset_element(leg, 
                               left = 0.65, 
                               bottom = 0.05,
                               right = 0.95,
                               top = 0.35)


####  SSP5 Bivariate  ####
(bivar_map_ssp5 <-  clean %>% 
    filter(str_detect(scenario, "CMIP6_SSP5")) %>% 
    ggplot() +
    geom_sf(aes(fill = bivar_lab), show.legend = F) +
    geom_sf(data = land_sf, color = "white", fill = "gray40") +
    geom_sf(data = hague_sf, linewidth = 1, color = "black") +
    coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
    theme_map() +
    scale_fill_manual(values = gmri_bipal_2) +
    labs(title = str_c("SSP5: ", species_choice, "\nBivariate Seasonal Effect Map for 2050")))


# Put Map and Legend Together
bivar_map_ssp5 + inset_element(leg, 
                               left = 0.65, 
                               bottom = 0.05,
                               right = 0.95,
                               top = 0.35)









####_________####
# Un developed


####  Leaflet Maps - Data not hooked up  ####


####  Interactive Base Map  ####

# # Plotly plot for space
# plotly_widget <- plot_ly(x = diamonds$cut, type = "histogram") %>%
#   config(displayModeBar = FALSE) %>%
#   layout(margin = list(t = 0, b = 0, l = 0, r = 0))


# # Set up the basemap to lay data on:
# leaflet_widget <- leaflet() %>%
#     addProviderTiles(providers$Esri.WorldImagery ) %>% 
#     # addProviderTiles(providers$Esri.WorldGrayCanvas ) %>% 
#     # addProviderTiles(providers$OpenStreetMap ) %>% 
#     setView(lng = -68.7, lat = 42.3, zoom = 5)  %>%
#     addMiniMap()






# Adding sf objects to it

# Demo baseline:
lob_demo <- read_sf(here::here("Data/demos/lobster_baseline.geojson"))
baseline_prep(lob_demo)


# Palette Functions
fill_palette <- colorNumeric(
  #palette = "viridis",
  palette = "Spectral",
  domain = c(0, max(lob_demo$Log_Biomass)), reverse = T)



# Trying hover labels
lobster_map <- leaflet_widget %>% 
  addPolygons(
    data = st_transform(lob_demo, st_crs(4326)), 
    color = ~fill_palette(Log_Biomass), 
    weight = 0.5, 
    label = ~round(Log_Biomass,1)) %>% 
  addLegend(position = "bottomleft",
            title = "Log(Biomass)",
            pal = fill_palette, 
            values = c(0, max(lob_demo$Log_Biomass)))





####  TMAP  ####
library(tmap)




# Should be able to juest run with it




####  Colourist Maps  ####
library(colorist)
library(gmRi)

# load example data, field sparrow occurrence probability
#data("fiespa_occ")
# Should be a raster stack
# Could do different decades to show movement?

# Lets see if Andrew has some stacks handy, na
coca_path <- cs_path("mills", "Projects/COCA19_Projections/projections")
lob_ssp1 <- readRDS(str_c(coca_path, "Lobster_full_CMIP6_SSP1_26_mean_projections.rds"))
lob_ssp1[[1]] %>% names()
raster::stack()


diff_dataList

# calculate distribution metrics
r <- metrics_distill(fiespa_occ)

# generate hcl color palette
pal <- palette_timecycle(fiespa_occ)

# map
map_single(r, pal, lambda_i = -2)


