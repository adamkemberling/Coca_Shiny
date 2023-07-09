# Distribution Map Testing



####  Packages  ####
library(bslib)
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



#### Support Functions:  ####


# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))
land_wgs <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs4326.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))
hague_wgs <- read_sf(here::here("Data/spatial", "hagueline_crs4326.geojson"))

# Bathymetry
# Add the bottom contours:
# Or make terra for ggplot using tidyterra
bathy <- terra::rast("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")
bathy <- terra::rast(here::here("Data/spatial/etopo_bedrock_15arcsec.tiff"))



####________________####
####___ Testing using FishViz Data____####
####________________####
####  Data  ####

source(here::here("R/app_support.R"))

# Load Pre-Prepped App Data::
baseline_dataList <- fetch_appdata(data_resource = "baseline_data")



####  Data Prep  ####

# Test options
test_species <- "American Lobster"
display_option <- "baseline"

# Use one for testing
test_baseline <- baseline_dataList$American_lobster_ST_SSP5_85_mean_baseline.geojson

# the dumb meter grid
test_1 <- baseline_prep(baseline_dat = test_baseline)


# Make it 4326
test_2 <- st_transform(test_baseline)
test_3 <- st_transform(test_1)

# Square Options
ggplot(test_baseline) + geom_sf() + ggtitle("Before bsquare - crs32619") # Meh
ggplot(test_1) + geom_sf() + ggtitle("After bsquare - crs32619") # Don't Do
ggplot(test_2) + geom_sf() + ggtitle("Before bsquare - crs4326") # Meh
ggplot(test_3) + geom_sf() + ggtitle("After bsquare - crs4326") # Don't do

# Make a honeycomb grid
area_honeycomb_grid = st_make_grid(
  x = test_baseline, 
  cellsize = c(25000, 25000), 
  what = "polygons", 
  square = FALSE)

# Trim it?

# Literally so annoying, need to make it an sf class
# https://github.com/r-spatial/sf/issues/1148
clipped_comb <- area_honeycomb_grid %>%
  st_as_sf() %>% 
  st_filter(test_baseline) 

clipped_comb %>% 
  ggplot() + geom_sf()

# Or do it this way, so dumb
clipped_comb <- area_honeycomb_grid %>% st_as_sf() %>%
  filter(lengths(st_intersects(., test_baseline)) > 0) 
clipped_comb %>% 
  ggplot() + geom_sf()

####  HoneyComb  Testing  ####
# Putting the information into the comb? 
st_join(clipped_comb, test_baseline, join = st_intersects) %>% 
# st_join(clipped_comb, st_centroid(test_baseline), join = st_contains) %>%  Centroids create gaps
  ggplot() +
  geom_sf(aes(fill = Log_Biomass)) +
  geom_sf(data = land_wgs, color = "white", fill = "gray40") +
  geom_sf(data = hague_wgs, linewidth = 1, color = "black") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map()  +
  theme(legend.background = element_rect(color = "black"))



####  Projection Testing  ####


# build a base map in sf
ggplot() +
  geom_sf(data = test_2, aes(fill = Log_Biomass)) +
  geom_sf(data = land_wgs, color = "white", fill = "gray40") +
  geom_sf(data = hague_wgs, linewidth = 1, color = "black") +
  #coord_sf(xlim = c(-75.25, -63), ylim = c(35.5, 48) , expand = F, crs = 32619)
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619)

# Do points use the projected coordinates of coorD_sf()? YES okay...
ggplot() +
  geom_point(data = test_baseline, aes(x = Lon, y = Lat, color = Log_Biomass, fill = Log_Biomass), size = 2) +
  geom_sf(data = land_wgs, color = "white", fill = "gray40") +
  geom_sf(data = hague_wgs, linewidth = 1, color = "black") +
  #coord_sf(xlim = c(-75.25, -63), ylim = c(35.5, 48) , expand = F, crs = 32619)
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map()



# I think I want to explore using the points instead of rectangles, so 32619 it is
ggplot() +
  geom_point(data = test_baseline, aes(x = Lon, y = Lat, color = Log_Biomass), fill = "white", size = 2) +
  geom_sf(data = land_sf, color = "white", fill = "gray40") +
  geom_sf(data = hague_sf, linewidth = 1, color = "black") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map() +
  theme(legend.background = element_rect(color = "black"))



#  Speed Check on ggplot + sf:
profvis(expr = {
  sf_minimum <- ggplot() +
    geom_point(data = test_baseline, aes(x = Lon, y = Lat, color = Log_Biomass), fill = "white", size = 2) +
    geom_sf(data = land_sf, color = "white", fill = "gray40") +
    geom_sf(data = hague_sf, linewidth = 1, color = "black") +
    coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
    theme_map() +
    theme(legend.background = element_rect(color = "black"))
  print(sf_minimum)
})


####  Foreground Color testing  ####

# Light foreground testing
RColorBrewer::display.brewer.all() # Palettes

# I think I want to explore using the points instead of rectangles, so 32619 it is
ggplot() +
  geom_point(data = test_baseline, aes(x = Lon, y = Lat, fill = Log_Biomass), 
             color = "gray40", size = 2.5, shape = 21) +
  geom_sf(data = land_sf, color = "white", fill = "gray45") +
  geom_sf(data = hague_sf, linewidth = 1, fill = "black") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  #scale_fill_distiller(palette = "YlGnBu", direction = -1) +
  scale_fill_distiller(palette = "PuBuGn", direction = 1) +
  #scale_fill_distiller(palette = "OrRd", direction = 1) + # For dark BG
  theme_map() +
  theme(legend.background = element_rect(color = "black"))



# Dark Foreground

# I think I want to explore using the points instead of rectangles, so 32619 it is
ggplot() +
  geom_point(data = test_baseline, aes(x = Lon, y = Lat, fill = Log_Biomass), 
             color = "gray40", size = 2.5, shape = 21) +
  geom_sf(data = land_sf, color = "gray90", fill = "gray40") +
  geom_sf(data = hague_sf, linewidth = 1, fill = "black") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  scale_fill_distiller(palette = "YlGnBu", direction = -1) +
  # scale_fill_distiller(palette = "OrRd", direction = 1) + # For dark BG
  theme_map() +
  theme(
    legend.background = element_rect(color = "black"), 
    panel.background = element_rect(fill = "gray20", color = "gray90"))



##### Log Colorscale testing  ####

# Looks better this way: For Baseline atleast
#scale_color_distiller(palette = "RdYlBu", trans = "log10", labels = scales::label_log(base = 10)) +



#### Bathymetry Testing  ####

display.brewer.all() 


# Expand color palette for depth:
# https://r-graph-gallery.com/40-rcolorbrewer-get-a-longer-palette.html

# Classic palette BuPu, with 4 colors
coul <- brewer.pal(4, "Greys") 

# Add more colors to this palette :
coul <- colorRampPalette(coul)(25)

# remove values on land
bathy_2 <- clamp(x = bathy, lower = -6000, upper = 0, values = TRUE)
bathy_2 <- clamp(x = bathy_2, lower = -2000, upper = 0, values = FALSE)

# Play around...
ggplot() +
  tidyterra::geom_spatraster_contour_filled(
    data = bathy_2,
    breaks = c(0, -10, -50, -100, -250, -500, -1000, -2000, -4000, -6000, -10000),
    alpha = 1,
    linewidth = 0.25, 
    color = "transparent"
    #color = "gray20"
  ) +
  scale_fill_brewer(palette = "Greys", direction = 1) +
  geom_point(data = test_baseline, aes(x = Lon, y = Lat, color = Log_Biomass), size = 1.5) +
  geom_sf(data = land_sf, color = "gray90", fill = gmRi::gmri_cols("blue"), linewidth = 0.25) +
  #geom_sf(data = land_sf, color = "gray90", fill = "gray20", linewidth = 0.25) +
  geom_sf(data = hague_sf, linewidth = 1, fill = "black") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3985000, 5370000) , expand = F, crs = 32619) +
  scale_color_distiller(palette = "OrRd", direction = 1) +
  theme_map() +
  guides(
    fill = guide_colorsteps(
      title = "Depth",
      title.position = "top",
      title.hjust = 0.5, 
      barwidth = unit(3, "in"),
      barheight = unit(0.6, "cm"),  show.limits = T,
      direction = "horizontal",
      frame.colour = "black", 
      ticks.colour = "black"),
    color = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5, 
      barwidth = unit(3, "in"),
      barheight = unit(0.6, "cm"), 
      direction = "horizontal",
      frame.colour = "black", 
      ticks.colour = "black")) +
  theme(legend.background = element_rect(color = "black"),
        legend.position = "bottom")





####  Climate Projection Maps  ####

# Projection Data:
diff_dataList <- fetch_appdata(data_resource = "projected_data")
diff_test <- diff_dataList$American_lobster_ST_SSP5_85_mean_projected.geojson







#### Difference Maps  ####



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
  "C2_C1" = "#01006d"
)

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


pal<-c(
  "A2_A1"="#e8e8e8",
  "A2_B1"="#d3a7cb",
  "A2_C1"="#be64ac",
  "B2_A1"="#a6d9d9", 
  "B2_B1"="#a6a7cb",
  "B2_C1"="#a664ac",
  "C2_A1"="#5ac8c8",
  "C2_B1"="#5aa7c8", 
  "C2_C1"="#5a64ac"
)

#### BiVariate legend  ####


tib<-tibble(
  x1 = rep(c("A","B","C"),3),
  y2 = c(rep("A",3),
         rep("B",3),
         rep("C",3)),
  value = glue::glue("{y2}2_{x1}1")
)

leg <- ggplot(data=tib,aes(x=y2,y=x1,fill=value))+
  #geom_point(pch=21,size=60,color="grey90")+
  geom_tile()+
  geom_text(aes(label=value),size=10)+
  # scale_fill_manual(values=pal)+
  #scale_fill_manual(values=gmri_bipal)+
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



####  Prep Seasonal Comparison  ####

# So here is the general setup
# Grading 2 Biomass variables (Summer & Fall)
# into 3 bins (1, 2, & 3) for low-medium-high
# These are benchmarked against the biomass during the baseline period, at a point in space


# So for testing: take two species from the fishviz projections or two seasons
# can maybe difference against the baseline?

# Species Test


# Single Species Projections
species_1 <- diff_dataList$Yellowtail_flounder_ST_SSP5_85_mean_projected.geojson  %>% 
  select(shape_id = V1, Species, Season, Decade, Biomass, geometry) 

# Single Species Baseline
species_1_base <- baseline_dataList$Yellowtail_flounder_ST_SSP5_85_mean_baseline.geojson %>% 
  #st_drop_geometry() %>% 
  select(shape_id = V1, Species, Season, base_bio = Biomass, geometry) %>% 
  drop_na()


####  Separate Grid from the Data  ####

# Pull centerpoints from baseline, apply the ID's to both baseline and projections



# Use centerpoints to label the mesh cells
label_centers <- function(baseline_sf, projection_sf){
  
  # Centerpoints from baseline
  centerpoints <- baseline_sf %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
 
   # Distinct matches
  cp_distinct <- centerpoints %>% distinct(X, Y) %>% mutate(cell_id = str_c("cell_", row_number()))
  
  # Apply them to both
  baseline_sf <- baseline_sf %>% 
    mutate(X = centerpoints$X, Y = centerpoints$Y) %>% 
    left_join(cp_distinct)
  
  # Do it again with projection
  projection_cps <- projection_sf %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
  projection_sf <- projection_sf %>% 
    mutate(X = projection_cps$X, Y = projection_cps$Y) %>% 
    left_join(cp_distinct)
  
  
  # Just the cell_id's and the geometry
  grid_sf <- baseline_sf %>% distinct(cell_id, geometry)
  
  return(list("baseline" = baseline_sf, "projection" = projection_sf, "cpoints" = cp_distinct, "grid" = grid_sf))
}

# Label the centerpoints
centers <- label_centers(baseline_sf = species_1_base, 
                         projection_sf = species_1)




####  Bivariate Normalization  ####

# This step is where the data from two baselines (Fall/Spring) or SSP1 and SSP5
# Or some other comparison feature, need to be coded into 9 levels
# Use centerpoints as the shape id: cell_id from label_centers

# pull the pieces:
x_proj <- centers$projection  %>% st_drop_geometry()
x_base <- centers$baseline  %>% st_drop_geometry()
x_cent <- centers$cpoints
x_grid <- centers$grid


# # Visualise to verify that the grid exists
x_grid  %>% 
  ggplot() +
  geom_sf()


# Get difference between projection and base and scale
x_both <- left_join(
  x = x_proj %>% distinct(cell_id, Species, Season, Decade, Biomass), 
  y = x_base %>% distinct(cell_id, Species, Season, base_bio), 
  by = join_by(cell_id, Species, Season)) %>% 
  group_by(cell_id, Species, Season, Decade) %>% 
  mutate(decade_diff = Biomass - base_bio) %>% 
  summarise(
    decade_relative = case_when(
      decade_diff < 0.50 * base_bio ~ "decline",
      decade_diff > 1.50 * base_bio ~ "increase",
      TRUE ~ "similar"),
    .groups = "drop")



# From here I think we can pivot the seasons (after dropping spring), 
# Lastly: Add the geometry back

x_diffs <- x_both %>% 
  pivot_wider(values_from = "decade_relative", names_from = "Season") %>% 
  left_join(x_grid) %>% 
  st_as_sf()
  


# Use {glue} to pair the changes in two or more variables to match the
# palette color names
clean <- x_diffs %>% 
  mutate(
    # Summer Change: y axis, high medium low
    summer_lab = case_when(
      Summer == "decline" ~ "lowY",
      Summer == "similar" ~ "midY",
      Summer == "increase" ~ "highY"
    ),
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


####  Bivariate Map  ####
bivar_map <- st_join(clipped_comb, clean, join = st_intersects) %>% 
  filter(Decade == "2030") %>% 
  ggplot() +
  geom_sf(aes(fill = bivar_lab), show.legend = F) +
  geom_sf(data = land_sf, color = "white", fill = "gray40") +
  geom_sf(data = hague_sf, linewidth = 1, color = "black") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map() +
  scale_fill_manual(values = gmri_bipal_2) +
  labs(title = "Yellowtail Flounder 2030:\nBivariate Seasonal Effect Map")


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



# Put the map and legend together:

# Basic use
bivar_map + inset_element(leg, 
                          left = 0.65, 
                          bottom = 0.05,
                          right = 0.95,
                          top = 0.35)




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


