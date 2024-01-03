####  Putting EPUs on map without coastlines  ####
# Want to display EPU's and labels in a non-cluttered and fast way:



####  Packages  ####
library(here)
library(sf)
library(scales)
library(RColorBrewer)
library(rcartocolor)
library(patchwork)
library(rnaturalearth)
library(showtext)
library(tidyverse)
library(gmRi)
library(ggtext)

####  App Functions  ####
source(here::here("Coca_SDM_app_dev/app_functions/coca_application_funs.R"))


####   Spatial Components  ####

# Hexagonal grid simple feature geometry
hex_grid <- read_sf(here::here("Data/spatial/hex_grid.geojson"))

# Make a random color for fill
hex_grid <- hex_grid %>% mutate(
  fill_val = rnorm(nrow(hex_grid), mean = 20, sd = 15),
  fill_val = (fill_val * Lat)/40)


# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))



# 4. Ecological Production Units
epu_sf  <- ecodata::epu_sf
gom_epu <- epu_sf %>% filter(EPU == "GOM")
gb_epu  <- epu_sf %>% filter(EPU == "GB")
ss_epu  <- epu_sf %>% filter(EPU == "SS")
mab_epu <- epu_sf %>% filter(EPU == "MAB")


# Katies's EPU's
epu_buff <- read_sf(here::here("Data/spatial/EPU_inshorebuffer/EPU_inshorebuffer.shp"))
epu_buff <- read_sf(here::here("Data/spatial/EPU_inshorebuffer/EPU_inshorebuffer.shp"))
epu_buff <- read_sf(here::here("Data/spatial/EPU_Separated_Buffer2/EPU_Separated_Buffer2.shp"))


####  How do they look as is  ####
# Takes forrrrever if I widen the line widths


# Original EPU's
ggplot() +
  geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
  #geom_sf(data = hague_sf, color = "black", linewidth = 1) +
  geom_sf(data = hex_grid, aes(fill = fill_val), color = "transparent") +
  geom_sf(data = epu_sf, fill = "transparent", color = "gray40") +
  scale_fill_carto_c(palette = "SunsetDark") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map()


# Buffered EPU's
ggplot() +
  geom_sf(data = hex_grid, aes(fill = fill_val), color = "transparent") +
  geom_sf(data = epu_buff, fill = "transparent", color = "black", linewidth = 1) +
  geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
  scale_fill_carto_c(palette = "SunsetDark") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map()


####  Identifying Verices to Label  ####
# Want to point off to some area

# Really annnoying way to get the vertex with the minimum X/Y Coordinate
epu_buff %>% filter(EPU == "GB") %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(avg = (abs(X)+Y)/2) %>% 
  arrange(avg) %>% 
  slice(1) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 32619) %>% 
  st_coordinates()

labels_df <- tibble(
  x = c(280000, 700000, 420000, 945000),
  y = c(4200000, 4400000, 4950000, 4600000),
  hjust = c(0),
  vjust = c(0),
  angle = c(0),
  # color = c("white"),
  # fill = c("#00736D"),
  label = c("Mid-Atlantic<br>Bight", "Georges Bank", "Gulf of<br>Maine", "Scotian Shelf")
)



# Buffered EPU's
ggplot() +
  geom_sf(data = hex_grid, aes(fill = fill_val), color = "transparent") +
  geom_sf(data = epu_buff, fill = "transparent", color = "black", linewidth = 1) +
  geom_sf(data = land_sf, color = "gray95", fill = "gray40", linewidth = 0.15) +
  geom_richtext(data = labels_df, aes(x, y, label = label), family = "Avenir", size = 4) +
  scale_fill_carto_c(palette = "SunsetDark") +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
  theme_map()

