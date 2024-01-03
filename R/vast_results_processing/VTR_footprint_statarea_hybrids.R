# Footprint and VTR intersections:
# 


####  Libraries  ####
library(sf)
library(here)
library(rnaturalearth)
library(gmRi)
library(tidyverse)
library(patchwork)
library(smoothr)
library(gt)

# Path to research folder
res_path <- cs_path("res")



####  Spatial Files  ####


# 1. Statistical Zones
stat_zones <- read_sf(str_c(res_path, "Shapefiles/Statistical_Areas/Statistical_Areas_2010_withNames.shp"))

# # Restrict them to ones were using
# our_zones <- c(511,512,513,514,521,537,538,539,612,613,614,615,616,621,622,625,626,631)
# stat_zones <- stat_zones %>% filter(Id %in% our_zones)
# 
# # Label the groups we are thinking about clustering
# # Split them into a list
# stat_zones <- stat_zones %>% 
#   mutate(
#     comm_zones = case_when(
#       Id %in% c(511) ~ "Downeast Maine",
#       Id %in% c(512) ~ "Midcoast Maine",
#       Id %in% c(513) ~ "Southern Maine",
#       Id %in% c(514) ~ "Northern MA",
#       Id %in% c(521) ~ "Cape Cod",
#       Id %in% c(537:539) ~ "Southern MA / RI",
#       Id %in% c(612:616) ~ "NY / NJ",
#       Id %in% c(621,622, 625, 626, 631) ~ "NC / VA / DE",
#       TRUE ~ NA))


# Load the footprints
fprints <- read_rds(here::here("Data/spatial/Community_Footprints.RDS"))


# Format as sf
fprints <- fprints[["polygons"]] %>% setNames(fprints$names)
fprints <- imap(fprints, ~st_as_sf(.x) %>% 
                  st_union() %>% 
                  st_as_sf() %>%  
                  mutate(community_id = .y) %>% 
                  rename(geometry = x))




####  Overlay Check  ####
# So for this check we just want to return the stat areas that intersect, we don't need to
# pull the actual areas out here. keep it simple

"My only question is whether you thought about footprints that might span multiple stat 
areas—thinking about places like New Bedford and Portland.  
Did you consider that option, and would you then use all intersected stat areas?  
I think that approach could work, or we could implement a multi-step decision process such as:

--if footprint intersects one stat area, use stat area

--if footprint intersects more than one stat area and spans >20 grid cells, 
use footprint (note: 20 is completely arbitrary in this example)

--if footprint intersects more than one stat area and spans <20 grid cells, use stat area 
representing the largest portion of the footprint (this would need more thinking but just 
tossing an idea out to complete the logic structure)

 "

sf::sf_use_s2(FALSE)

# Do an intersection

get_overlap_ids <- function(fprint){
  st_join(
    st_transform(fprint, st_crs(stat_zones)),
    stat_zones,
    # fprint, 
    # st_transform(stat_zones, st_crs(fprint)),
    suffix = st_intersects) %>% 
    st_drop_geometry() %>% 
    distinct(Id) %>% 
    pull() 
}



# Table of overlaps
num_intersects <- map_dfr(fprints, 
        ~get_overlap_ids(.x) %>% length() %>% as.data.frame() %>% setNames("Stat-Area Intersections"), 
        .id = "Community") %>% 
  arrange(`Stat-Area Intersections`)

num_intersects %>% 
  gt::gt()



####  Hole Filling  ####

fprints$`PORTLAND, ME` %>% 
  fill_holes(threshold = units::set_units(100000, km^2)) %>%
  #smooth(method = "spline", n = 200) %>% 
  ggplot() +
  geom_sf() + 
  geom_sf(data = fprints$`PORTLAND, ME`, color = "orange", fill = NA) +
  labs(title = "Portland, ME: Hole fill")




####  Gatting Area of a Square  ####
# Stonington has 6
plot(fprints$`STONINGTON, ME`$geometry)
st_area(fprints$`STONINGTON, ME`) / 6

# Whats the order of size
size_order <- map_dfr(fprints, ~data.frame("area" = st_area(.x)), .id = "footprint") %>% arrange(desc(area))






####  Mapping pairs  ####
# This section should be a function that takes a name of a footprint
# checks the relevant stat areas, and plots both
# this is for verification
# the first step could be pulled out as its own thing and used here and for pulling them to run the
# processed timeseries


map_overlaps <- function(fprint){
  # Get the overlaps
  test_overlaps <- get_overlap_ids(fprint)
  
  # plot some of them
  stat_zones %>% 
    filter(Id %in% test_overlaps) %>% 
    ggplot() +
    geom_sf() +
    geom_sf(data = fprint %>% 
              fill_holes(threshold = units::set_units(100000, km^2)), 
            color = "orange", fill = "transparent", linewidth = 1) +
    labs(title = fprint$community_id)
}







# Here is the list:
num_intersects %>% gt::gt()


# Run all the overlaps so we can view them
overlap_maps <- map(fprints, map_overlaps)



# Look at them in order of number of intersects
overlap_maps$`STONINGTON, ME`
overlap_maps$`SCITUATE, MA`
overlap_maps$`PORTSMOUTH, NH`
overlap_maps$`NEW LONDON, CT`
overlap_maps$`CHATHAM, MA`
overlap_maps$`PORTLAND, ME`
overlap_maps$`STONINGTON, CT`
overlap_maps$`BOSTON, MA`
overlap_maps$`POINT PLEASANT, NJ`
overlap_maps$`MONTAUK, NY`
overlap_maps$`GLOUCESTER, MA`
overlap_maps$`NEWPORT NEWS, VA`
overlap_maps$`CAPE MAY, NJ`
overlap_maps$`POINT JUDITH, RI`
overlap_maps$`NEW BEDFORD, MA`




# Problem children
((map_overlaps(fprints$`STONINGTON, ME`)   + labs(title = "Desired Behavior", subtitle = "Stonington, ME")) + 
  (map_overlaps(fprints$`SCITUATE, MA`)   + labs(title = "Unintended Over-reach", subtitle = "Scituate, MA"))) /
  ((map_overlaps(fprints$`PORTSMOUTH, NH`) + labs(title = "Borderline", subtitle = "Portsmouth, NH")) +
  (map_overlaps(fprints$`NEW BEDFORD, MA`) + labs(title = "Not Needed - Large Footprints", subtitle = "Remaining 13 Ports")) ) +
  patchwork::plot_annotation(tag_levels = 'A')


size_order %>% gt()



####  Decision Making  ####
# So it seems like we get more than we bargained for with larger footprints
# Good example is all of chesapaeake bay with cape may:

# Rule based:

# Type 1: Footprint size
# If footprint area was less than __ we used the intersecting statistical zones

# Type 2: ratio
# We computed the areas of the footprint and their intersection with nmfs statistical areas.
# If the difference between them was < ___km2 the latter was used.

# Type 3: 
# If footprint area is less than 10,000,000,000 m2
# 10,000 km2

# Degree of overlap: percent of total footprint in stat area

# This is the footprint area
fp <- fprints$`SCITUATE, MA`




# Function to get the stat zones from intersection, 
# But only if a significant portion of the footprints total area is within it
# 15-20% cutoff target
cutoff_percent <- 15


####  Get ID's Based on Overlap %  ####
min_overlap_ids <- function(fp, min_cutoff){
  
  # Get area of the footprint
  fp_area <- st_area(fp)
  
  # # This is the area of all the footprints it intersects
  # zones_area <- filter(stat_zones, Id %in% get_overlap_ids(fp)) %>% 
  #   st_union() %>% 
  #   st_area()
  
  # Id's and Area of overlap between footprint and stat zones
  overlap_areas <- st_intersection(stat_zones, st_transform(fp, st_crs(stat_zones))) %>% 
    group_by(Id) %>% 
    summarise(area = st_area(geometry))

  # Use only these stat areas:
  # based on degree of overlap
  use_Ids <- overlap_areas %>% 
    st_drop_geometry() %>% 
    mutate(area = as.numeric(area),
           fp_area = as.numeric(fp_area),
           # Calculate percent of footprint that sits in the stat zones it intersects
           perc_overlap = (as.numeric(area)/fp_area)*100,
           is_under = ifelse(perc_overlap < cutoff_percent, T, F)) %>% 
    filter(!is_under) %>% 
    pull(Id)
  
  # There ya go
  use_Ids
}


# Can we display the impact visually

rule_based_overmaps <- map(names(fprints), function(x){
  overlap_maps[[x]] +
    geom_sf(data = filter(
      stat_zones, 
      Id %in% min_overlap_ids(fp = fprints[[x]], min_cutoff = 15)),
      color = "darkgreen", fill = "lightgreen", size = 2, alpha = 0.2)
  
}) 


# set names
rule_based_overmaps <- setNames(rule_based_overmaps, names(fprints))


# Consequence of the rule based filtering
rule_based_overmaps$`STONINGTON, ME`
rule_based_overmaps$`SCITUATE, MA` |
rule_based_overmaps$`PORTSMOUTH, NH`
rule_based_overmaps$`NEW LONDON, CT`
rule_based_overmaps$`CHATHAM, MA`
rule_based_overmaps$`PORTLAND, ME`
rule_based_overmaps$`STONINGTON, CT`
rule_based_overmaps$`BOSTON, MA`
rule_based_overmaps$`POINT PLEASANT, NJ`
rule_based_overmaps$`MONTAUK, NY`
rule_based_overmaps$`GLOUCESTER, MA`
rule_based_overmaps$`NEWPORT NEWS, VA`
rule_based_overmaps$`CAPE MAY, NJ`
rule_based_overmaps$`POINT JUDITH, RI`
rule_based_overmaps$`NEW BEDFORD, MA`



####  Applying Rules  ####

# 1. Minimum area footprints: 
# If footprint area is less than 10,000,000,000 m2
small_footprints <- filter(size_order, as.numeric(area) < 10000000000) %>% pull(footprint)




# 2. Perform overlay, with minimum area overlap
min_overlap_zones <- map(
  .x = setNames(small_footprints, small_footprints), 
  .f = ~min_overlap_ids(fp = fprints[[.x]], min_cutoff = 15))

# Save the list of these combinations somehow...
# probably some table: community | use_fprint T/F | zone_ids
footprint_rules <- imap_dfr(
  .x = fprints, 
  function(x, y){
    
    if(y %in% small_footprints){
      community_rule <- tibble(
        "community_id" = y,
        "use_fprint" = FALSE,
        "zone_ids" = list(min_overlap_zones[[y]])
      )} else {
        community_rule <- tibble(
          "community_id" = y,
          "use_fprint" = TRUE,
          "zone_ids" = list(NA))
      }
      return(community_rule)
    }
)



# This is the breakdown of how footprints would be processed:
footprint_rules



####  Perform Masking for Hybrids ####

# Taking function from vast_regional_summaries.R

# Turn off s2 for spatial overlay
sf_use_s2(FALSE)


####  Master Data  ####
unique_pts <- read_sf(here::here("Data/spatial/unique_location_coords.csv"))


# Density data for all species
all_density_results <- read_csv(here::here("Data/projections/VAST_all_densities_all_species.csv"))

# input 1: Unique locations as sf
unique_pts_sf <- unique_pts %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = F)

# input 2: shapefile to check
# shape_in <- gom_epu




# Make a function to do it a bunch
filter_within <- function(unique_pts_sf, shape_in, region_title, plot_check = FALSE){
  
  domain_use <- st_make_valid(shape_in) %>% st_transform(st_crs(unique_pts_sf))
  
  # Overlay Points with study area:
  # Use drop_na to drop points that picked up no informaiton in join
  unique_pts_within <- st_join(x = unique_pts_sf, y = domain_use) %>% drop_na()
  
  # Verify the deed is done:
  if(plot_check){
    p <- unique_pts_within %>%
      ggplot() +
      geom_sf(data = domain_use) +
      geom_sf() +
      labs(title = region_title)
    return(p)
  }
  
  
  # Now we can use those points to filter and summarise
  # Can then average whatever is within
  # Already comes in as year month and season
  region_dat <- all_density_results %>% 
    filter(pt_id %in% unique_pts_within$pt_id) %>% 
    mutate(region = region_title) %>% 
    group_by(VAST_id, region, Year, Month, Season) %>% 
    summarise(
      across(.cols = c("Prob_0.1", "Prob_0.5", "Prob_0.9"), 
             .fns = ~mean(.x, na.rm = T), 
             .names = "mean_{.col}"),
      .groups = "drop") 
  
  # add species and scenario details from the list's names
  region_dat <- region_dat %>% 
    mutate(
      VAST_id = str_replace(VAST_id, "_full_", "-")) %>% 
    separate(VAST_id, into = c("species", "scenario"), sep = "-") %>% 
    mutate(
      species = str_replace_all(species, "_", " "),
      species = tolower(species),
      scenario = str_remove(scenario, "_mean"))
  return(region_dat)
  
}




####  Process Hybrids  ####
hybrid_densities <- footprint_rules %>% 
  filter(!use_fprint) %>% 
  split(.$community_id) %>% 
  map_dfr(function(hybrid_community){
    
    # Subset the zone id's from the statistical zones shapefiles
    hybrid_fprint <- filter(stat_zones, Id %in% unlist(hybrid_community$"zone_ids")) %>% 
      st_union() %>% 
      st_as_sf() %>%  
      mutate(community_id = hybrid_community$community_id[1]) %>% 
      rename(geometry = x)
    
    # Then use that to perform a masking operation
    filter_within(
      unique_pts_sf = unique_pts_sf,
      shape_in = hybrid_fprint, 
      region_title = str_c(hybrid_community$community_id[1], " - statistical zone overlap"), 
      plot_check = T)
    
  })


# Check it as a timeline
hybrid_densities %>% 
  filter(species == "cod",
         #scenario == "CMIP6_SSP5_85",
         region == "PORTSMOUTH, NH - statistical zone overlap") %>% 
  group_by(Year, scenario) %>% 
  summarise(proj_dens = mean(mean_Prob_0.5, na.rm = T)) %>% 
  ggplot(aes(Year, proj_dens, color = scenario), linewidth = 1) +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2100)) +
  labs(y = "Projected Average Annual Density")



####  Save Hybrids  ####

# Save these somewhere:
write_csv(hybrid_densities, here::here("Data/projections/annual_small_footprint_szone_overlap_projections.csv"))

