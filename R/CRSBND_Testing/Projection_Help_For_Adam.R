library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sdmTMB)
library(sdmTMBextra)
library(ggeffects)
library(patchwork)

#####
# Read in projection files
#####
# Make a tibble with fitted model as its own column so we can map different functions to the models
proj_root<- here::here("Working/Results")
all_proj <- list.files(proj_root, pattern = "projs", full.names = TRUE)

proj_df <- tibble("Name" = c("cod_env_only", "cod_spst_seas_fac"), "Full_Proj" = vector("list", length(all_proj)))

for(i in seq_along(all_proj)){
  proj_df$Full_Proj[[i]] <- readRDS(all_proj[[i]])
}

#####
# Get biomass indices
#####
# Likely want by region -- I think you've got these already to load and bind, though here might not work as I am not sure where you have em saved
# Read and combine shapefiles
dfo_reg <- st_read(here::here("Data/Supporting/Index_Shapefiles", "DFO.shp"))
nmfs_reg <- st_read(here::here("Data/Supporting/Index_Shapefiles", "NMFS.shp"))

# Add region labels if needed
dfo_reg <- dfo_reg |> mutate(region = "DFO")
nmfs_reg <- nmfs_reg |> mutate(region = "NMFS")

all_regs <- bind_rows(nmfs_reg, dfo_reg)

proj_ind <- proj_df |>
  mutate(
    Proj_Ind = map(Full_Proj, ~ {
      proj <- .x |>
        mutate(
          proj_biomass_log = proj_biomass_mean + log(area),
          lwr0.1_log = (proj_biomass_mean - qnorm(0.9) * proj_biomass_se) + log(area),
          upr0.9_log = (proj_biomass_mean + qnorm(0.9) * proj_biomass_se) + log(area),
          proj_biomass_resp = exp(proj_biomass_log),
          lwr0.1_resp = exp(lwr0.1_log),
          upr0.9_resp = exp(upr0.9_log)
        )

      # Convert to sf
      proj_sf <- st_as_sf(proj, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

      # Join with region polygons
      joined <- st_join(proj_sf, all_regs |> select(region), left = TRUE)

      # Return back to data.frame, drop geometry
      st_drop_geometry(joined)
    })
  )

# Summarize total biomass per model
proj_ind_summ_total <- proj_ind |>
  mutate(
    Proj_Ind_Summ = map2(Proj_Ind, Name, ~ {
      .x |>
        group_by(year, season, year_season_int) |>
        summarise(
          proj_bio_total_log = sum(proj_biomass_log, na.rm = TRUE),
          proj_bio_lwr_log = sum(lwr0.1_log, na.rm = TRUE),
          proj_bio_upr_log = sum(upr0.9_log, na.rm = TRUE),
          proj_bio_total_resp = sum(proj_biomass_resp, na.rm = TRUE),
          proj_bio_lwr_resp = sum(lwr0.1_resp, na.rm = TRUE),
          proj_bio_upr_resp = sum(upr0.9_resp, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(region = "Total", model_name = .y)
    })
  )

# Summarize regional biomass per model
proj_ind_summ_reg <- proj_ind |>
  mutate(
    Proj_Ind_Summ_Reg = map2(Proj_Ind, Name, ~ {
      .x |>
        group_by(region, year, season, year_season_int) |>
        summarise(
          proj_bio_total_log = sum(proj_biomass_log, na.rm = TRUE),
          proj_bio_lwr_log = sum(lwr0.1_log, na.rm = TRUE),
          proj_bio_upr_log = sum(upr0.9_log, na.rm = TRUE),
          proj_bio_total_resp = sum(proj_biomass_resp, na.rm = TRUE),
          proj_bio_lwr_resp = sum(lwr0.1_resp, na.rm = TRUE),
          proj_bio_upr_resp = sum(upr0.9_resp, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(model_name = .y)
    })
  )

summ_total <- proj_ind_summ_total |>
  select(Name, Proj_Ind_Summ) |>
  unnest(Proj_Ind_Summ)

summ_reg <- proj_ind_summ_reg |>
  select(Name, Proj_Ind_Summ_Reg) |>
  unnest(Proj_Ind_Summ_Reg)

proj_ind_combined <- bind_rows(summ_total, summ_reg) |>
  mutate(
    # For any missing region, assign "Total"
    region = if_else(is.na(region), "Total", region),
    region = factor(region, levels = c("Total", "NMFS", "DFO")),
    season = factor(season, levels = c("spring", "summer", "fall"))
  )

proj_plots <- proj_ind_combined |>
  ungroup() |>
  group_by(model_name) |>
  group_map(~ {
    ggplot(.x) +
      geom_ribbon(aes(x = year, ymin = proj_bio_lwr_resp, ymax = proj_bio_upr_resp, fill = region),
                  alpha = 0.2) +
      geom_line(aes(x = year, y = proj_bio_total_resp, color = region), linewidth = 1.2) +
      facet_wrap(~season, nrow = 3) +
      scale_color_manual(values = c("Total" = "black", "NMFS" = "#1f78b4", "DFO" = "#33a02c")) +
      scale_fill_manual(values = c("Total" = "black", "NMFS" = "#1f78b4", "DFO" = "#33a02c")) +
      labs(title = paste("Model:", unique(.y$model_name)),
           x = "Year", y = "Relative Biomass Index") +
      theme_bw() +
      theme(legend.position = "bottom")
  })

combined_plot <- wrap_plots(proj_plots, names = mod_names_keep, nrow = 1, guides = "collect") +
  plot_annotation(title = "Projected Biomass by Model and Season")
combined_plot

#####
# Get COG
#####
# Center of gravity
proj_cog_summ <- proj_ind |>
  mutate(
    Proj_COG_Summ = map(Proj_Ind, ~ .x |>
      group_by(year, season, year_season_int) |>
      mutate(season = factor(season, levels = c("spring", "summer", "fall"))) |>
       summarise(
        center_lat = weighted.mean(latitude, proj_biomass_resp, na.rm = TRUE),
        center_lon = weighted.mean(longitude, proj_biomass_resp, na.rm = TRUE),
        center_lat_lwr = weighted.mean(latitude, lwr0.1_resp, na.rm = TRUE),
        center_lon_lwr = weighted.mean(longitude, lwr0.1_resp, na.rm = TRUE),
        center_lat_upr = weighted.mean(latitude, upr0.9_resp, na.rm = TRUE),
        center_lon_upr = weighted.mean(longitude, upr0.9_resp, na.rm = TRUE)
      ))
  )

proj_centers <- proj_cog_summ |>
  unnest(Proj_COG_Summ) |> # if needed
  select(Name, year, season, center_lat, center_lon, center_lat_lwr, center_lon_lwr, center_lat_upr, center_lon_upr) |>
  mutate(
    decade = paste0(floor(year / 10) * 10, "s")
  )

# Get base map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Optional: crop to NE US + Maritimes bounding box
bbox <- st_as_sfc(st_bbox(c(
  xmin = -78, xmax = -56,
  ymin = 38, ymax = 50
), crs = st_crs(world)))

centers_sf <- proj_centers |>
  st_as_sf(coords = c("center_lon", "center_lat"), crs = 4326)

cog_plot_out<- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70") +
  geom_sf(data = centers_sf, aes(color = decade), size = 2) +
  coord_sf(xlim = c(-78, -56), ylim = c(38, 50), expand = FALSE) +
  facet_wrap(~Name + season) +
   scale_color_viridis_d(option = "plasma", direction = -1) + 
  labs(
    title = "Center of Projected Biomass by Season and Decade",
    subtitle = "Northeast US and Canadian Maritimes",
    color = "Decade"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey90"))

cog_plot_out




