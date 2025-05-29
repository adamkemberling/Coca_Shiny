#----------------------------------
## Libraries and preliminaries
#----------------------------------
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sdmTMB)
library(sdmTMBextra)
library(ggeffects)
library(patchwork)
library(sf)
library(units)
library(gganimate)
source(here::here("Code/sdmTMB_validation_Functions.R"))

# Base map land info
region <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")
states <- ne_states(country = c("United States of America", "Canada"), returnclass = "sf")

lat_lims <- c(35.2, 48)
lon_lims <- c(-76, -56.2)

# Read in projections
res<- readRDS(here::here("Projections/adult_projected_biomass.rds"))
head(res)

# Keep what we need
res_use<- res |>
    dplyr::select(longitude, latitude, season, year, Year_Season, Date, Region, proj_biomass_mean, proj_biomass_se)
summary(res_use)


#####
## Biomass time series
#####
# Need to get an area per region...number of cells per region
all_regs<- st_read(here::here("Data/Derived/all_regions.shp"))
all_regs<- all_regs |>
    mutate(area = st_area(geometry))
all_regs$area<- set_units(all_regs$area, km^2)

# Average density within each region per time step
res_summ<- res_use |>
    group_by(season, year, Year_Season, Date, Region) |>
    summarize("mean_biomass" = mean(proj_biomass_mean))

# Join, calculate total area
res_summ<- res_summ |>
    left_join(all_regs, by = c("Region" = "Region")) |>
    mutate("Biomass_Total" = drop_units(exp(mean_biomass)*area))

# Region order
res_summ<- res_summ |>
    mutate(Region = factor(Region, levels = c("SS", "GoM_ECS", "BoF", "GoM_NCS", "GoM", "GoM_SCS", "GB", "SNE", "MAB")))

all_regs<- all_regs |>
    mutate(Region = factor(Region, levels = c("SS", "GoM_ECS", "BoF", "GoM_NCS", "GoM", "GoM_SCS", "GB", "SNE", "MAB")))

reg_plot<- ggplot() + 
    geom_sf(data = all_regs, aes(fill = Region)) + scale_fill_manual(name = "Region", values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00")) + 
    theme_bw()

ts<- ggplot() +
#   geom_ribbon(data = res_summ, aes(x = year, ymin = log_lwr0.1, ymax = log_upr0.9, fill = season), alpha = 0.3, linewidth = 0) +
  geom_vline(xintercept = 2023, lty = "dashed") +
  geom_line(data = res_summ, aes(x = year, y = Biomass_Total, color = Region), lwd = 1) +
  geom_vline(xintercept = 2023, lty = "dashed") +
  # geom_hline(data = bio_clim, aes(yintercept = mean)) +
  scale_color_manual(name = "Region", values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00")) + 
#   scale_fill_manual(name = "Region") + 
  xlab("Year") +
  ylab("Relative Biomass Index") +
  facet_wrap(~season, nrow = 1) +
  theme_bw()

ts_out<- ts / reg_plot + plot_layout(widths = c(1, 0.9), height = c(1, 0.9))
ggsave(here::here("Figures/BiomassTS_ByRegion.jpg"))

#####
## COG
#####
head(res_use)

res_cog<- res_use |>
    group_by(season, year) |>
    nest() 
res_cog$ID<- seq(1, nrow(res_cog))

res_cog<- res_cog |>
    mutate(COG = map(data, ~ COGravity(x = .x$longitude, y = .x$latitude, z = NULL, wt = .x$proj_biomass_mean))
    ) |>
    # dplyr::select(year, season, COG) |>
    unnest_wider(COG)

# COGy overtime by season
res_cog$season<- factor(res_cog$season, levels = c("Spring", "Summer", "Fall"))

# Decade
res_cog<- res_cog |>
    mutate(Decade = year - year %% 10)

cog_lat_ts <- ggplot() +
    #   geom_ribbon(data = res_cog, aes(x = year, ymin = COGy - COGy.sd, ymax = COGy + COGy.sd, fill = season), alpha = 0.3, linewidth = 0) +
    # geom_line(data = res_cog, aes(x = year, y = COGy), lwd = 1) +
    geom_line(data = res_cog, aes(x = year, y = COGy, color = season), lwd = 1) +
    scale_color_manual(name = "season", values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
    scale_fill_manual(name = "season", values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
    xlab("Year") +
    ylab("Center of Latitude") +
    facet_wrap(~season, ncol = 3) +
    theme_bw()
ggsave(here::here("Figures/COG_Lat_BySeason.jpg"))

res_cog_sf<- st_as_sf(res_cog, coords = c("COGx", "COGy"), crs = 4326)

cog_plot<- ggplot() +
    geom_point(data = res_cog, aes(x = COGx, y = COGy, color = Decade)) + 
    geom_sf(data = region, fill = "#f0f0f0") +
    geom_sf(data = states, color = "dark gray", lwd = 0.2, na.rm = TRUE) +
    coord_sf(xlim = c(-72, -55), ylim = c(40, 48), expand = FALSE) +
    scale_fill_viridis_c(name = "Center of Gravity") +
    theme_minimal() +
    facet_wrap(~season, ncol = 3)

#### COG GIF
res_cog_map <- res_cog |>
    dplyr::select(season, year, data) |>
    unnest()

res_cog_ts <- res_cog |>
    dplyr::select(season, year, COGx, COGy)

for (j in seq_along(unique(rss_cog_map$season))) {
    season_use <- unique(res_cog_map$season)[j]

    ts_dat <- res_cog_ts |>
        filter(season == season_use) |>
        mutate(year_date = as.Date(year))

    map_dat <- res_cog_map |>
        filter(season == season_use) |>
        mutate(year_date = as.Date(year))
    
    map_out <- ggplot() +
        borders("world", colour = "gray50", fill = "gray90") +
        geom_raster(data = map_dat, aes(x = longitude, y = latitude, fill = proj_biomass_mean)) +
        geom_point(
            data = ts_dat,
            aes(x = COGx, y = COGy),
            color = "#FF0000",
            size = 3
        ) +
        geom_sf(data = region, fill = "#f0f0f0") +
        geom_sf(data = states, color = "dark gray", lwd = 0.2, na.rm = TRUE) +
        coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE) +
        scale_fill_viridis_c(name = "Log(Predicted biomass)") +
        labs(title = "{closest_state}") + 
        theme_minimal() +
        # animation code
        transition_states(
            year, # Uses each year (est_year in the data) as a state
            transition_length = 1, state_length = 1
        ) +
        ease_aes("linear") # Smooth transition, other options for how they transition looks 
    animate(map_out, renderer = gifski_renderer("adult_biomass_cog_map.gif"))
    
    ts_out <- ggplot() +
        geom_line(data = ts_dat, aes(x = year, y = COGy), lwd = 1) +
        geom_point(data = ts_dat, aes(x = year, y = COGy)) +
        xlab("Year") +
        ylab("Center of Latitude") +
        theme_bw() +
        labs(title = "{closest_state}") + 
        transition_reveal(year)
    animate(ts_out, renderer = gifski_renderer("adult_biomass_cog_ts.gif"))

    both <- ts_out / map_out

        # Convert to magick image
        frames[[i]] <- magick::image_graph(width = 800, height = 400)
        print(combined)
        dev.off()
    }
}

# Create GIF
magick::image_join(frames) |>
    magick::image_animate(fps = 2)

