#####  Projected Timeseries Cleanup  ####

# Streamline the projected timeseries data for plotting:


library(sf)
library(gmRi)
library(tidyverse)
library(scales)
library(ggtext)
library(geomtextpath)
library(showtext)




# Fix Species names:
name_fix <- tribble(
  ~"species",              ~"comname",
  "atlanticmackerel",      "Atlantic mackerel",              
  "butterfish",            "butterfish",     
  "blackseabass",          "black sea bass",
  "cod",                   "Atlantic cod",              
  "haddock",               "haddock",             
  "hagfish",               "hagfish",                
  "halibut",               "halibut",                
  "herring",               "herring",               
  "jonahcrab",             "Jonah crab",                
  "littleskate",           "little skate",                
  "lobster",               "American lobster",               
  "longfinsquid",          "longfin squid",                
  "monkfish",              "monkfish",                
  "northernsandlance",     "northern sandlance",               
  "oceanquahog",           "ocean quahog",                
  "pollock",               "pollock",                
  "reddeepseacrab",        "red deepsea crab",      
  "redfish",               "acadian redfish",
  "redhake",               "red hake",                
  "rockcrab",              "rock crab",               
  "scallop",               "scallop",              
  "scup",                  "scup",               
  "shortfinsquid",         "shortfin squid",               
  "silverhake",            "silver hake",              
  "smoothskate",           "smooth skate",               
  "spinydogfish",          "spiny dogfish",               
  "summerflounder",        "summer flounder",              
  "thornyskate",           "thorny skate",               
  "whitehake",             "white hake",               
  "windowpaneflounder",    "windowpane flounder",              
  "winterflounder",        "winter flounder",               
  "winterskate",           "winter skate",                
  "witchflounder",         "witch flounder",              
  "yellowtailflounder",    "yellowtail flounder"             
)



####  Load the Data  ####
# timeseries made in:
# vast_regional_summaries.R

# This has all the regions:
regional_ts <- read_csv(
  #file = here::here("Data/projections/annual_regional_species_projections.csv"), 
  file = here::here("COCA_SDM_app_dev/dev", "projections/annual_regional_species_projections.csv"), 
  col_types = list(
    species  = col_character(),
    scenario = col_character(),
    region   = col_character(),
    Year     = col_double(),
    Month    = col_character(),
    Season   = col_character(),
    Year     = col_double()
  ))



# Really only need EPU's and maybe Canada
regional_ts <- regional_ts %>% 
  filter(region %in% c("GOM_epu", "GB_epu", "SS_epu", "MAB_epu")) %>% 
  select(-c(mean_Prob_0.1, mean_Prob_0.9, Month))

# Get an annual mean
annual_avg <- regional_ts %>% 
  group_by(species, scenario, region, Year) %>% 
  summarise(mean_Prob_0.5 = mean(mean_Prob_0.5, na.em = T),
            .groups = "drop") %>% 
  mutate(Season = "Annual Average")


# Rejoin
timeseries_all <- bind_rows(regional_ts, annual_avg) %>% 
  mutate(region = case_when(
    region == "GOM_epu" ~ "Gulf of Maine",
    region == "GB_epu" ~ "Georges Bank",
    region == "SS_epu" ~ "Scotian Shelf",
    region == "MAB_epu" ~ "Mid-Atlantic Bight"),
    region = factor(
      region,
      levels = c("Scotian Shelf", "Gulf of Maine", "Georges Bank", "Mid-Atlantic Bight")))


# Fix the species common name thing
timeseries_all <- left_join(timeseries_all, name_fix) %>% 
  drop_na() # drops the special models that andrew ran


# Don't need data before 2010
timeseries_all <- filter(timeseries_all, Year >= 2010) %>% 
  rename_all(.funs = ~tolower(.x)) %>% 
  rename(avg_dens = mean_prob_0.5)


#### Save App-Ready Data
write_csv(timeseries_all, here::here("Coca_SDM_app_dev/app_ready_data/projected_densities_timeseries.csv"))
