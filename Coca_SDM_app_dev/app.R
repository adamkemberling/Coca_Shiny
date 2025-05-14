# Libraries
library(here)




####  Load Global Assets  ####

# Load Global Assets: Shared/Available across all sessions
# Contains: Support Code and Modules
source(here("Coca_SDM_app_dev/global.R"))









####____________________####
#### Testing Space + Defaults ####



# These match the two user selection controls on the App
# species_choice <- "haddock"
species_choice <- species_opts[["Black Sea Bass"]]
scenario_choice <- "CMIP6_SSP5_85"
horizon_choice <-  "4C"

# Filter
test_species <- horizon_projections %>% 
  filter(comname == species_choice) 

# At this point we can join back in the lat/lon or the grid
test_species   <- left_join(test_species, hex_grid, by = join_by(pt_id))

# seasonal versions
#test_species_season <- horizon_szns %>% filter(comname == species_choice, season == season_coice) 
#test_species_season <- left_join(test_species, hex_grid, by = join_by(pt_id))



# Get a baseline and a projection to use as test selections

# SSP1
test_base <- test_species %>%
  filter(
    scenario == scenario_choice,
    temp_horizon == "0C") %>%
  st_as_sf()


# Plot a baseline
ssp_proj_map(dist_df = test_base, reactive = F)



# Get the values at a horizon
test_proj <- test_species %>%
  filter(
    scenario == scenario_choice,
    temp_horizon == horizon_choice) %>%
  st_as_sf()


# Get the difference of testers
diff_test <- get_difference(base_dat = test_base, proj_dat = test_proj)





####  Next UP  ####

# Timeseries baby!
# Load the annual data, make it smooth, overlay seasons in a thoughtful way
# Show when the thresholds are crossed







####_____________________####
####  Define UI Displays and Modules  ####



# Define the modules for

# Text and image content:
# 1. Species Display Card
# 2. Image/artwork for the species
# 3. General Biology information
# 4. Species habitat preference boxes

# Map displays:
## 1. Baseline Biomass
## 2. SSP Scenario Conditions
## 3. SSP Scenario Differences


# # Download buttons
# downloadUI <- function(id) {
#   ns <- NS(id)
#   tagList(downloadButton(ns('downloadPlot'), 'Download Plot'),
#           downloadButton(ns('downloadData'), 'Download Data'))
# }



####__________________________####
####  Random Static Elements  ####
link_ssp <- tags$a("SSP Pathways", href = "https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/", target = "_blank")



#### Modules Testing:  ####
# test modules here before exporting them out to their own files


# ####  Modules: Data Filtering  ####

# Moved to their own files

#### Modules: Plot generation  ####

# Moved to their own files










####_________________________####
####_________________________####

####  UI:  ####

# Define UI for application that draws a histogram



# Build using a navbar page to hold navigation tabs:
ui <- page_navbar(
  
  # 1. App Title
  title = "Species Distribution Shifts Projected Under Climate Change", 
  
  # 2. Theming with bslib
  theme = custom_theme,
  
  #### Side Panel Controls  ####
  sidebar = sidebar(
    id = "sideBar", 
    title = "Select a Species and Climate Scenario for Projections:",
    
    
    # Panel to display the Controls
    nav_panel(
      style = "
        overflow-wrap: anywhere;
        h1, h2, h3, p{
          padding-left: 10px;
          padding-right: 10px;
        }",
      h3("Display Controls"),
      sideUI("sidebar_ID"),
      startExpanded = TRUE),
      br(),
      p("Use the above controls to view species 
        distributions under future climate scenarios 
        based on their individual habitat preferences.")
      
    
  ), # Close the sidebar
    
  
  ####  Top Navigation Panel  ####
  
  # Tab 1: 
  nav_panel(
    title = "1. Baseline Period Distribution", 
    value = "baseline_tab",
    
    #### 1. Baseline Conditions  ####
    
    ##### a. Baseline Description #### 
    layout_column_wrap(
      width = 1/2, 
      height = 800,
      
      # Card 1: Written Description
      card(
        card_header(
          # class = "bg-dark",
          style = "background-color: #004966;  color: white;",
          "Baseline Period Characteristics"),
        
        card_body(
          h3(markdown("**Using Historic Baselines for Comparison**")),
          
          p("The map to the right shows the estimated distribution of this species
             over a ten-year baseline period. Distribution models estimate each species' 
             biomass using average surface and bottom temperatures and any 
             persistent associations to specific areas observed in the data."),
          
          card_body(
            class = "p-static",
            em("This site uses a ten-year period (2010-2019) as a modern baseline 
                from which to compare projected future changes against."),
            p("Models are tools which help scientists understand how species respond 
               to changes in the environment. Abundance data from this baseline period 
               is used to train models to learn each species' unique preferences for 
               different environmental conditions. Modeled relationships allows scientists 
               to make an educated guess on distribution changes under projected future conditions.
               "),
            
             
            
            #p("Over this baseline period, _____ have shown the following broad characteristics and preferences:"),
            
            ####  Dynamic Text TEsting ####
            textOutput("species_text"),
            
            
            ##### b. Value Boxes  ####
            # Second layout wrap height for the species metrics  
            layout_column_wrap(
              fill = FALSE, 
              row_heights = 300,
              width = "300px",
              
              # Value Box 1: Temperature
              value_box(
                  title = "Preferred Temperature",
                  value = str_c(9, deg_c),
                  showcase = bsicons::bs_icon("thermometer-sun")
                ),
              
              # Value Box 2: Depth
              value_box(
                  title = "Preferred Depth",
                  value = str_c(67, " meters"),
                  showcase = bsicons::bs_icon("align-bottom"),
                  theme_color = "dark"
                ),
              
              # Value Box 3: Bodymass
              value_box(
                  title = "Average body mass",
                  value = str_c(740, "g"),
                  showcase = bsicons::bs_icon("handbag"),
                  theme_color = "secondary"),
              
              # Value Box 4: Average Length
              value_box(
                  title = "Average length",
                  value = str_c(83, "cm"),
                  showcase = bsicons::bs_icon("rulers"),
                  theme_color = "secondary")
              
            ) # Close value box layout
            
            ) # close card body 
          
          
          ),# Close Card 1
        
        card_footer(markdown("[Learn more about the ecology of this species](https://www.fisheries.noaa.gov/species/atlantic-cod)"))
      ),
      
      ##### c. Baseline Map  ####
      # Use Module for map card
      projection_map_ui("baseline_bio_map")
      

    
    ) # End layout_col for map and baseline description
    
    
  ), # End nav_panel 1
  
  
  
  
  
    #### Tab 2: Projected Biomass  ####
    nav_panel(
      title = "2. Projected Distribution Change", 
      value = "projected_bio_tab",
       
      layout_column_wrap(
        width = 1/2, 
        height = 800,
        
        ##### a. Timeseries Projections ####
        # Card 1: Projected Density Timeseries
        projected_timeseries_ui("projected_bio_timeseries"), 
              
        ##### b. Projected Bio Map ####
        # Card 2: Projected Species Distribution
        projection_map_ui("projected_bio_map") 

      ) # End layout_col for map and baseline description
      
    ), # End Tab 2
    
    
  
  
  
    #### Tab 3. Biomass Changes  ####
    
    nav_panel(
      title = "3. Changes to Habitat Suitability",
      value = "difference_tab",
      
      
      layout_column_wrap(
        width = 1/2, 
        height = 800,
        
        ##### a. Preference Curves #### 
        # Card 1: Preference Curve Plot
        preference_card_ui("preference_curve_plot"),
        
        #####  b. Distribution Change  ####
        # Card 2: Projected Difference Map
        difference_map_ui("projected_diff_map")
        
      ) # End layout column
  ), # End Panel 3
  
  
  # Add a spacer
  nav_spacer(),


  #### Navbar Menu  ####
  # Additional Options via Menu
  nav_menu(
    align = "right",
    title = "More Information:",
    nav_panel(
      title = "Data Request Information", 
      "data_rqst_panel",
      p("
        Some background content, this should be a markdown doc that has disclaimers and 
        links to the full dataset")
      ), # End data request information
   
    
    "----",  # Line separator in dropdown list
    "Learn More About SSP Scenarios",   # Text for the contents beneath the line
    nav_item(link_ssp)
    
  ) # Close Nav Menu
    
       
) # End the whole thing
  
  




####_________________________####
####_________________________####
####  Server  ####


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #### Reactive Data  ####
  
  # 1. Filter the input data for the different pieces:
  # id matches the sidebar id where the inputs all are
  
  
  
  
  # Spatial density projections: species
  species_dat    <- filterSpecies_server(
    "sidebar_ID", 
    species_projection_list = species_projection_list)
  
  
  # Projected densities: baseline
  baseline_dat   <- filterBaseline_server(
    "sidebar_ID", 
    df = species_dat)
  
  # Projected densities: projections
  projection_dat <- filterProjection_server(
    "sidebar_ID", 
    df = species_dat)
  
  # Projected_densities: changes
  difference_dat <- getDifference_server(
    "sidebar_ID", 
    base_df = baseline_dat, 
    proj_df = projection_dat)
  
  # Projected density timeseries
  timeseries_dat <- getTimeseries_server(
    "sidebar_ID", 
    density_timeseries = density_timeseries)
  
  # Species preference curve and environmental states
  preferences_dat <- filterPreferences_server(
    "sidebar_ID", 
    pref_data = pref_data, 
    env_condition_data = env_condition_data)
  
  
  ####  Reactive UI  ####
  # Reactive temperature horizons
  callModule(updateHorizons, 'sidebar_ID')
  
  
  #### Card Modules  ####

  # # 2. Build the map/plot content for the UI
  # ID's should match the cards for their outputs
  
  # Card 1: Baseline projection
  projection_map_server(
    id = "baseline_bio_map", 
    in_data = baseline_dat)
  
  # Card 2: 
  # a. projected timeseries
  projected_timeseries_server(
    id = "projected_bio_timeseries", 
    in_data = timeseries_dat)
  
  # b. Projected Biomass Map
  projection_map_server(
    id = "projected_bio_map", 
    in_data = projection_dat, 
    add_labels = TRUE)
  
  # Card 3:
  # a. 
  preference_curve_server(
    "preference_curve_plot", 
    in_data = preferences_dat) 
  # b. difference map
  difference_map_server(
    id = "projected_diff_map", 
    in_data = difference_dat)
  
  
  
  ####  Reactive Text  ####
  
  selected_species <- updateSpecies("sidebar_ID")  # call the module
  
  output$species_text <- renderText({
    paste0(str_to_title(selected_species()),  " have shown the following broad characteristics and preferences over this baseline period:")
  })
  
  
  
  
  
}


# Running it Normally
shinyApp(ui, server)


# # # Running the themer from bslib
# run_with_themer(shinyApp(ui, server))
