####  Packages  ####
library(shiny)
library(bslib)
library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(waiter)
library(bsicons)
library(rcartocolor)
library(showtext)
library(scales)


#### Support Functions:  ####

# OG support script, should probably replace with minimum necessary data
#source(here::here("R/app_support.R"))

# New support script
source(here::here("Coca_SDM_app_dev/app_functions/coca_application_funs.R"))

# # Add fonts for figures
font_add_google("Raleway", "raleway")

# Path to the directory containing the font file (replace with your actual path)
font_dir <- paste0(system.file("stylesheets", package = "gmRi"), "/GMRI_fonts/Avenir/")
font_add(
  family = "Avenir",
  file.path(font_dir, "LTe50342.ttf"),
  bold = file.path(font_dir, "LTe50340.ttf"),
  italic = file.path(font_dir, "LTe50343.ttf"),
  bolditalic = file.path(font_dir, "LTe50347.ttf"))
showtext_auto()


####  App Theming  ####
custom_theme <- bs_theme(
  version          = 5,
  base_font        = "Avenir", 
  heading_font     = font_google("Raleway"), 
  fg               = "#00736D",
  bg               = "#FFFFFF",
  primary          = "#00608A", 
  secondary        = "#535353", 
  success          = "#407331", 
  info             = "#ABB400", 
  warning          = "#EACA00", 
  danger           = "#EA4F12", 
  font_scale       = 1.15, 
  `enable-shadows` = TRUE, 
  spacer           = "1.25rem"
  
)





# Fix Species names:
name_fix <- tribble(
  ~"species",              ~"comname",
  "butterfish",            "butterfish",              
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

####_________________________####
####  Data Preparations:  ####



##### VAST Ouptput Data  ####



####  Timeseries Datasets  ####

# # Annual Averages and the Baseline Difference
# annual_wide <- read_csv(
#   file = here::here("Data/projections/annual_proj_wide.csv"),
#   col_types = cols(
#     pt_id = col_double(),
#     species = col_character(),
#     scenario = col_character(),
#     `mean_2010-2019` = col_double(),
#     diff_2050 = col_double(),
#     diff_2050_z = col_double(),
#     diff_2100 = col_double(),
#     diff_2100_z = col_double()
#   ))
# 
# # Seasonal Average and the Baseline Difference
# season_wide <- read_csv(
#   file = here::here("Data/projections/seasonal_proj_wide.csv"),
#   col_types = cols(
#     pt_id = col_double(),
#     species = col_character(),
#     scenario = col_character(),
#     season = col_character(),
#     `mean_2010-2019` = col_double(),
#     diff_2050 = col_double(),
#     diff_2050_z = col_double(),
#     diff_2100 = col_double(),
#     diff_2100_z = col_double()
#   ))


#### Temperature Milestones Map Data  ####


# Load the decadal milestone summaries
horizon_projections <- read_csv(
  here::here("Data/projections/Cmilestones_all_species_test.csv"),
  col_types = cols(
    var = col_character(),
    ref_period = col_character(),
    temp_horizon = col_character(),
    species = col_character(),
    scenario = col_character(),
    pt_id = col_double(),
    val = col_double()))  %>% 
  left_join(name_fix, by = join_by(species)) %>% 
  filter(str_detect(var, "_mu"))

# # Save seasonal version
# horizons_out_szns <- read_csv(
#   here::here("Data/projections/Cmilestones_all_seasons_test.csv"),
#   col_types = cols(
#     Season = col_character(),
#     var = col_character(),
#     ref_period = col_character(),
#     temp_horizon = col_character(),
#     species = col_character(),
#     scenario = col_character(),
#     pt_id = col_double(),
#     val = col_double()
#   ))


# Hexagonal grid simple feature geometry
hex_grid <- read_sf(here::here("Data/spatial/hex_grid.geojson"))


# Projection data before reactive selection
species_projection_list <- horizon_projections %>% 
  split(.$comname)


##### User Selections "_opts"  ####


# Names of species
species_opts <- sort(unique(horizon_projections$comname))
species_opts <- setNames(species_opts, str_to_title(species_opts))

#  SSP scenarios
scenario_opts <- c(
  "Sustainable Development" = "CMIP6_SSP1_26",
  "Fossil-Fueled Development" = "CMIP6_SSP5_85")

# Temperatures Above Modern Climate
horizon_opts  <-  str_c(c(0, 0.5, 1, 1.5, 2, 2, 3, 4), "C")







##### Species Graphic Assets  ####

# This could be text and any images/artwork associated with a species
# Not a huge deal, but would add to the baseline page


# Also need species baseline characteristics:
# average temp/depth preferences
# average size/weight
# This should all be in some simple small csv





####____________________####
#### R Generated Content  ####

# These match the two user selection controls on the App
species_choice <- "Atlantic cod"
scenario_choice <- "CMIP6_SSP5_85"
horizon_choice <-  "0.5C"

# Filter
annual_species_i <- species_projection_list %>% 
  pluck(species_choice) 

# At this point we can join back in the lat/lon or the grid
annual_i   <- left_join(annual_species_i, hex_grid, by = join_by(pt_id))

# seasonal versions
#seasonal_i <- left_join(seasonal_species_i, hex_grid, by = join_by(pt_id))
#seasonal_species_i <- horizon_szns %>% split(.$comname) %>% pluck(species_choice) #%>% filter(scenario == scenario_choice)



# Split the two scenarios

# SSP1
ssp1_base <- annual_i %>% 
  filter(
    scenario == "CMIP6_SSP1_26", 
    temp_horizon == "0C",
    str_detect(var, "_mu")) %>% 
  st_as_sf()
ssp1_i <- annual_i %>% 
  filter(
    scenario == "CMIP6_SSP1_26", 
    temp_horizon == horizon_choice,
    str_detect(var, "_mu")) %>% 
  st_as_sf()

# SSP5








#####  Demo Content  ####





#### 1. Baseline Biomass Density  ####

# This should be made reactively
ssp5_base <- annual_i %>% 
  filter(
    scenario != "CMIP6_SSP1_26", 
    temp_horizon == "0C", 
    str_detect(var, "_mu")) %>% 
  st_as_sf()

# Getting the max 10^x value for scales
max_l10 <- 10^(round(log10(max(ssp5_base$val)))+1)

# This is the map
demo_base_map <- ssp_proj_map_static(
  dist_df          = ssp5_base,
  scenario         = scenario_choice,
  horizon_choice   = "0C",
  species_option   = species_choice,
  max_l10          = max_l10)




#### 2. Projected Biomass Density  ####

# This should also be filtered reactively
ssp5_i <- annual_i %>% 
  filter(
    scenario != "CMIP6_SSP1_26", 
    temp_horizon == horizon_choice, 
    str_detect(var, "_mu")) %>% 
  st_as_sf()

# And the map
demo_proj_map <- ssp_proj_map_static(
  dist_df = ssp5_i,
  scenario = scenario_choice,
  horizon_choice = horizon_choice,
  species_option = species_choice,
  max_l10 = max_l10
  )



#### 3. Projected Change in Biomass Density  ####


# this is the map
demo_diff_map <- map_difference(
  species_choice = species_choice, 
  ssp_base = ssp5_base, 
  ssp_projection = ssp5_i, 
  horizon = "0.5C", 
  col_lims = c(-5, 5)
  )



#### 4. Environmental Preference Curves  ####

# 



#### 5. Projected Biomass Timeseries  ####

# Thinking we do colors for SSP scenario
# make the annual average bold, make the seasons faded
# then add direct labels to the ends











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


####  Modules ####
##### IDs: species, scenario, horizon,  
# Sidebar elements (e.g., filter controls)
sideUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Select Species
    selectInput(
      inputId = ns("in_species"),
      label = "Species Name", 
      choices = unique(species_opts),
      selected = "halibut"),
    
    # 2. Pick a climate source
    selectInput(
      inputId = ns("in_scenario"),
      label = "Climate Scenario", 
      choices = as.character(scenario_opts),
      selected = "Fossil-Fueled Development"),
    
    # 3. Select the temperature horizon about baseline
    selectInput(
      inputId = ns("in_horizon"),
      label = "Increase in Region-Wide Temperatures:", 
      choices = horizon_opts, 
      selected = "0.5C")
      )
}


# The UI end of this works:
# Module for creating an output card of a species distribution
projection_map_ui <- function(id){
  
  # Build the card to contain everything
  card(
    
    # Card Header Contents
    card_header(
      class = "bg-dark", 
      "Distribution Under Average Baseline Conditions"),
    
    # Card Body Content
    card_body(
      
      # Headline text above the plot - can be swapped with textoutput to be reactive
      markdown("Baseline Density Distribution"),
      
      # This is where the focal element goes:
      card_body(
        class = "p-distribution_map",
        plotOutput(
          NS(id, "proj_map"), 
          height = "100%", 
          width = "100%")
        
      ) # Close cardbody
    ), # Close cardbody
    
    # Footer information should be a module if dynamic
    card_footer(
      class = "fs-6",
      "Projected distributions not based on stock recovery status or any ecological interactions.")
    
  ) # Close the card
  
}




####  Breaking HERE  ####

# The reactive filtering of the species is breaking ####
# Seems like the inputs aren't being accessed correctly

# # Testing the input  filters:
# input <- list(
#   "in_species" = "halibut", 
#   "in_scenario" = "SSP5 8.5", 
#   "in_horizon" = "0C")

# Filter the species/decade/season and eventually SSP to display as maps/plots
# This selects data for the baseline and for temporal trends



####  Modules: Data Filtering  ####

# Filter the data based on species and ssp
filterSpecies_server <- function(id, species_projection_list){  

    moduleServer(
      id,
      function(input, output, session){
        
        # Get dataset based on user inputs:
        # subsets from dataList or baseline_dataList
        df <- reactive({
          
          # Perform the filtering used to make the baseline map
          df <- species_projection_list %>%
            pluck(input$in_species) %>%
            filter(scenario == input$in_scenario) %>%
            left_join(hex_grid, by = join_by(pt_id))
          
          return(df)
          
        }) # End Reactive
        
    } # Close module function
    
  ) # Close moduleServer
  
} # End filtering



####  We Could Further Split here for baseline, projection, and difference

# Filter the species and scenario data for just the baseline data
filterBaseline_server <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Take projection data and subset by user selections
      df_r <- reactive({
        df <- df() %>% dplyr::filter(temp_horizon == "0C")
        return(df)
      })
    })
}


# Filter the species and scenario data for just the projection data
filterProjection_server <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Take projection data and subset by user selections
      df_r <- reactive({
        df <- df() %>% dplyr::filter(temp_horizon == input$in_horizon)
        return(df)
      })
    })
}

#### Modules: Plot generation  ####


####  Problem Identified - namespace for inputs not working  ####

# Server side: Baseline Map Generation
projection_map_server <- function(id, in_data) {
  
  moduleServer(
    id, 
    function(input, output, session) {
    
    # # Get some reactive data:
    # dat <- reactive(
    #   horizon_projections %>% 
    #     filter(
    #       comname == "halibut",
    #       temp_horizon == "0C",
    #       scenario == "CMIP6_SSP5_85") %>% 
    #       # comname == input$in_species,
    #       # temp_horizon == input$in_horizon,
    #       # scenario == input$in_scenario) %>% 
    #     left_join(hex_grid, by = join_by("pt_id")))
    
    # Use the sidebar server details to load the reactive data
    dat <- in_data
    
    # Add plot to output for access into UI
    output$proj_map <- renderPlot({
      
      map_out <- ssp_proj_map(              # Function to produce the plot
        dist_df = dat,         # simple features dataframe that is to be mapped
        max_l10 = 10000)
      print(map_out)
      
    })
      
  })
}








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
    title = "Select a Species to See its Projections:",
    
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
    title = "Baseline Distribution", value = "baseline_tab",
    
    #### 1. Baseline Conditions  ####
    
    ##### a. Description & Map #### 
    layout_column_wrap(
      width = 1/2, 
      height = 800,
      
      # Card 1: Written Description
      card(
        card_header(
          class = "bg-dark",
          "Species Name Baseline Characteristics"),
        card_body(
          markdown("**What do we mean by baseline?**"),
          card_body(
            class = "p-static",
            #plotly_widget
            strong("To set a standard from which to compare against, we've used a ten-year period of 2010-2019 as our modern baseline."),
            p("Scientists lean on observations from a baseline period of to learn species preferences 
               for different environments. Using data from these periods scientists model how individual 
                species respond to changes in the natural environment like depth, salinity, and temperature. 
                These relationships allow scientists to make an educated guess on distribution 
                changes under projected future conditions."),
            
            p("The map to the right displays the average density that this species can be found. Habitat
              use reflects the combined influence of the physical factors used in the model: surface and bottom temperature, 
              surface and bottom salinity, and any tendency to cluster around important seafloor features.")
            # This is where reactive outputs would go
          )),
        card_footer(
          markdown("[Learn more about the ecology of this species](https://www.fisheries.noaa.gov/species/atlantic-cod)")
        )
      ),
      
      #### Card UI  Testing  ####
      # Card Module testing
      projection_map_ui("baseline_bio_map")
      

    
    ), # End layout_col for map and baseline description
    
    
    
    ##### a. Value Boxes  ####
    # Second layout wrap height for the species metrics  
    layout_columns(
      fill = FALSE, row_heights = 300,
      
      # Value Box 1: Temperature
      card(
        style ="border-color: #FFFFFF; border-radius: .150rem",
        # Value Boxes for Characteristics
        value_box(
          title = "Preferred Temperature",
          value = str_c(45, deg_c),
          showcase = bsicons::bs_icon("thermometer-sun")
        )),
      
      # Value Box 2: Depth
      card(
        style ="border-color: #FFFFFF; border-radius: .150rem",
        value_box(
          title = "Preferred Depth",
          value = str_c(120, " meters"),
          showcase = bsicons::bs_icon("align-bottom"),
          theme_color = "dark"
        )),
      
      # Value Box 3: Bodymass
      card(
        style ="border-color: #FFFFFF; border-radius: .150rem",
        value_box(
          title = "Average body mass",
          value = str_c(740, "g"),
          showcase = bsicons::bs_icon("handbag"),
          theme_color = "secondary")),
      
      # Value Box 4: Average Length
      card(
        style ="border-color: #FFFFFF; border-radius: .150rem",
        value_box(
          title = "Average length",
          value = str_c(83, "cm"),
          showcase = bsicons::bs_icon("rulers"),
          theme_color = "secondary"))
      
    ) # End Value Box layout_columns()
    
    
  ), # End nav_panel 1
  
    #### Tab 2: Projected Biomass at Temperature Horizon  ####
    nav_panel(
      title = "Projected Distribution at +0.5C", 
      value = "projected_bio_tab",
      
      ##### a. Description & Map #### 
      layout_column_wrap(
        width = 1/2, 
        height = 800,
        
        # Card 1: Written Description
        card(
          card_header(
            class = "bg-dark",
            "Species Characteristics"),
          card_body(
            markdown("How are Future Distributions Estimated?"),
            card_body(
              class = "p-static",
              p("The display to the right displays the predicted biomass distribution 
                at the point in our ensemble climate model when the shelf-wide temperatures 
                approach +0.5C above current conditions (2010-2019). These projections are 
                modeled using surface temperature, bottom temperature, depth, and spatial correlation
                patterns. Each species is modeled independently to capture their specific habitat needs."),
              p("The further that the regional climate moves from current conditions, the larger 
                the change in distribution species must undergo to follow suitable conditions, and the
                harder it becomes to predict without accounting for other key ecological needs.")
              # This is where reactive outputs would go"
              
              )),
              card_footer(
                markdown("[Shared Socio-Economic Pathways](https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/)")
              )
                      ),
              
              # Card 2: Projected Species Distribution
              projection_map_ui("projected_bio_map") 

      ) # End layout_col for map and baseline description
      
    ), # End Tab 2
    
    
    #### Tab 3. Difference in Biomass at Temperature Horizon  ####
    
    nav_panel(
      title = "Changes in Distribution", "difference_map_tab",
      ##### a. Description & Map #### 
      layout_column_wrap(
        width = 1/2, 
        height = 800,
        
        # Card 1: Written Description
        card(
          card_header(
            class = "bg-dark",
            "Species Characteristics"),
          card_body(
            markdown("What does SSP5-8.5 Mean"),
            card_body(
              class = "p-static",
              #plotly_widget
              p("The display to the right displays how biomass is projected to change
                under future conditions consistent with our ensemble climate data  
                when the shelf-wide temperatures approach +0.5C above current conditions (2034-2038)."),
              p("Something boilerplate here")
              # This is where reactive outputs would go
            )),
        card_footer(
          markdown("[Shared Socio-Economic Pathways](https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/)")
        )
        ),
        
        # Card 3: Projected Difference Map
        # Card Module testing
        projection_map_ui("projected_diff_map")
        
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
  species_dat <- filterSpecies_server("sidebar_ID", species_projection_list = species_projection_list)
  baseline_dat <- filterBaseline_server("sidebar_ID", df = species_dat)
  projection_dat <- filterProjection_server("sidebar_ID", df = species_dat)
  
  
  #### Map Modules  ####

  # # 2. Build the map/plot content for the UI
  # ID's should match the cards for their outputs
  projection_map_server(id = "baseline_bio_map", in_data = baseline_dat)
  projection_map_server(id = "projected_bio_map", in_data = projection_dat)
  projection_map_server(id = "projected_diff_map", in_data = projection_dat)
  
  
  #### Static Demo Content  ####
  
  # # # # Baseline Distribution Map
  # # Projected Biomass Map
  # output$baseline_bio_map <- renderPlot({
  #   demo_base_map
  # })

  # output$baseline_bio_map <- renderPlot({
  #      # demo_base_map
  #      ssp_proj_map(
  #        dist_df          = baseline_df,
  #        scenario         = input$in_scenario,
  #        horizon_choice   = "0C",
  #        species_option   = input$in_species,
  #        max_l10          = max_l10)
  # 
  # })
  # # Projected Biomass Map
  # output$projected_bio_map <- renderPlot({
  #   demo_proj_map
  # })
  # 
  # # Change in Biomass App
  #  output$projected_diff_map <- renderPlot({
  #   demo_diff_map
  # })
  
}


# Running it Normally
shinyApp(ui, server)


# # # Running the themer from bslib
# run_with_themer(shinyApp(ui, server))
