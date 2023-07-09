####  Packages  ####
library(shiny)
library(bslib)
library(here)
library(sf)
library(tidyverse)
library(waiter)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(bsicons)



#### Support Functions:  ####
source(here::here("R/app_support.R"))




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








####  Data Prep  ####



# # Read the cropped land coverage:

# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))
#land_wgs <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs4326.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))
#hague_wgs <- here::here("Data/spatial", "hagueline_crs4326.geojson")


#### User Selection  Lists  ####

# List of the available species
speciesList <- sort(
  c("American lobster", #"Black sea bass", 
    "Atlantic cod", 
    "Atlantic halibut", 
    "Atlantic herring", 
    "Haddock", 
    "Sea scallop", 
    "Yellowtail flounder"))


# List of the available SSP scenarios
sspList <- c(
  "Emissions Reductions"      = "SSP1 - 2.6",
  "Fossil-Fueled Development" = "SSP5 - 8.5")





####_________________________####

#### File Loading:  ####


####__Test Data  ####

# Load data that is prepared for deployment:
# Data is saved following the preparation steps to minimize processing times
# data_resource matches file directory structure within project directory
# Options: baseline, ssp1, ssp5, model_outputs



# Load Pre-Prepped App Data::
baseline_dataList <- fetch_appdata(data_resource = "baseline_data")






####__ Real Data  ####



#### 1. Species List  ####

#### 1a. Species Assets  ####

# This could be text and any images/artwork associated with a species
# Not a huge deal, but would add to the baseline page


# Also need species baseline characteristics:
# average temp/depth preferences
# average size/weight


#### 2. Baseline Distributions  ####


#### 3. SSP1 Projections  ####


#### 4. SSP 5 Projections  ####



####  Static Base Map Options  ####

# Test options
test_species <- "American Lobster"
display_option <- "baseline"


# Map Prep:
####  NOTE: SHOULD BE DONE BEFORE APP

# Baseline data prep:
# Cleans up geojson to minimum needs for map
# drops un-needed columns
baseline_prep <- function(baseline_dat){
  baseline_prepped <- baseline_dat %>% 
    mutate(Season = factor(Season, levels = c("Spring", "Summer","Fall"))) %>% 
    bSquare(25000*25000, coords = c("Lon", "Lat")) # 
  
  # Drop columns we don't need
  baseline_prepped <- baseline_prepped%>% 
    select(Lon, Lat, Season, Climate_Scenario, Species, Log_Biomass, geometry)
}



# Use one for testing
test_baseline <- baseline_prep(baseline_dat = baseline_dataList$American_lobster_ST_SSP5_85_mean_baseline.geojson)


# Make a honeycomb grid from one output
area_honeycomb_grid <-
  test_baseline %>% 
  distinct(geometry) %>% 
  st_make_grid(
    #x = test_baseline, 
    #cellsize = c(25000, 25000), #25km x #25km
    cellsize = c(30000, 30000), #30km x #30km
    what = "polygons", 
    square = FALSE)

# Trim it?
# Literally so annoying, need to make it an sf class
# https://github.com/r-spatial/sf/issues/1148
clipped_comb <- area_honeycomb_grid %>%
  st_as_sf() %>% 
  st_filter(test_baseline) 

# Don't need full grid
rm(area_honeycomb_grid)
clipped_comb


####  Bivariate Map  ####



# GMRI color palette using blue and yellow
gmri_bipal_2 <- c(
  "lowX_lowY"   = "#e8f7ff", 
  "lowX_midY"   = "#a0d5fa", 
  "lowX_highY"  = "#53b1f5", 
  "midX_lowY"   = "#e8c694", 
  "midX_midY"   = "#a0c694",
  "midX_highY"  = "#53b194", 
  "highX_lowY"  = "#e88a11", 
  "highX_midY"  = "#a08a11", 
  "highX_highY" = "#538a11"
)


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

# Code for Bivariate map
# "clean" is a dataframe that has a column indicating outcome color for bivariate palette
# bivar_map <- st_join(clipped_comb, clean, join = st_intersects) %>% 
#   filter(Decade == "2030") %>% 
#   ggplot() +
#   geom_sf(aes(fill = bivar_lab), show.legend = F) +
#   geom_sf(data = land_sf, color = "white", fill = "gray40") +
#   geom_sf(data = hague_sf, linewidth = 1, color = "black") +
#   coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000) , expand = F, crs = 32619) +
#   theme_map() +
#   scale_fill_manual(values = gmri_bipal_2) +
#   labs(title = "Yellowtail Flounder 2030:\nBivariate Seasonal Effect Map")


#### 1. sf + ggplot



#### 2. tmap



















####  Interactive Base Map  ####

# # Plotly plot for space
# plotly_widget <- plot_ly(x = diamonds$cut, type = "histogram") %>%
#   config(displayModeBar = FALSE) %>%
#   layout(margin = list(t = 0, b = 0, l = 0, r = 0))


# Set up the basemap to lay data on:
leaflet_widget <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery ) %>% 
    # addProviderTiles(providers$Esri.WorldGrayCanvas ) %>% 
    # addProviderTiles(providers$OpenStreetMap ) %>% 
    setView(lng = -68.7, lat = 42.3, zoom = 5)  %>%
    addMiniMap()



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






####_____________________####
####_____________________####
####  Define UI Displays and Modules  ####


# Species Display Card
# Image/artwork for the species
# General Biology


# Map displays:
## Baseline
## SSP1
## SSP2


# Download buttons
downloadUI <- function(id) {
  ns <- NS(id)
  tagList(downloadButton(ns('downloadPlot'), 'Download Plot'),
          downloadButton(ns('downloadData'), 'Download Data'))
}




####  Random Static Elements  ####
link_ssp <- tags$a("SSP Pathways", href = "https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/", target = "_blank")


####  SideBar Elements  ####
# Sidebar elements (e.g., filter controls)
sideUI <- function(id) {
  
  
  ns <- NS(id)
  tagList(
    
    # Select Species
    selectInput(
      inputId = ns("species"),
      label = "Species Name", 
      choices = unique(speciesList),
      # selected = unique(speciesList)[1]
      selected = "Atlantic halibut"
    ),
    # # Select the survey season
    # selectInput(
    #   inputId = ns("season"),
    #   label = "Survey Season", 
    #   choices = c("Spring", "Summer", "Fall"),
    #   selected = "Spring"
    # ),
    
    # Select the forecast decade
    selectInput(
      inputId = ns("projection_decade"),
      label = "Decade Projection to Display", 
      choices = c("2050", "2100")
      )
    
  )
  
}


####_________________________####
####_________________________####

####  App Design and UI Layout:  ####

# Define UI for application that draws a histogram



# Build using a navbar page to hold navigation tabs:
ui <- page_navbar(
  
  # 1. App Title
  title = "Projected Species Distribution Changes Under Shared Socio-Economic Pathways", 
  
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
      sideUI("side1"),
      startExpanded = TRUE),
      br(),
      p("Changing the above options will update the map projections of
         how our models predict species will move based on
         their preferences and climate model ensemble data.")
      
    
    
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
          "-Species Name- Baseline Characteristics"),
        card_body(
          markdown("**What do we mean by baseline?**"),
          card_body(
            class = "p-static",
            #plotly_widget
            strong("To set a baseline from which to compare against, we consider the period of 2010-2019 as our baseline."),
            p("Scientists lean on observations from a baseline period of to learn species preferences 
               for different environments. Baseline periods have information on both the environment, 
               and species abundance. Having both pieces, scientists can model the relationships that individual 
               species have with features of the natural environment like depth, salinity, and temperature. 
               Understanding how each species adjusts its habitat use based on these conditions allows scientists
               to make an educated guess on distribution changes under projected future conditions."),
            
            p("The map to the right displays the average density that this species can be found at, and how that reflects a
              combined influence of the physical factors used in the model: surface and bottom temperature, 
              surface and bottom salinity, and any tendency to cluster around important seafloor features.")
            # This is where reactive outputs would go
          )),
        card_footer(
          markdown("[Shared Socio-Economic Pathways](https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/)")
        )
      ),
      
      # Card 2: Baseline Map
      card(
        card_header(
          class = "bg-dark",
          "Distribution Under Average Baseline Conditions"),
        card_body(
          markdown("Baseline Density Distribution"),
          card_body(
            class = "p-interactive",
            lobster_map  # This is where you'd do leafletoutput
            # Example:
            #leafletOutput(outputId = "Lob_zone_map", height = "600px", width = "600px")
          )
        ),
        card_footer(
          class = "fs-6",
          "Footnote")
        
      ) # End Card 2     
    
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
  
    #### 2. SSP1 2.6 Contents  ####
    # Tab 2: SSP1 2.6
    ####  IDEA  ####
    # Use value boxes to highlight the pathway number and the radiative forcing number
    nav_panel(
      title = "Sustainability Pathway: SSP1 2.6", 
      value = "ssp1_tab",
      
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
            markdown("What does SSP1-2.6 Mean"),
            card_body(
              class = "p-static",
              #plotly_widget
              p("
              Sit suspendisse sapien lectus ornare: netus ultrices diam volutpat: molestie ac? Laoreet sollicitudin etiam posuere potenti at duis, torquent eleifend feugiat. Congue dapibus
phasellus et facilisis eros dignissim maecenas pretium aenean semper luctus curabitur magna sociosqu!"),
              p("Consectetur commodo tempor mattis dis ac, tellus, dignissim quis orci sapien etiam. Mattis sollicitudin lobortis tellus sociosqu habitant imperdiet elementum dignissim
              sagittis montes quisque. Pulvinar sed egestas mus suscipit. Augue convallis mi leo vitae cubilia montes – fringilla tincidunt vivamus! A at malesuada vel blandit fusce cras,
              tincidunt quis. Venenatis praesent pretium mus non et magnis diam – enim aliquam, penatibus fusce convallis – ultrices, diam accumsan rutrum orci etiam?")
              # This is where reactive outputs would go
                          )),
              card_footer(
                markdown("[Shared Socio-Economic Pathways](https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/)")
              )
                      ),
              
              # Card 2: SSP1 Map
              card(
                card_header(
                  class = "bg-dark",
                  "Distribution Under SSP1"),
                card_body(
                  markdown("SSP1 Density Change"),
                  card_body(
                    class = "p-interactive",
                    lobster_map  # This is where you'd do leafletoutput
                    # Example:
                    #leafletOutput(outputId = "Lob_zone_map", height = "600px", width = "600px")
                  )
                ),
                card_footer(
                  class = "fs-6",
                  "Footnote")
              
            ) # End Card 2     

      ) # End layout_col for map and baseline description
      
    ), # End Tab 2
    
    
    #### 3. SSP 5 8.5 Contents  ####
    # Tab 3: SSP5 8.5
    nav_panel(
      title = "Fossil-fueled Development Pathway: SSP5 8.5", value = "ssp5_tab",
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
              p("
              Sit suspendisse sapien lectus ornare: netus ultrices diam volutpat: molestie ac? Laoreet sollicitudin etiam posuere potenti at duis, torquent eleifend feugiat. Congue dapibus
phasellus et facilisis eros dignissim maecenas pretium aenean semper luctus curabitur magna sociosqu!"),
              p("Consectetur commodo tempor mattis dis ac, tellus, dignissim quis orci sapien etiam. Mattis sollicitudin lobortis tellus sociosqu habitant imperdiet elementum dignissim
                      sagittis montes quisque. Pulvinar sed egestas mus suscipit. Augue convallis mi leo vitae cubilia montes – fringilla tincidunt vivamus! A at malesuada vel blandit fusce cras,
                      tincidunt quis. Venenatis praesent pretium mus non et magnis diam – enim aliquam, penatibus fusce convallis – ultrices, diam accumsan rutrum orci etiam?")
              # This is where reactive outputs would go
            )),
        card_footer(
          markdown("[Shared Socio-Economic Pathways](https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/)")
        )
        ),
        
        # Card 2: SSP1 Map
        card(
          card_header(
            class = "bg-dark",
            "Distribution Under SSP1"),
          card_body(
            markdown("SSP1 Density Change"),
            card_body(
              class = "p-interactive",
              lobster_map  # This is where you'd do leafletoutput
              # Example:
              #leafletOutput(outputId = "Lob_zone_map", height = "600px", width = "600px")
            )
          ),
          card_footer(
            class = "fs-6",
            "Footnote")
          
        ) # End Card 2   

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
      value = "data_rqst_panel",
      p("Some background content, this should be a markdown doc that has disclaimers and links to the full dataset")
      ), # End data request information
    ####__Content Break__####
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
  
  
  # Default Shiny Output
  # output$distPlot <- renderPlot({
  # 
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #          xlab = 'Waiting time to next eruption (in mins)',
  #          main = 'Histogram of waiting times')
  # 
  # })
  
}


# Running it Normally
shinyApp(ui, server)


# # # Running the themer from bslib
# run_with_themer(shinyApp(ui, server))
