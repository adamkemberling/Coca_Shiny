# mapping module ----
# Module for creating an output card of a species distribution
projection_map_ui <- function(id){
  
  # Card 2: Baseline Map
  card(
    
    # Header
    card_header(
      class = "bg-dark",
      "Distribution Under Average Baseline Conditions"),
    
    # Body Content
    card_body(
      
      # Headline text above the plot - can be swapped with textoutput to be reactive
      markdown("Baseline Density Distribution"),
      
      # This is where the focal element goes:
      card_body(
        class = "p-interactive",
        plotOutput(
          outputId = NS(id, "proj_map"), 
          height = "100%", 
          width = "100%")
        
      ) # Close cardbody
    ), # Close cardbody
    
    # Footer infomration should be a module if dynamic
    card_footer(
      class = "fs-6",
      "Footnote")
    
  ) # Close the card
  
}



# Server side details:
####  NEED TO ADD MAPPING FUNCTION:  ####
# Add at viz_monthly() line

projection_map_server <- function(id, df, in_scenario, in_horizon, in_species) {
  
  moduleServer(id, function(input, output, session) {
    
    # Make the reactive element
    plot <- reactive({
      ssp_proj_map(                    # Function to produce the plot
        dist_df = df(),                # simple features dataframe that is to be mapped
        scenario = in_scenario,        # SSP scenario for label generation
        horizon_choice = in_horizon,   # The amount hotter than current climate being projected
        horizon_year_key,              # Key to the boundary time for the temperature horizon
        species_option = in_species,   # The species name
        max_l10)                       # Color scale limit
      })
    
    # Add plot to output for access into UI
    output$proj_map <- renderPlot({plot()})
    
    
  })
}




# demo function: 
plot_demo <- function() {
  
  # Shit we need
  df <- ssp5_base
  
  # UI usage
  ui <- page_navbar(
    nav_panel(title = "testing a module and using bslib elements"),
    layout_columns(
      projection_map_ui("x")))
  
  # Server Side Usage
  server <- function(input, output, session) {
    projection_map_server(
      id = "x", 
      df = reactive({df}),
      in_scenario = "CMIP6_SSP1_26",
      in_horizon = "0.5C",
      in_species = "Atlantic cod")
  }
  shinyApp(ui, server)
  
}
