#### Modules: Plot generation  ####


# The UI end of this works:
# Module for creating an output card of a species distribution
projection_map_ui <- function(id){
  
  # Build the card to contain everything
  card(
    
    # Card Header Contents
    card_header(
      class = "bg-dark", 
      "Distribution Under Average Conditions"),
    
    # Card Body Content
    card_body(
      
      # Headline text above the plot - 
      #can be swapped with textoutput to be reactive
      markdown("Model-Predicted Density Distribution"),
      
      # This is where the focal element goes:
      card_body(
        class = "p-distribution_map",
        
        p("The map to the right displays the projected biomass  
           distribution based on habitat preferences & local environmental 
           conditions."), 
        
        # Map Display:
        plotOutput(
          NS(id, "proj_map"), 
          height = "100%", 
          width = "100%")
      ) # Close cardbody
    ), # Close cardbody
    
    # Footer information should be a module if dynamic
    card_footer(
      class = "fs-6",
      "Projected distributions not based on stock recovery status 
      or any ecological interactions.")
  ) # Close the card
  
}



# Server side: Baseline Map Generation
projection_map_server <- function(id, in_data, add_labels = F) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Use the sidebar server module and user inputs to load the reactive data
      # this is done outside this module
      dat <- in_data
      
      # Add plot to output for access into UI
      output$proj_map <- renderPlot({
        
        # Function to produce the plot
        map_out <- ssp_proj_map(            
          # simple features dataframe that is to be mapped  
          dist_df = dat,         
          reactive = T,
          add_labels = add_labels)
        print(map_out)
        
      })
      
    })
}


