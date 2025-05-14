#### Modules: Plot generation  ####


# The UI end of this works:
# Module for creating an output card of a species distribution
difference_map_ui <- function(id){
  
  # Build the card to contain everything
  card(
    
    # Card Header Contents
    card_header(
      style = "background-color: #004966;  color: white;",
      "Changes in Biomass Distribution Under Future Climate Scenario"),
    
    # Card Body Content
    card_body(
      
      # Headline text above the plot - can be swapped with textoutput to be reactive
      h3(markdown("**Projected Impacts of Environmental Change**")),
      
      p("The display below maps how biomass is projected to change
         under future conditions consistent with our ensemble climate data."),
      
      # This is where the focal element goes:
      card_body(
        class = "p-distribution_map",
        
        
        # Map Display
        plotOutput(
          NS(id, "diff_map"), 
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





# Server side: Baseline Map Generation
difference_map_server <- function(id, in_data) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Use the sidebar server module and user inputs to load the reactive data
      # this is done outside this module
      dat <- in_data
      
      # Add plot to output for access into UI
      output$diff_map <- renderPlot({
        
        # Function to produce the plot
        map_out <- ssp_difference_map(              
          dist_df = dat, 
          reactive = T)
        print(map_out)
        
      })
      
    })
}



