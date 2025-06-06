#### Modules: Plot generation  ####


# The UI end of this works:
# Module for creating an output card of a species distribution
preference_card_ui <- function(id){
  
  # Build the card to contain everything
  # Card 1: Written Description
  card(
    card_header(
      # class = "bg-dark",
      style = "background-color: #004966;  color: white;",
      "Change in Distribution under Projected Climate Change"),
    
    card_body(
      h3(markdown("**Projected Biomass Change from Shifting Conditions**")),
      
      p("The display below shows where regional conditions are projected to be (vertical lines),
      relative to this species observed habitat preferences (preference curves)."),
      
      card_body(
        class = "p-static",
        #plotly_widget
        
        plotOutput(
          NS(id, "pref_curves"), 
          height = "100%", 
          width = "100%")
      )),
    
    card_footer(
      markdown("[Learn more about model diagnostics here.](https://climatedata.ca/resource/understanding-shared-socio-economic-pathways-ssps/)")
    )
  ) # Close the card
  
}



# Server side: Preference Curve Plot Creation
preference_curve_server <- function(id, in_data) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Use the sidebar server module and user inputs to load the reactive data
      # this is done outside this module
      
      # Add plot to output for access into UI
      output$pref_curves <- renderPlot({
        
        # Function to produce the plot
        plot_out <- plot_preference_curves(              
          pref_dat = in_data,  
          reactive = T)
        print(plot_out)
        
      })
      
    })
}

