#### Modules: Plot generation  ####


# The UI end of this works:
# Module for creating an output card of a species distribution
projected_timeseries_ui <- function(id){
  
  # Build the card to contain everything
  # Card 1: Written Description
  card(
    card_header(
      class = "bg-dark",
      "Projecting Based on Future Conditions"),
    card_body(
      markdown("How are Future Distributions Estimated?"),
      card_body(
        class = "p-static",
        p("Scientist make projections using the observed changes in
           the presence/absence + abundances of species with the 
           surface & bottom temperatures from the baseline period. These
           relationships are then projected forward using climate 
           model ensembles."), 
        p("Displayed below are the longer-term trajectories four four major areas of interest."),
        plotOutput(
          NS(id, "proj_timeseries"), 
          height = "100%", 
          width = "100%")
        
      )),
    card_footer(
      markdown("[Learn more information on how future conditions are estimated](https://gulfofmaine.github.io/sdm_workflow/docs/)")
    )
  ) # Close the card
  
}



# Server side: Baseline Map Generation
projected_timeseries_server <- function(id, in_data) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Use the sidebar server module and user inputs to load the reactive data
      # this is done outside this module
      dat <- in_data
      
      # Add plot to output for access into UI
      output$proj_timeseries <- renderPlot({
        
        # Function to produce the plot
        plot_out <- ssp_projected_timeseries(              
          timeseries_data = dat,  
          reactive = T)
        print(plot_out)
        
      })
      
    })
}
