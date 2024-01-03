####  Modules: Data Filtering  ####

# Filter the species/decade/season and eventually SSP to display as maps/plots
# This selects data for the baseline and for temporal trends

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
      choices = scenario_opts,
      selected = "Fossil-Fueled Development"),
    
    # 3. Select the temperature horizon about baseline
    selectInput(
      inputId = ns("in_horizon"),
      label = "Increase in Region-Wide Temperatures:", 
      choices = horizon_opts, 
      #choices = NULL, 
      selected = "0.5C")
  )
}



# Can we just update the horizons so we don't get drill-down errors?
# maybe i need a module?
updateHorizons <- function(input, output, session){
      
      # Get dataset based on user inputs:
      # subsets from dataList or baseline_dataList
     observe({
      req(input$in_scenario)
        
        #Update horizons using the scenario option
        if(input$in_scenario == "CMIP6_SSP1_26"){
          choices <- setNames("0.5C", str_c("+0.5", deg_c))}
        if(input$in_scenario != "CMIP6_SSP1_26"){
          vals <- c(0.5, 1, 1.5, 2,3,4)
          choices <- str_c(vals, "C")
          choices <- setNames(choices, str_c("+", vals, deg_c))}
        updateSelectInput(session, "in_horizon", choices = choices) 
        
      }) # End Observe
      
}





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
      
    }) # Close moduleServer
  
} # End filtering



####  Split away the baseline densities

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


# Filter the projection densities for just the projection data
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



# Build difference dataframe from baseline and projection difference
getDifference_server <- function(id, base_df, proj_df){
  moduleServer(
    id,
    function(input, output, session){
      # Put code to get the difference between base conditions and future state
      diff_data <- reactive({
        df <- get_difference(base_dat = base_df(), 
                             proj_dat = proj_df())
        return(df)
        
      })
      return(diff_data)
    }
  )
  
  
}



# Get the Correct Timeseries information:
getTimeseries_server <- function(id, density_timeseries){
  moduleServer(
    id,
    function(input, output, session){
      # Get dataset based on user inputs:
      # subsets from dataList or baseline_dataList
      df <- reactive({
        
        # Perform the filtering used to make the baseline map
        df <- density_timeseries %>%
          pluck(input$in_species) %>% 
          dplyr::filter(scenario == input$in_scenario,
            year>2019) %>% 
          mutate(temp_horizon = input$in_horizon)
        
        return(df)
        
      }) # End Reactive
      
      return(df)
    }
  )
  
  
}

# Filter the data based on species and ssp
filterPreferences_server <- function(id, pref_data, env_condition_data){  
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Get dataset based on user inputs:
      # subsets from dataList or baseline_dataList
      df <- reactive({
        
        # Build the dataset we need for the preference plot using inputs
        # a. conditions
        cond_x <- filter(env_condition_data, 
                         scenario == str_remove(input$in_scenario, "CMIP6_"), 
                         temp_horizon == input$in_horizon)
        
        
        # b. preferences
        prefs_x <- pref_data %>% pluck(input$in_species) 
        
        # build out regions
        prefs_x <- bind_rows(list(
          mutate(prefs_x, region = "Gulf of Maine"),
          mutate(prefs_x, region = "Georges Bank"),
          mutate(prefs_x, region = "Scotian Shelf"),
          mutate(prefs_x, region = "Mid-Atlantic Bight"))) %>% 
          mutate(region = factor(region, levels = c("Scotian Shelf", "Gulf of Maine", "Georges Bank", "Mid-Atlantic Bight")))
        
        # Join them:
        df <- left_join(prefs_x, cond_x)
        
        return(df)
        
      }) # End Reactive
      
    }) # Close moduleServer
  
} # End filtering

