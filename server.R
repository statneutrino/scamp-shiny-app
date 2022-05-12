# Define server logic for random distribution app ----
function(input, output) {
  
  # LOAD DATA
  
  # Simulate for now
  N <- 500
  age_options <- c("12 or under", "13-17", "18-25", "36-45", "46-55", "56-65", "66-75", "76+")
  
  df <- data.frame(
    sex = ifelse(rbinom(N, 1, 0.5) == 0, "Female", "Male"),
    age = rpois(N, 30) + rnorm(N, sd=6) %>% round(),
    screentime = rnorm(N, 240, 50) %>% round(.)
  )
  
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  histogram_reactive <- reactive({
    
    # Get inputs from sidebar
    gender <- input$button_gender
    age_bracket <- input$age_slider
    
    # Filter df according to gender
    if(gender != "All"){
      df_out <- df %>% filter(sex == gender)
    } else {
      df_out <- df
    }
    # Filter df according to age
    df_out <- df_out %>% filter(age >= age_bracket[1] & age <= age_bracket[2])
    
    df_out$screentime
  })
  
  # Get information
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    
    hist(histogram_reactive(),
         main = "Test",
         col = "#75AADB", border = "white")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
}