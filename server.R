library(tidyverse)

# Define server logic for random distribution app ----
function(input, output) {
  
  # LOAD DATA
  
  # Simulate for now
  N <- 500
  age_options <- c("12 or under", "13-17", "18-25", "36-45", "46-55", "56-65", "66-75", "76+")
  
  df <- data.frame(
    sex = factor(ifelse(rbinom(N, 1, 0.5) == 0, "Female", "Male")),
    age = rpois(N, 30) + rnorm(N, sd=6) %>% round()
  )
  df$screentime <- rnorm(N, 8 * df$age + 60 + (as.numeric(df$sex)-1)*50, 30) %>% round(.)
  
  
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
  
  # defined below then use the value computed from this expression
  compare_reactive <- reactive({
    
    # Get inputs from sidebar
    gender1 <- input$compare_gender1
    gender2 <- input$compare_gender2
    age_bracket1 <- input$compare_age_range1
    age_bracket2 <- input$compare_age_range2
    include_group2 <- input$group2_checkbox
    
    # Filter group1 according to gender
    if(gender1 != "All"){
      df_group1 <- df %>% filter(sex == gender1)
    } else {
      df_group1 <- df
    }
    # Filter group1 according to age
    df_group1 <- df_group1 %>% filter(age >= age_bracket1[1] & age <= age_bracket1[2])
    df_group1$group <- "Group 1"
    
    if(include_group2){
      
      # Filter group 2 according to sex
      if(gender2 != "All"){
        df_group2 <- df %>% filter(sex == gender2)
      } else {
        df_group2 <- df
      }
      # Filter group2 according to age
      df_group2 <- df_group2 %>% filter(age >= age_bracket2[1] & age <= age_bracket2[2])
      df_group2$group <- "Group 2"
      
      output_df <- rbind(df_group1, df_group2)
      
    } else {
      output_df <- df_group1
    }
    output_df
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
  output$compare_hist <- plotly::renderPlotly({
    plotly::ggplotly(ggplot(compare_reactive(), aes(x=screentime, fill=group)) + 
      geom_histogram(alpha=0.9, position="identity", bins = 10) + 
      ggdark::dark_theme_gray() +
      scale_fill_brewer(palette = "Pastel2"))
  })
  
  # Generate an HTML table view of the data ----
  output$compare_violin <- renderPlot({
    ggplot(compare_reactive(), aes(x=group, y=screentime, fill=group)) + 
      geom_violin(alpha=0.6) + theme(legend.position="none") + 
      geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1) +
      ggdark::dark_theme_gray() +
      scale_fill_brewer(palette = "Pastel2") + 
      stat_summary(fun = "mean",
                   geom = "point",
                   color = "red", show.legend=FALSE)
  })

  output$t_test <- renderUI({
    
    if("Group 2" %in% unique(compare_reactive()$group)){
      HTML(paste(paste("Mean screentime in Group 1 is: ", compare_reactive() %>% 
                        filter(group == "Group 1") %>%
                        select(screentime) %>%
                        pull %>%
                        mean %>%
                        round), 
                 paste("Mean screentime in Group 2 is: ", compare_reactive() %>%
                        filter(group == "Group 2") %>%
                        select(screentime) %>%
                        pull %>%
                        mean %>%
                        round), 
                 paste("Observed difference in means is:", (compare_reactive() %>% 
                         filter(group == "Group 2") %>%
                         select(screentime) %>%
                         pull %>%
                         mean %>%
                         round) - (compare_reactive() %>% 
                                     filter(group == "Group 1") %>%
                                     select(screentime) %>%
                                     pull %>%
                                     mean %>%
                                     round)),
                 paste0("95% Confidence interval for absolute difference in means is: (",
                        t.test(
                          compare_reactive()$screentime[compare_reactive()$group == "Group 1"], 
                          compare_reactive()$screentime[compare_reactive()$group == "Group 2"]
                          )[4]$conf.int[1] %>% round, 
                        ", ",
                        t.test(
                          compare_reactive()$screentime[compare_reactive()$group == "Group 1"], 
                          compare_reactive()$screentime[compare_reactive()$group == "Group 2"]
                          )[4]$conf.int[2]%>% round,
                        ")"),
                 sep="<br/>"))
    } else {
      HTML(paste("Mean screentime in Group 1 is: ", compare_reactive() %>% 
                   select(screentime) %>%
                   pull %>%
                   mean %>%
                   round))
    }

  })
  
}