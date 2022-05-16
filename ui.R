library(markdown)

navbarPage("SCAMP Screentime Data Explorer", theme = shinythemes::shinytheme("cyborg"),
           
  
  ## COMPARING DISTRIBUTIONS AND MEANS OF TWO GROUPS ##
  tabPanel("Compare",
           
    sidebarLayout(
      
      sidebarPanel(
    
    titlePanel("Compare screentime in two groups"),
    fluidRow(
      column(6, br()),
      column(6, 
        checkboxInput("group2_checkbox", label = "Include second group?", value = FALSE),  
      )
    ),
    fluidRow(
      column(6,
         radioButtons(inputId = "compare_gender1", 
                      label = "Gender for group 1:",
                      choices = c(
                        "All" = "All",
                        "Male" = "Female",
                        "Female" = "Male")),
         
         sliderInput(inputId = "compare_age_range1", 
                     label = "Age range for group 1:",
                     min = 0,
                     max = 100,
                     value = c(0,100))
      ),
      
      column(6,
         radioButtons(inputId = "compare_gender2", 
                      label = "Gender for group 2:",
                      choices = c(
                        "All" = "All",
                        "Male" = "Female",
                        "Female" = "Male")),
         
         sliderInput(inputId = "compare_age_range2", 
                     label = "Age range for group 2:",
                     min = 0,
                     max = 100,
                     value = c(0,100))
      ),


      
    ),
    fluidRow(
      column(12,
             sliderInput("bin_n",
                         "Number of histrogram intervals:",
                         value = 5,
                         min = 5,
                         max = 200))
    )
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Histogram", 
               plotly::plotlyOutput("compare_hist")
               ),
      tabPanel("Violin Plot", 
               plotOutput("compare_violin")
               ),
      tabPanel("Compare Means",
               plotOutput("compare_density"),
               htmlOutput("t_test")
      )
      )
  ))
  ),
  
  ## SHOWING REGRESSION LINE OF AGE WITH SCREENTIME ##
  tabPanel("Trend",
    sidebarLayout(
      sidebarPanel(
        titlePanel("Relationship between age and screentime"),
        
        # Checkbox option to determine whether to disaggregate trend for gender
        checkboxInput(inputId = "sep_gender_trend", 
                     label = "Fit separate trendline for each gender",
                     value = FALSE),
        
        # Radio button to choose type of line to fit
        radioButtons(inputId = "regression_type", 
                      label = "Type of trendline to fit",
                      choices = c(
                        "Linear" = "linear",
                        "Quadractic" = "quadratic",
                        "LOESS" = "loess")),
        
        # Checkbox option to include confidence interval
        checkboxInput(inputId = "conf_interval", 
                      label = "Show confidence inteval for trend",
                      value = FALSE)
        
        
      ),
      mainPanel(
        plotOutput("regression_line")
      )
    )
  ),
  tabPanel("Most Used App",
    sidebarLayout(
     sidebarPanel(
       
       titlePanel("Most used apps"),
       
       checkboxInput(inputId = "sep_gender_trend", 
                     label = "Show Male/Female split",
                     value = FALSE),     
     ),
     mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("Overview", 
                            plotOutput("app_summary")
                   ),
                   tabPanel("By Age", 
                            plotOutput("app_by_age")
                   )
       )
     )) # end of main panel and page for "MOST USED APP"
  ),
  tabPanel("About",
    
  )
)


