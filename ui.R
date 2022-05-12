library(markdown)

navbarPage("Navbar!",
           
  tabPanel("Histogram",
     
     # Sidebar layout with input and output definitions ----
     sidebarLayout(
       
       # Sidebar panel for inputs ----
       sidebarPanel(
         
         verbatimTextOutput("Choose demographics to include in histogram"),
         
         # Input: SEX
         radioButtons(inputId = "button_gender", 
                      label = "Gender:",
                      choices = c(
                        "All" = "All",
                        "Male" = "Female",
                        "Female" = "Male")),
         
         # Input: AGE CATAEGORY
         
         sliderInput(inputId = "age_slider", 
                     label = "Age range:",
                     min = 0,
                     max = 100,
                     value = c(0,100)
         ),
         
         # Input: Slider for the number of bins in histogram
         sliderInput("n",
                     "Histogram bin size:",
                     value = 5,
                     min = 5,
                     max = 200)
         
         ),
         
         # Main panel for displaying outputs ----
         mainPanel(
           
           tabPanel("Plot", plotOutput("plot"))
           
         )
  )),
  tabPanel("Compare",
           
    sidebarLayout(
      
      sidebarPanel(
    
    titlePanel("Compare distribution and mean of two groups"),
    
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
      )
    )
  ),
  mainPanel())
  ),
  tabPanel("Trend",

  ),
  tabPanel("About",
  )
)


