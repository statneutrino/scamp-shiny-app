library(tidyverse)
library(googlesheets4)
library(markdown)

# Authorize access to Google spreadsheet
gs4_auth(cache = ".secrets", email = "[YOUR EMAIL]")
    
ui <- navbarPage("SCAMP Screentime Data Explorer", theme = shinythemes::shinytheme("cyborg"),
                     
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
                                         "Number of histogram intervals:",
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
                                       plotly::plotlyOutput("compare_violin")
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
                  
                  checkboxInput(inputId = "male_female_split", 
                                label = "Show Male/Female split",
                                value = FALSE),     
              ),
              mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Overview", 
                                       plotly::plotlyOutput("app_summary")
                              ),
                              tabPanel("By Age", 
                                       plotOutput("app_by_age")
                              )
                  )
              )) # end of main panel and page for "MOST USED APP"
    ),
    tabPanel("About",
             fluidRow(
               column(6,
                      includeMarkdown("about.md")
               ),
               column(3,
                      img(class="img-polaroid",
                          src="scamp.jpg",height="80%", width="80%", align="right")
                )
              )
    )
)

server <- function(input, output) {
    
    # LOAD DATA
    age_options <- c("12 or under", "13-17", "18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+")
    app_options <- c("Facebook", "Youtube", "Whatsapp", "Instgram", "WeChat", "Tiktok", "Snapchat", "Twitter", "Linkedin", "Other")
    
    gform_df <- read_sheet("https://docs.google.com/spreadsheets/d/1tquDI6xRWkqs5UcUYj9QatYaiEmlhT9i0XNxv-YQQGM")
    df <- gform_df[,c(2:6)] %>%
        rename(!!!setNames(names(.), c("sex", "age", "hours", "minutes", "social_media"))) %>%
        mutate(screentime = round((hours * 60 + minutes)/60,2)) %>%
        mutate(age_category = case_when(
            age <= 12 ~ "12 or under",
            age <= 17 ~ "13-17",
            age <= 25 ~ "18-25",
            age <= 35 ~ "26-35",
            age <= 45 ~ "36-45",
            age <= 55 ~ "46-55",
            age <= 65 ~ "56-65",
            age <= 75 ~ "66-75",
            age > 75 ~ "76+",
            TRUE ~ NA_character_
        )) %>%
        mutate(age_category = factor(age_category, levels=age_options)) %>%
        filter(sex %in% c("Male", "Female"))

    
    # Reactive expression for histograms and mean comparisons
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
    
    bin_n <- reactive({input$bin_n})
    
    # Reactive expression for regression line trend options
    trend_options <- reactive({
        gender_stratified <- input$sep_gender_trend
        regression_type <- input$regression_type
        
        smooth_options <- switch(regression_type,
                                 "linear" = c("lm", "y ~ x"),
                                 "quadratic" = c("lm", "y ~ x + I(x^2)"),
                                 "loess" = c("loess", "y ~ x"))
        
        list(
            gender_stratified = gender_stratified,
            smooth_options = smooth_options,
            shaded_se = input$conf_interval
        )
    })
    

    # Create histrogram
    output$compare_hist <- plotly::renderPlotly({
        plotly::ggplotly(ggplot(compare_reactive(), aes(x=screentime, fill=group)) + 
                             geom_histogram(alpha=0.9, position="identity", bins = bin_n()) + 
                             ggdark::dark_theme_gray() + xlab("Screentime / hrs") + ylab("Count") + labs(fill = "Group") +
                             scale_fill_brewer(palette = "Pastel2"))
    })
    
    # Create violin plot
    output$compare_violin <- plotly::renderPlotly({
        set.seed(100); plotly::ggplotly(ggplot(compare_reactive(), aes(x=group, y=screentime, fill=group)) + 
                                            geom_violin(alpha=0.6) + theme(legend.position="none") + 
                                            geom_point(shape = 21, size=1.5, position = position_jitterdodge(), color="black", alpha=1) +
                                            ggdark::dark_theme_gray() + labs(fill = "Group") +
                                            xlab("Group") + ylab("Screentime / hrs") + 
                                            scale_fill_brewer(palette = "Pastel2") + 
                                            stat_summary(fun = "mean",
                                                         geom = "point",
                                                         color = "red", show.legend=FALSE))
    })
    # Create density plot
    output$compare_density <- renderPlot({
        
        mean_lines <- compare_reactive() %>%
            group_by(group) %>%
            summarize(mean = mean(screentime))
        
        ggplot(compare_reactive() , aes(x=screentime, fill=group)) + 
            geom_density(alpha=.7) + 
            ggdark::dark_theme_gray() + labs(fill="Group", color="Group") +
            scale_fill_brewer(palette = "Pastel2") + 
            scale_color_brewer(palette = "Pastel2") + 
            xlab("Screentime / hrs") + ylab("Density") + 
            geom_vline(data = mean_lines, aes(xintercept = mean, color = group), size=3, linetype="dashed")
    })
    
    output$t_test <- renderUI({
        
        if("Group 2" %in% unique(compare_reactive()$group)){
            HTML(paste(paste("Mean screentime in Group 1 is: ", compare_reactive() %>% 
                                 filter(group == "Group 1") %>%
                                 select(screentime) %>%
                                 pull %>%
                                 mean %>%
                                 round(., 2), "hours"), 
                       paste("Mean screentime in Group 2 is: ", compare_reactive() %>%
                                 filter(group == "Group 2") %>%
                                 select(screentime) %>%
                                 pull %>%
                                 mean %>%
                                 round(., 2), "hours"), 
                       paste("Observed difference in means is:", ((compare_reactive() %>% 
                                                                       filter(group == "Group 2") %>%
                                                                       select(screentime) %>%
                                                                       pull %>%
                                                                       mean ) - (compare_reactive() %>% 
                                                                                     filter(group == "Group 1") %>%
                                                                                     select(screentime) %>%
                                                                                     pull %>%
                                                                                     mean ) ) %>% round(., 2), "hours" ),
                       paste0("95% Confidence interval for difference in means is: (",
                              -t.test(
                                  compare_reactive()$screentime[compare_reactive()$group == "Group 1"], 
                                  compare_reactive()$screentime[compare_reactive()$group == "Group 2"]
                              )[4]$conf.int[2] %>% round(., 2), 
                              ", ",
                              -t.test(
                                  compare_reactive()$screentime[compare_reactive()$group == "Group 1"], 
                                  compare_reactive()$screentime[compare_reactive()$group == "Group 2"]
                              )[4]$conf.int[1]%>% round(., 2),
                              ")", " hours"),
                       sep="<br/>"))
        } else {
            HTML(paste("Mean screentime in Group 1 is: ", compare_reactive() %>% 
                           select(screentime) %>%
                           pull %>%
                           mean %>%
                           round(., 2), "hours"))
        }
        
    })
    
    output$regression_line <- renderPlot({
        if(!trend_options()$gender_stratified){
            # Plot gender aggregated
            set.seed(100); ggplot(df, aes(x=age, y=screentime)) + 
                geom_jitter(col="pink", size=1.5, width=0.3, height = 0) + 
                ggdark::dark_theme_gray() + 
                xlab("Age / years") + ylab("Screentime / hrs") +
                geom_smooth(method=trend_options()$smooth_options[1], formula = trend_options()$smooth_options[2],
                            se=trend_options()$shaded_se, alpha=0.5, color="pink", fill="pink") + 
                theme(text = element_text(size = 14))
        } else {
            # Plot gender stratified
            
            # Find the minimum age for females OR males
            min_st <- max(c(min(df$age[df$sex == "Female"]), min(df$age[df$sex == "Male"])))
            
            max_st <- min(c(max(df$age[df$sex == "Female"]), max(df$age[df$sex == "Male"]))) 
            
            set.seed(100); ggplot(df %>% filter(sex %in% c("Male", "Female")), aes(x=age, y=screentime, fill=sex, color=sex)) + 
                geom_jitter(size=1.5, width=0.3, height = 0) + 
                ggdark::dark_theme_gray() + 
                geom_smooth(method=trend_options()$smooth_options[1], formula = trend_options()$smooth_options[2],
                            se=trend_options()$shaded_se, alpha=0.5, fullrange = TRUE) + 
                theme(text = element_text(size = 14)) +
                xlab("Age / years") + ylab("Screentime / hrs") +
                scale_fill_brewer(palette = "Pastel2") + 
                scale_color_brewer(palette = "Pastel2")# + 
            #coord_cartesian(xlim = c(min_st, max_st))
        }
        
        
    })
    
    app_bar_options <- reactive({
        male_female_split <- input$male_female_split
        
        list(
            male_female_split = male_female_split
        )
    })
    
    output$app_summary <- plotly::renderPlotly({
      sex_agg_p <- ggplot(df %>% mutate(app = fct_relevel(fct_infreq(social_media), "Other", after = Inf)), 
                          aes(x=app)) + 
        geom_bar(fill="pink") + 
        xlab("Most Used App") + 
        ggdark::dark_theme_gray()
      
      sex_disagg_p <- ggplot(df %>% 
                               mutate(app = fct_relevel(fct_infreq(social_media), "Other", after = Inf)), 
                             aes(x=app)) + 
        geom_bar(aes(fill=sex), position="dodge") + 
        xlab("Most Used App") + 
        ggdark::dark_theme_gray()
      
      if(!app_bar_options()$male_female_split){
        plotly::ggplotly(sex_agg_p)
      } else{
        plotly::ggplotly(sex_disagg_p)
      }
    })
    
    
    output$app_by_age <- renderPlot({
        sex_agg_p <- ggplot(df %>% mutate(app = fct_relevel(fct_infreq(social_media), "Other", after = Inf)), 
                            aes(x=app)) + 
            geom_bar(aes(fill=age_category), position="stack") + 
            xlab("Most Used App") + labs(fill="Age") +
            ggdark::dark_theme_gray() + 
            facet_wrap( ~ sex) +
            scale_fill_brewer(palette = "YlOrRd")
        
        
        sex_disagg_p <- ggplot(df %>% 
                                 mutate(app = fct_relevel(fct_infreq(social_media), "Other", after = Inf)),
                               aes(x=app)) + 
            geom_bar(aes(fill=age_category), position="stack") + 
            xlab("Most Used App") + labs(fill="Age") +
            ggdark::dark_theme_gray() + 
            scale_fill_brewer(palette = "YlOrRd")
        
        if(app_bar_options()$male_female_split){
            sex_agg_p
        } else{
            sex_disagg_p
        }
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
