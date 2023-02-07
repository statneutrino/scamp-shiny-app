library(tidyverse)

# Define server logic for random distribution app ----
function(input, output) {
    
    # LOAD DATA
    
    # Simulate for now
    N <- 500
    age_options <- c("12 or under", "13-17", "18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+")
    app_options <- c("Facebook", "Youtube", "Whatsapp", "Instgram", "WeChat", "Tiktok", "Snapchat", "Twitter", "Linkedin", "Other")
    
    set.seed(100); df <- data.frame(
        sex = factor(ifelse(rbinom(N, 1, 0.5) == 0, "Female", "Male")),
        age = rpois(N, 40) + rnorm(N, sd=6) %>% round(),
        most_used = sample(app_options, N, replace = TRUE, prob = c(0.4, 0.3, rep(0.3/8, 8)))
    )
    df$screentime <- rnorm(N, 8 * df$age + 60 + (as.numeric(df$sex)-1)*50, 105) %>% round(.)
    df <- df %>%
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
        mutate(screentime = round(screentime/60,2))
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    
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
    
    bin_n <- reactive({input$bin_n})
    
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
    
    
    # Get information
    
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    
    # Generate a summary of the data ----
    output$compare_hist <- plotly::renderPlotly({
        plotly::ggplotly(ggplot(compare_reactive(), aes(x=screentime, fill=group)) + 
                             geom_histogram(alpha=0.9, position="identity", bins = bin_n()) + 
                             ggdark::dark_theme_gray() + xlab("Screentime / hrs") + ylab("Count") + labs(fill = "Group") +
                             scale_fill_brewer(palette = "Pastel2"))
    })
    
    # Generate an HTML table view of the data ----
    output$compare_violin <- plotly::renderPlotly({
        set.seed(100); plotly::ggplotly(ggplot(compare_reactive(), aes(x=group, y=screentime, fill=group)) + 
                                            geom_violin(alpha=0.6) + theme(legend.position="none") + 
                                            geom_point(shape = 21, size=1.5, position = position_jitterdodge(jitter.width=0.2), color="black", alpha=1) +
                                            ggdark::dark_theme_gray() + labs(fill = "Group") +
                                            xlab("Group") + ylab("Screentime / hrs") + 
                                            scale_fill_brewer(palette = "Pastel2") + 
                                            stat_summary(fun = "mean",
                                                         geom = "point",
                                                         color = "red", show.legend=FALSE))
    })
    
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
        sex_agg_p <- ggplot(df %>% mutate(app = fct_relevel(fct_infreq(most_used), "Other", after = Inf)), 
                            aes(x=app)) + 
            geom_bar(fill="pink") + 
            xlab("Most Used App") + 
            ggdark::dark_theme_gray()
        
        sex_disagg_p <- ggplot(df %>% mutate(app = fct_relevel(fct_infreq(most_used), "Other", after = Inf)), 
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
        sex_agg_p <- ggplot(df %>% mutate(app = fct_relevel(fct_infreq(most_used), "Other", after = Inf)), 
                            aes(x=app)) + 
            geom_bar(aes(fill=age_category), position="stack") + 
            xlab("Most Used App") + labs(fill="Age") +
            ggdark::dark_theme_gray() + 
            facet_wrap( ~ sex) +
            scale_fill_brewer(palette = "YlOrRd")
        
        
        sex_disagg_p <- ggplot(df %>% mutate(app = fct_relevel(fct_infreq(most_used), "Other", after = Inf)), 
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