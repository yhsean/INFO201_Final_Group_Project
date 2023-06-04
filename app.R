library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)
library(tidyverse)
library(plotly)
library(gridExtra)

#MAKE SURE TO LOAD THE data cleaning CSV!!!"
#function for contrast section
category_function <- function(state_name, dataset) {
  state_data <- walk_df %>%
    filter(state == state_name)  
  
  if (nrow(state_data) == 0) {
    message("State not found.")
    return(NULL)
  } else {
    obesity <- state_data$obesity
    walkability_category <- state_data$walkability_category
    message(paste("This state has obesity of", obesity, "and is", walkability_category))
    return(list(obesity = obesity, walkability_category = walkability_category))
  }
}
#barplot for contrast section
walk_df <- read.csv("INFO201_group_project_data_cleaning.csv")
summary_df <- walk_df %>%
  group_by(walkability_category) %>%
  summarize(mean_obesity = mean(obesity))
barplot <- ggplot(summary_df, aes(x = walkability_category, y = mean_obesity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Walkability Categories", y = "Obesity", title = "Obesity by Walkability Categories") 

#functions for factions section
col <- colnames(walk_df)
col2 <- col[3:6]

#Intersection
low_df <- filter(walk_df, str_detect(walkability_category,  "below average walkable"))
high_df <- filter(walk_df, str_detect(walkability_category,  "above average walkable"))

input_selection <- function(df, input) {
  if (input == "Intersection Density") {
    return(df$intersection_density)
  } else if(input == "Transit Stops") {
    return(df$transit_stops)
  } else if(input == "Employment Mix") {
    return(df$employment_mix)
  } else if(input == "Employment and Household") {
    return(df$employment_and_household)
  }
}

equation_trendline <- function(df, input) {
  df <- data.frame(x = input, y = df$obesity)
  graph <- lm(y ~ x, data = df)
  return(paste('y =', round(coef(graph)[[2]], digits = 3), '* x', '+', round(coef(graph)[[1]], digits = 3)))
}

intersection_value <- function(df1, df2, input1, input2) {
  df1 <- data.frame(x = input1, y = df1$obesity)
  df2 <- data.frame(x = input2, y = df2$obesity)
  graph1 <- lm(y ~ x, data = df1)
  graph2 <- lm(y ~ x, data = df2)
  
  coefMatrix <- matrix(c(-coef(graph1)[[2]], 1, -coef(graph2)[[2]], 1), nrow = 2, ncol = 2, byrow = TRUE)
  rhsMatrix <- matrix(c(coef(graph1)[[1]], coef(graph2)[[1]]), nrow = 2, ncol = 1, byrow = TRUE)
  inverse <- solve(coefMatrix)
  result <- inverse %*% rhsMatrix
  return(result)
}



ui <- fluidPage(
  theme = shinytheme("flatly"),  # Set the theme to "flatly"
  
#background color
  tags$head(tags$style(HTML("body {background-color: #F0F8FF;}"))),
  
#tabs
  navbarPage(
    "Walkability and Obesity",
    id = "main-navbar",

    #introductory tab
    tabPanel(
      "Introduction",
      fluidRow(
        column(12,
               h1("Walkability and Obesity", align = "center"),
               h3("Ethan Barnes, Sean Yoon, Jisoo Ku", align = "center"),
               br(),
               p("Obesity is one of the our countries biggest issues, especially when looking at our obesity rates in comparison to other countries. When looking at our city design, it is eas to notice how many cars are out and about. The average American spends an hour a day in their car. Time that could easily be spent in better places. European cities are built around their transit systems, which cuts down on pollution, and also gets peoples to walk much more every day. We want to look at the impact that examples of such city planning in the US has on obesity rates.", align = "center"),
               br(),
               h4("Background Information", align = "center"),
               p("Walkability index formula is given by EPA. (w/3) + (x/3) + (y/6) + (z/6) = Final National Walkability Index Score. 
                Variables w,x,y, and z are all included in the dataset provided by the EPA, which are ranks given to their intersection density, transit stops, employment mix, employment and household mix (respectively).
                 Then given the walkability index score, we can use EPA's categories for their walkability index.
                 1-5.75: Least Walkable 
                 5.76 - 10.5: Below Average Walkable
                 10.51 - 15.25: Above Average Walkable
                 15.26 - 20: Most Walkable", align = "center")
        )
      )
    ),
    # Contrast tab
    tabPanel(
      "Contrast",
      fluidRow(
        column(12,
               h1("Contrast Data Storytelling", align = "center"),
               h3("Jisoo Ku", align = "center"),
               p("Using the contrast method to deliver the story derived from our data, we will be contrasting two different conditions: state’s obesity based on their walkability index (below average walkable and above average walkable). Using the two datasets with obesity and walkability index, we were able to find the relationship between obesity and walkability index. Using EPA’s National Walkability Index Guide, we were able to categorize states’ walkability category based on their index scores.", align = "justify"),
               p("We were able to find out that states with higher walkability (classified as above average walkable) had lower obesity than those with lower walkability (classified as below average walkable).", align = "justify"),
               br(),
               h4("Bar Chart"),
               p("The bar chart at the bottom of the page was created from R Studio based on the dataset that we have created following the data joining and cleaning part of the project.", align = "justify"),
               p("Here are the variables observed in the bar chart:", align = "justify"),
               p("Dependent Variable: Walkability categories", align = "justify"),
               p("Independent variable: mean obesity of all states categorized accordingly to the dependent variable", align = "justify"),
               p("The bar chart clearly suggests that there is a correlation between the two conditions with the mean obesity. The higher the walkability index, the lower the obesity and vice versa. Therefore, we can conclude that areas that are more walkable can reduce obesity and low walkability plays a factor in mean obesity.", align = "justify"),
        ),
            column(6,
                   textInput("state_input", "Enter State Abbreviation"),
                   actionButton("show_category", "Show Category Function"),
                   plotOutput("barplot")
            ),
            column(6,
                   h4("Category Result:"),
                   verbatimTextOutput("category_result")
            )
          )
        ),

    
    # Factions tab
    tabPanel("Factions",
             h1("Storytelling with Factions", align = "center"),
             h3("Ethan Barnes", align = "center"),
             h4("Background", align = "left"),
             p("Using the Faction method of data storytelling, we want to show which one of the four factors(Intersection Density, Distance to transit stops, employment mix, and household and employment mix) of the walkability index has a greater correlation with state obesity rates than the others. Then, by zooming out, we will calculate the correlation between obesity rates and the walkability index by state.", align = "justify"),
             h4("Findings", align = "left"),
             p("The faction method helped us find that the average distance to the nearest transit stop had the greatest impact on the obesity rates by state in the US. There was a correlation of -.63 between the two variables, far greater than the correlations between each of the other three variables and the obesity rates. The correlation between the walkability index and the obesity rates was greater than each of the 4 factors that go into the walkability index itself.", align = "justify"),
             h4("Charts", align = "left"),
             p("Each chart contains fifty data points, one for each state, and has a linear trendline. ", align = "justify"),
             br(),
              mainPanel(
                tabsetPanel(
                  tabPanel("Four Factors",
                      selectInput(
                        inputId = "factor_select",
                        label = "Select a Factor",
                        choices = col2),
                      plotlyOutput(outputId = "factor_charts"),
                      textOutput("correlation_text")),
                  tabPanel("Walkability Index", 
                      plotlyOutput(outputId = "comp_chart"),
                      textOutput("correlation_text2"))
                    )
                  )
             ),
    
    # Intersections tab
    tabPanel(
      "Intersection",
      h1("Intersection Data Storytelling", align = "center"),
      h3("Sean Yoon", align = "center"),
      p("Using scatter plots that linearize the relationship between one of four normalized factors(intersection density, transit stops, employment, and household) of walkability index and the obesity rate with trendlines for each group of low walkable states(below average) and high walkable states(above average), we will find a converging point by overlapping the overall trend plot with a plot of either group and analyze significant values from the intersections to conclude the priority among the four factors to be considered and developed in each group.", align = "justify"),
      br(),
      h3("Data Visualization"),
      sidebarLayout(
        sidebarPanel(
          selectInput (
            inputId = "selection_intersection",
            label = "Select a factor",
            choice = c("Intersection Density", "Transit Stops", "Employment Mix", "Employment and Household")
          ),
          h4("Summary"),
          p("Low Walkability Group:"),
          textOutput(outputId = "equation_low"),
          br(), 
          p("High Walkability Group:"),
          textOutput(outputId = "equation_high"),
          br(), 
          p("Overall Walkability trend:"),
          textOutput(outputId = "equation_overall")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Low Walkability",
              plotOutput(outputId = "low_intersection")
            ),
            tabPanel(
              "High Walkability",
              plotOutput(outputId = "high_intersection")
            ),
            tabPanel(
              "Intersection",
              plotOutput(outputId = "combined_intersection")
            )
          )
        )
      ),
      h4("Intersection Points"),
      selectInput (
        inputId = "factor_point",
        label = "Choose a factor to see the analysis in detail",
        choice = c("Intersection Density", "Transit Stops", "Employment Mix", "Employment and Household")
      ),
      textOutput(outputId = "intersection_points"),
      h3("Analysis"),
      p(
        "The equation with negative slope represents the inverse proportionality between the obesity rate and the factor; in other words, the larger the index of factor is, the lower obesity rate there will be.",
        "Furthermore, the overall walkability trendline represents the general ratio of obesity rate over each factor of walkability index all around the US.",
        "In this regard, the values of factors in each group intersected with the overall trendline reflect how great the factors are developed and have affected the walkability of the states as following the global trend.",
        "Then, we are expected to draw the most significant factor to consider and care more, from one of the factors with the lowest value.",
        "The values of each factor at the intersection with the global trend are,"
      ),
      fluidRow(
        column(
          6,
          p("for the low walkability group,"),
          tableOutput(outputId = "table_low_intersection"),
        ),
        column(
          6,
          p("for the high walkability group,"),
          tableOutput(outputId = "table_high_intersection"),
        )
      ),
      p(
        "Therefore, we could assume that the low walkable states should develop the factor of EMPLOYMENT MIX,",
        "and the high walkable states should develop the factor of TRANSIT STOPS, in order for better walkability and further decrease in obesity rate."
      ),
      fluidRow(
        column(
          6,
          h4("Low Walkability Group"),
          textOutput(outputId = "group_low_walkability")
        ),
        column(
          6,
          h4("High Walkability Group"),
          textOutput(outputId = "group_high_walkability")
        )
      )
    ),
    tabPanel(
      "Summary",
      h3("Summary"),
      h3("Key Findings", align = "left"),
      p("We found a moderate to strong negative correlation between state obesity rates and the state walkability index. Meaning the higher the walkability index, the lower the obesity rate. We also noticed that states that were in the category of above average walkability had a much lower average obesity rate than the states that were of below average walkability.", align = "justify"),
      p("Furthermore, we found that being close to transit stops had the greatest positive impact on the state obesity rates. More so that each of the other three factors.", align = "justify"),
      h3("Our Datasets", align = "left"),
      p("We found our datasets on the US governments catalog of datasets. Our final dataset was a joined and cleaned version of the walkability index and a study done by Illinois that gave the obesity rates for each state. The walkability index covers every census block in the US and contains over 200,000 records. We were able to group this data by state to get it down to a meager 50 records, one for each state.", align = "justify")
         )
      
    )
  )


# Define server
server <- function(input, output) {
  
  # category_function for contrast
  observeEvent(input$show_category, {
    state_name <- input$state_input
    result <- category_function(state_name = state_name, dataset = walk_df)
    if (!is.null(result)) {
      output$category_result <- renderPrint({
        cat("This state has obesity of", result$obesity, "and is", result$walkability_category)
      })
    }
  })
  
  # barchart for contrast
  output$barplot <- renderPlot({
    barplot
  })
  
  output$factor_charts <- renderPlotly({
    pick_factor <- select(walk_df, state, obesity, input$factor_select)
    p <- ggplot(pick_factor, aes_string(x = "obesity", y = input$factor_select)) +
      geom_point(aes(col=state, text=state)) +
      geom_smooth(method = "lm", se = FALSE, col = "turquoise")
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$correlation_text <- renderText({
    pick_factor <- select(walk_df, state, obesity, input$factor_select)
    correlation <- cor(pick_factor$obesity, pick_factor[[input$factor_select]])
    paste("Correlation:", round(correlation, 2))
  })
  
  output$comp_chart <- renderPlotly({
    pl <- ggplot(walk_df, aes(x = obesity, y = walkability_index)) +
      geom_point(aes(col=state, text=state)) +
      geom_smooth(method = "lm", se = FALSE, col = "turquoise")
    pl <- ggplotly(pl, tooltips = "text")
    return(pl)
  })  
  
  output$correlation_text2 <- renderText({
    correlation2 <- cor(walk_df$obesity, walk_df$walkability_index)
    paste("Correlation:", round(correlation2, 2))
  })
  
  output$equation_low <- renderText({
    equation <- equation_trendline(low_df, input_selection(low_df, input$selection_intersection))
    return(equation)
  })
  output$equation_high <- renderText({
    equation <- equation_trendline(high_df, input_selection(high_df, input$selection_intersection))
    return(equation)
  })
  output$equation_overall <- renderText({
    equation <- equation_trendline(walk_df, input_selection(walk_df, input$selection_intersection))
    return(equation)
  })
  
  output$low_intersection <- renderPlot({
    low_plot <- ggplot(low_df, aes(x = input_selection(low_df, input$selection_intersection), y = obesity)) +
      geom_point() +
      ggtitle(paste("Obesity Rate versus", input$selection_intersection, "Graph for Low Walkability Group")) +
      geom_smooth(method = "lm", se = FALSE, color = "#00BFC4", size = 2) +
      labs(x = input$selection_intersection, y = "Obesity Rate")
    return(plot(low_plot))
  })
  
  output$high_intersection <- renderPlot({
    high_plot <- ggplot(high_df, aes(x = input_selection(high_df, input$selection_intersection), y = obesity)) +
      geom_point() +
      geom_smooth(method = "lm", color = "#F8766D", size = 2, se = FALSE) +
      ggtitle(paste("Obesity Rate versus", input$selection_intersection, "Graph for High Walkability Group")) +
      labs(x = input$selection_intersection, y = "Obesity Rate")
    return(plot(high_plot))
  })
  
  output$combined_intersection <- renderPlot({
    final_plot <- ggplot(walk_df, aes(x = input_selection(walk_df, input$selection_intersection), y = obesity, color = walkability_category)) +
      geom_point() +
      ggtitle(paste("Obesity Rate versus", input$selection_intersection, "Graph in overall view")) +
      geom_smooth(method = "lm", se = FALSE, size = 1) +
      labs(x = input$selection_intersection, y = "Obesity Rate", color = "Walkability Level") +
      scale_color_manual(labels = c("High Walkability", "Low Walkability"), values = c("#F8766D", "#00BFC4"))
    final_plot <- final_plot + geom_smooth(method = "lm", formula = y ~ x, se= FALSE, color = "purple", size = 1)
    return(plot(final_plot))
  })
  
  output$intersection_points <- renderText({
    converging_point_low <- intersection_value(low_df, walk_df, input_selection(low_df, input$factor_point), input_selection(walk_df, input$factor_point))
    converging_point_high <- intersection_value(high_df, walk_df, input_selection(high_df, input$factor_point), input_selection(walk_df, input$factor_point))
    message <- paste(
      "For the factor of intersection density, We are expected to see that the low and average walkability groups are intersected at the obesity rate of ", round(converging_point_low[2,], digits = 3), " and the intersection density of ", round(converging_point_low[1,], digits = 3),
      " and the high and average walkability groups are intersected at Obesity rate of ", round(converging_point_high[2,], digits = 3), " and the intersection density of ", round(converging_point_high[1,], digits = 3), ".", sep = ""
    )
    return(message)
  })
  
  output$table_low_intersection <- renderTable({
    converging_point_ID_low <- intersection_value(low_df, walk_df, low_df$intersection_density, walk_df$intersection_density)
    converging_point_TS_low <- intersection_value(low_df, walk_df, low_df$transit_stops, walk_df$transit_stops)
    converging_point_EM_low <- intersection_value(low_df, walk_df, low_df$employment_mix, walk_df$employment_mix)
    converging_point_EH_low <- intersection_value(low_df, walk_df, low_df$employment_and_household, walk_df$employment_and_househol)
    
    x <- c(round(converging_point_ID_low[1,], digits = 3), round(converging_point_TS_low[1,], digits = 3),round(converging_point_EM_low[1,], digits = 3), round(converging_point_EH_low[1,], digits = 3))
    y <- c("intersection density", "transit stops", "employment mix", "employment and household")
    df <- data.frame("factor" = y, "intersecting value" = x)
    return(df)
  })
  
  output$table_high_intersection <- renderTable({
    converging_point_ID_high <- intersection_value(high_df, walk_df, high_df$intersection_density, walk_df$intersection_density)
    converging_point_TS_high <- intersection_value(high_df, walk_df, high_df$transit_stops, walk_df$transit_stops)
    converging_point_EM_high <- intersection_value(high_df, walk_df, high_df$employment_mix, walk_df$employment_mix)
    converging_point_EH_high <- intersection_value(high_df, walk_df, high_df$employment_and_household, walk_df$employment_and_househol)
    
    x <- c(round(converging_point_ID_high[1,], digits = 3), round(converging_point_TS_high[1,], digits = 3),round(converging_point_EM_high[1,], digits = 3), round(converging_point_EH_high[1,], digits = 3))
    y <- c("intersection density", "transit stops", "employment mix", "employment and household")
    df <- data.frame("factor" = y, "intersecting value" = x)
    return(df)
  })
  
  output$group_low_walkability <- renderText({
    return(paste(state.name[match(low_df$state,state.abb)], collapse = ",\n"))
  })
  
  output$group_high_walkability <- renderText({
    return(paste(state.name[match(high_df$state,state.abb)], collapse = ",\n"))
  })
}


shinyApp(ui, server)


