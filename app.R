# Behind the Numbers: Exploring Fatal Traffic Crashes in the United States (2024)

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(bsicons)
library(plotly)
library(scales)

# Load the data
fars_data <- readRDS("fars_data.rds")
accidents <- fars_data$accidents
vehicles <- fars_data$vehicles
people <- fars_data$people
distractions <- fars_data$distractions

# Overview numbers
total_crashes <- nrow(accidents)
total_fatalities <- sum(accidents$FATALS, na.rm = TRUE)
total_people <- sum(accidents$PERSONS, na.rm = TRUE)

# Chart colors
blue <- "#17365D"
teal <- "#268F88"
gold <- "#E5B447"

# Function for the bar charts
# Function for the bar charts
make_bar_plot <- function(data, chart_title, y_title = "Number of Records") {
  
  p <- ggplot(data, aes(
    x = reorder(Category, Count),
    y = Count,
    fill = Category,
    text = paste0(Category, "<br>", y_title, ": ", comma(Count))
  )) +
    geom_col(width = 0.7, show.legend = FALSE) +
    coord_flip() +
    scale_fill_viridis_d(option = "turbo") +
    scale_y_continuous(labels = comma) +
    labs(
      title = chart_title,
      x = NULL,
      y = y_title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(hovermode = "closest")
}

# Group less common categories as Other
top_other <- function(data, number = 9) {
  data <- data %>% arrange(desc(Count))
  
  if (nrow(data) > number) {
    other_count <- sum(data$Count[(number + 1):nrow(data)])
    data <- data[1:number, ]
    
    data <- bind_rows(
      data,
      data.frame(Category = "Other", Count = other_count)
    )
  }
  
  data
}

# App appearance
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = blue,
  secondary = teal,
  base_font = font_google("Lato")
)

# USER INTERFACE
ui <- page_navbar(
  title = "Behind the Numbers",
  id = "main_nav",
  theme = app_theme,
  
  header = tags$div(
    style = "padding: 15px 25px 5px 25px;",
    h4("Exploring Fatal Traffic Crashes in the United States"),
    p(
      "An interactive look at data from the 2024 Fatality Analysis Reporting System (FARS).",
      style = "color: #666666;"
    )
  ),
  
  # OVERVIEW
  nav_panel(
    "Overview",
    
    fluidRow(
      column(
        4,
        value_box(
          title = "Fatal Crashes",
          value = comma(total_crashes),
          showcase = bs_icon("car-front"),
          theme = "primary"
        )
      ),
      column(
        4,
        value_box(
          title = "Total Fatalities",
          value = comma(total_fatalities),
          showcase = bs_icon("heart-pulse"),
          theme = "danger"
        )
      ),
      column(
        4,
        value_box(
          title = "People Involved",
          value = comma(total_people),
          showcase = bs_icon("people"),
          theme = "success"
        )
      )
    ),
    
    card(
      card_header("Fatal Crashes Throughout 2024"),
      plotlyOutput("monthlyPlot", height = "400px")
    )
  ),
  
  # PEOPLE INVOLVED
  nav_panel(
    "People Involved",
    
    fluidRow(
      column(
        6,
        card(
          card_header("Age Distribution of People Involved"),
          plotlyOutput("agePlot", height = "350px")
        )
      ),
      column(
        6,
        card(
          card_header("People by Sex"),
          plotlyOutput("sexPlot", height = "350px")
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        card(
          card_header("Person Type"),
          plotlyOutput("personTypePlot", height = "400px")
        )
      ),
      column(
        6,
        card(
          card_header("Restraint Use"),
          plotlyOutput("restraintPlot", height = "400px")
        )
      )
    )
  ),
  
  # CRASH CIRCUMSTANCES
  nav_panel(
    "Crash Circumstances",
    
    card(
      card_header("Explore Crash Circumstances"),
      
      selectInput(
        "circumstance_group",
        "Choose a category:",
        choices = c(
          "Crashes by State" = "STATENAME",
          "Rural vs. Urban" = "RUR_URBNAME",
          "Day of the Week" = "DAY_WEEKNAME",
          "Time of Day" = "HOURNAME",
          "Speed-Related Factors" = "SPEEDRELNAME",
          "Driver Distraction" = "DRDISTRACTNAME"
        )
      ),
      
      plotlyOutput("circumstancePlot", height = "550px")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Fatal crashes by month
  output$monthlyPlot <- renderPlotly({
    
    monthly_data <- accidents %>%
      filter(!is.na(MONTH), !is.na(MONTHNAME)) %>%
      group_by(MONTH, MONTHNAME) %>%
      summarise(Crashes = n(), .groups = "drop") %>%
      arrange(MONTH)
    
    p <- ggplot(monthly_data, aes(
      x = MONTH,
      y = Crashes,
      group = 1,
      text = paste0(
        MONTHNAME,
        "<br>Fatal crashes: ",
        comma(Crashes)
      )
    )) +
      geom_line(color = teal, linewidth = 1.2) +
      geom_point(color = gold, size = 3) +
      scale_x_continuous(
        breaks = monthly_data$MONTH,
        labels = monthly_data$MONTHNAME
      ) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Fatal Crashes by Month",
        x = NULL,
        y = "Number of Fatal Crashes"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
  
  
  # Age distribution
  # Count people in five-year age groups
  output$agePlot <- renderPlotly({
    
    age_data <- people %>%
      filter(!is.na(AGE), AGE >= 0, AGE <= 97) %>%
      mutate(
        AgeGroup = cut(
          AGE,
          breaks = seq(0, 100, by = 5),
          right = FALSE,
          labels = paste(
            seq(0, 95, by = 5),
            seq(4, 99, by = 5),
            sep = "-"
          )
        )
      ) %>%
      count(AgeGroup, name = "People")
    
    p <- ggplot(age_data, aes(
      x = AgeGroup,
      y = People,
      text = paste0(
        "Age range: ", AgeGroup,
        "<br>People: ", comma(People)
      )
    )) +
      geom_col(fill = teal, width = 0.8) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Age Distribution of People Involved",
        x = "Age Range",
        y = "Number of People"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
  
  
  # People by sex
  output$sexPlot <- renderPlotly({
    
    sex_data <- people %>%
      filter(!is.na(SEXNAME), SEXNAME != "") %>%
      count(Category = SEXNAME, name = "People")
    
    plot_ly(
      data = sex_data,
      labels = ~Category,
      values = ~People,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      hovertemplate = paste(
        "%{label}",
        "<br>People: %{value:,}",
        "<extra></extra>"
      ),
      showlegend = FALSE
    ) %>%
      layout(
        title = "People by Sex",
        showlegend = FALSE
      )
  })
  
  
  # Person type
  output$personTypePlot <- renderPlotly({
    
    person_data <- people %>%
      filter(!is.na(PER_TYPNAME), PER_TYPNAME != "") %>%
      count(Category = PER_TYPNAME, name = "Count") %>%
      arrange(desc(Count))
    
    make_bar_plot(
      person_data,
      "People by Person Type",
      "Number of People"
    )
  })
  
  
  # Restraint use
  output$restraintPlot <- renderPlotly({
    
    restraint_data <- people %>%
      filter(!is.na(REST_USENAME), REST_USENAME != "") %>%
      count(Category = REST_USENAME, name = "Count") %>%
      arrange(desc(Count))
    
    make_bar_plot(
      restraint_data,
      "Reported Restraint Use",
      "Number of People"
    )
  })
  
  
  # Data for the selected crash circumstance
  circumstance_data <- reactive({
    
    selected_column <- input$circumstance_group
    
    if (selected_column == "DRDISTRACTNAME") {
      
      data <- distractions %>%
        filter(!is.na(DRDISTRACTNAME), DRDISTRACTNAME != "") %>%
        count(Category = DRDISTRACTNAME, name = "Count")
      
    } else if (selected_column == "SPEEDRELNAME") {
      
      data <- vehicles %>%
        filter(!is.na(SPEEDRELNAME), SPEEDRELNAME != "") %>%
        count(Category = SPEEDRELNAME, name = "Count")
      
    } else {
      
      data <- accidents %>%
        filter(
          !is.na(.data[[selected_column]]),
          .data[[selected_column]] != ""
        ) %>%
        count(
          Category = .data[[selected_column]],
          name = "Count"
        )
    }
    
    data %>% arrange(desc(Count))
  })
  
  
  # Map or bar chart for crash circumstances
  output$circumstancePlot <- renderPlotly({
    
    data <- circumstance_data()
    
    # Show a map when state is selected
    if (input$circumstance_group == "STATENAME") {
      
      state_map <- data %>%
        mutate(
          Abbrev = state.abb[
            match(tolower(Category), tolower(state.name))
          ]
        )
      
      # Add abbreviation for Washington, DC
      state_map$Abbrev[
        tolower(state_map$Category) == "district of columbia"
      ] <- "DC"
      
      state_map <- state_map %>%
        filter(!is.na(Abbrev))
      
      plot_ly(
        data = state_map,
        locations = ~Abbrev,
        z = ~Count,
        type = "choropleth",
        locationmode = "USA-states",
        colorscale = list(
          list(0, "#dbeafe"),
          list(1, "#17365D")
        ),
        showscale = FALSE,
        text = ~paste0(
          Category,
          "<br>Fatal crashes: ",
          comma(Count)
        ),
        hovertemplate = "%{text}<extra></extra>",
        marker = list(
          line = list(color = "white", width = 1)
        )
      ) %>%
        layout(
          title = "Fatal Crashes by State",
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa")
          )
        )
      
    } else {
      
      if (input$circumstance_group == "DRDISTRACTNAME") {
        data <- top_other(data, 9)
      }
      
      make_bar_plot(
        data,
        "Reported Crash Circumstances"
      )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)