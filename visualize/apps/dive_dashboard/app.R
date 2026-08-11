library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)  
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(viridis)

# Read data
dives <- read.csv("data/all_uvs_sites.csv", stringsAsFactors = FALSE) |> 
  filter(!exp_id %in% c("PNG_2024", "PLW_2024", "TRI_2017"))

# Data preparation
# Ocean basin classification by expedition
ocean_basin_lookup <- data.frame(
  exp_id = c(
    # INDIAN OCEAN (4 expeditions, 154 dives)
    "SYC_2012", "SYC_2015", "MOZ_2014", "MDV_2023",
    
    # PACIFIC OCEAN (17 expeditions, ~678 dives)
    "CRI_2009", "CHL_2011", "PCN_2012", "CHL_2013", "NCL_2013", "PLW_2014",
    "PYF_2014", "FRA_2016", "MEX_2016", "NIU_2016", "CHL_2017b", "COL_2018",
    "CRI_2019", "NIU_2023", "RMI_2023", "PLW_2024", "PNG_2024", "SLB_2024",
    
    # ATLANTIC OCEAN (4 expeditions, ~133 dives)
    "GAB_2012", "PRT_2015", "TRI_2017", "PRT_2018", "CAN_2022",
    
    # CARIBBEAN (2 expeditions, ~62 dives)
    "DMA_2022", "COL_2022",
    
    # ARCTIC OCEAN (1 expedition, 28 dives)
    "RUS_2013",
    
    # SOUTHERN OCEAN (4 expeditions, 75 dives)
    "CHL_2017a", "ARG_2018a", "ANT_2019", "CHL_2020"
  ),
  ocean = c(
    # Indian
    "Indian", "Indian", "Indian", "Indian",
    # Pacific
    "Pacific", "Pacific", "Pacific", "Pacific", "Pacific", "Pacific",
    "Pacific", "Pacific", "Pacific", "Pacific", "Pacific", "Pacific",
    "Pacific", "Pacific", "Pacific", "Pacific", "Pacific", "Pacific",
    # Atlantic
    "Atlantic", "Atlantic", "Atlantic", "Atlantic", "Atlantic",
    # Caribbean
    "Caribbean", "Caribbean",
    # Arctic
    "Arctic",
    # Southern
    "Southern", "Southern", "Southern", "Southern"
  ),
  stringsAsFactors = FALSE
)

# Climate zone classification by expedition (more accurate than lat/lon formulas)
climate_zone_lookup <- data.frame(
  exp_id = c(
    # TROPICAL
    "SYC_2012", "CRI_2009", "GAB_2012", "PLW_2014", "SYC_2015", "MOZ_2014",
    "FRA_2016", "MEX_2016", "NIU_2016", "COL_2018", "CRI_2019", "DMA_2022",
    "COL_2022", "MDV_2023", "NIU_2023", "RMI_2023", "PLW_2024", "PNG_2024",
    "SLB_2024",
    
    # TEMPERATE
    "CHL_2011", "PCN_2012", "CHL_2013", "NCL_2013", "PYF_2014", "PRT_2015",
    "TRI_2017", "CHL_2017b", "PRT_2018",
    
    # POLAR (includes Antarctic Peninsula)
    "RUS_2013", "CHL_2017a", "ARG_2018a", "ANT_2019", "CHL_2020", "CAN_2022"
  ),
  climate_zone = c(
    # Tropical (abs(lat) < 23.5°)
    rep("Tropical", 19),
    # Temperate (23.5° to ~45-50°)
    rep("Temperate", 9),
    # Polar (high latitudes + Antarctica)
    rep("Polar", 6)
  ),
  stringsAsFactors = FALSE
)

# Data preparation
dives <- dives %>%
  left_join(ocean_basin_lookup, by = "exp_id") %>%
  left_join(climate_zone_lookup, by = "exp_id") %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    habitat = ifelse(habitat == "", "Unknown", habitat),
    exposure = ifelse(exposure == "", "Unknown", exposure),
    region_clean = ifelse(region == "", "Unknown", region),
    ocean = ifelse(is.na(ocean), "Unknown", ocean),
    climate_zone = ifelse(is.na(climate_zone), "Unknown", climate_zone)
  ) %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Color palettes
pristine_blue <- "#003d5c"
pristine_teal <- "#00a8cc"
pristine_coral <- "#ff6b6b"

# UI
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Pristine Seas: 15 Years of Ocean Exploration",
    titleWidth = 600
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Timeline", tabName = "timeline", icon = icon("calendar")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    
    # Filters
    selectInput("region_filter", 
                "Region:", 
                choices = c("All Regions" = "all", sort(unique(dives$region_clean))),
                selected = "all"),
    
    selectInput("habitat_filter",
                "Habitat:",
                choices = c("All Habitats" = "all", sort(unique(dives$habitat))),
                selected = "all"),
    
    selectInput("exposure_filter",
                "Exposure:",
                choices = c("All Exposures" = "all", sort(unique(dives$exposure))),
                selected = "all"),
    
    sliderInput("year_range",
                "Year Range:",
                min = min(dives$year, na.rm = TRUE),
                max = max(dives$year, na.rm = TRUE),
                value = c(min(dives$year, na.rm = TRUE), 
                         max(dives$year, na.rm = TRUE)),
                step = 1,
                sep = "")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #003d5c; }
        .small-box { border-radius: 5px; }
        .main-header .logo { font-weight: 600; font-size: 18px; }
      "))
    ),
    
    tabItems(
      # OVERVIEW TAB
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_dives", width = 3),
          valueBoxOutput("total_regions", width = 3),
          valueBoxOutput("total_expeditions", width = 3),
          valueBoxOutput("years_active", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Global Dive Locations",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 600,
            leafletOutput("main_map", height = 540)
          )
        ),
        
        fluidRow(
          box(
            title = "Dives by Ocean",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("ocean_plot", height = 350)
          ),
          box(
            title = "Dives by Climate Zone",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("climate_plot", height = 350)
          )
        )
      ),
      
      # TIMELINE TAB
      tabItem(tabName = "timeline",
              fluidRow(
                box(
                  title = "Cumulative Dives Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  height = 500,
                  plotlyOutput("cumulative_plot", height = 440)
                ),
                box(
                  title = "Seasonal Patterns",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  height = 500,
                  plotlyOutput("seasonal_plot", height = 440)
                )
              ),
              
              fluidRow(
                box(
                  title = "Expedition Timeline",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = 550,
                  plotlyOutput("expedition_summary", height = 490)
                )
              )
      ),
      
      # DATA TAB
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Dive Database",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Download Data",
            status = "info",
            width = 12,
            downloadButton("download_filtered", "Download Filtered Data"),
            tags$p("", style = "margin-top: 10px;"),
            textOutput("filter_summary")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_dives <- reactive({
    data <- dives %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    if (input$region_filter != "all") {
      data <- data %>% filter(region_clean == input$region_filter)
    }
    
    if (input$habitat_filter != "all") {
      data <- data %>% filter(habitat == input$habitat_filter)
    }
    
    if (input$exposure_filter != "all") {
      data <- data %>% filter(exposure == input$exposure_filter)
    }
    
    data
  })
  
  # Value boxes
  output$total_dives <- renderValueBox({
    valueBox(
      value = nrow(filtered_dives()),
      subtitle = "Science Dives",
      icon = icon("water"),
      color = "blue"
    )
  })
  
  output$total_regions <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_dives()$region_clean),
      subtitle = "Regions Explored",
      icon = icon("map-marked-alt"),
      color = "aqua"
    )
  })
  
  output$total_expeditions <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_dives()$exp_id),
      subtitle = "Expeditions",
      icon = icon("ship"),
      color = "teal"
    )
  })
  
  output$years_active <- renderValueBox({
    valueBox(
      value = paste(min(filtered_dives()$year, na.rm = TRUE), "-", 
                   max(filtered_dives()$year, na.rm = TRUE)),
      subtitle = "Years of Exploration",
      icon = icon("calendar-alt"),
      color = "navy"
    )
  })
  
  # Main map
  output$main_map <- renderLeaflet({
    data <- filtered_dives()
    
    # Create color palette based on year
    pal <- colorNumeric(palette = colorRampPalette(c(
      "#3d1f6b",  # Deep purple (old)
      "#7e3f8f",  # Purple
      "#c9468e",  # Magenta
      "#f56d5e",  # Coral
      "#ffa600",  
      "#ffd700"   # Gold (recent)
    ))(30),
                        domain = data$year)
    
    leaflet(data
            ) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 4,
        color = ~pal(year),
        fillOpacity = 0.6,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<strong>", site_name, "</strong><br>",
          "Region: ", region_clean, "<br>",
          "Date: ", date, "<br>",
          "Habitat: ", habitat, "<br>",
          "Exposure: ", exposure
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~year,
        title = "Year",
        opacity = 0.8,
        labFormat = labelFormat(
          digits = 0,           # No decimal places
          big.mark = ""         # No comma separator
        )
      )
  })
  
  # Explore map
  output$explore_map <- renderLeaflet({
    data <- filtered_dives()
    
    # Color by habitat
    habitat_types <- unique(data$habitat)
    habitat_colors <- setNames(
      viridis(length(habitat_types), option = "D"),
      habitat_types
    )
    
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = 0, lat = 0, zoom = 2) |> 
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~habitat_colors[habitat],
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<strong>", site_name, "</strong><br>",
          "Region: ", region_clean, "<br>",
          "Subregion: ", subregion, "<br>",
          "Date: ", date, "<br>",
          "Habitat: ", habitat, "<br>",
          "Exposure: ", exposure, "<br>",
          "Team Lead: ", team_lead
        ),
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 40,          # Smaller radius = tighter clustering
          iconCreateFunction = JS("
    function(cluster) {
      var count = cluster.getChildCount();
      var size = count < 10 ? 'small' : count < 50 ? 'medium' : 'large';
      return new L.DivIcon({
        html: '<div><span>' + count + '</span></div>',
        className: 'marker-cluster marker-cluster-' + size,
        iconSize: new L.Point(30, 30)  // Smaller icon size (default is 40x40)
      });
    }
  ")
        )
      )
  })
  
  # Ocean plot
  output$ocean_plot <- renderPlotly({
    data <- filtered_dives() %>%
      count(ocean) %>%
      arrange(desc(n)) %>%
      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
      filter(!is.na(ocean) & ocean != "Unknown")
    
    plot_ly(data, 
            labels = ~ocean, 
            values = ~n, 
            type = 'pie',
            hole = 0.6,
            marker = list(
              colors = c(
                'Pacific' = '#1e3a8a',      # Deep navy
                'Atlantic' = '#3b82f6',     # Royal blue
                'Caribbean' = '#06b6d4',    # Cyan
                'Indian' = '#8b5cf6',       # Purple
                'Southern' = '#e0f2fe',     # Pale ice blue
                'Arctic' = '#cbd5e1'        # Silver gray
              ),
              line = list(color = '#FFFFFF', width = 2)
            ),
            textinfo = 'label+percent',
            textfont = list(size = 11),
            hovertemplate = '<b>%{label}</b><br>%{value} dives (%{percent})<extra></extra>') %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1, y = 0.5)
      )
  })
  
  # Habitat plot
  output$habitat_plot <- renderPlotly({
    data <- filtered_dives() %>%
      count(habitat) %>%
      arrange(desc(n)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(habitat, n), y = n, fill = habitat)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "D") +
      labs(x = "", y = "Number of Dives") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
  })
  
  # Climate zone plot

  output$climate_plot <- renderPlotly({
    data <- filtered_dives() %>%
      count(climate_zone) %>%
      mutate(
        climate_zone = factor(climate_zone, levels = c("Tropical", "Temperate", "Polar")),
        percentage = n / sum(n) * 100
      )
    
    plot_ly(data, labels = ~climate_zone, values = ~n, type = 'pie',
            hole = 0.6,
            marker = list(colors = c('Tropical' = '#dc2626',     # Hot red
                                     'Temperate' = '#f59e0b',    # Warm amber
                                     'Polar' = '#3b82f6'),
                          line = list(color = '#FFFFFF', width = 2))) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1, y = 0.5),
        annotations = list(
          text = paste0(nrow(filtered_dives()), "<br>dives"),
          x = 0.5, y = 0.5,
          font = list(size = 20, color = "#003d5c"),
          showarrow = FALSE
        )
      )
  })
  
  # Region bar chart
  output$region_bar <- renderPlotly({
    data <- filtered_dives() %>%
      count(region_clean) %>%
      arrange(desc(n)) %>%
      head(15)
    
    p <- ggplot(data, aes(x = reorder(region_clean, n), y = n)) +
      geom_col(fill = pristine_teal) +
      coord_flip() +
      labs(x = "", y = "Dives") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlotly({
    data <- filtered_dives() %>%
      count(date, region_clean) %>%
      arrange(date)
    
    p <- ggplot(data, aes(x = date, y = n, color = region_clean)) +
      geom_point(alpha = 0.6, size = 2) +
      scale_color_viridis_d(option = "turbo") +
      labs(x = "Date", y = "Dives per Day", color = "Region") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # Cumulative plot
  output$cumulative_plot <- renderPlotly({
    
    data <- filtered_dives() %>%
      arrange(date) %>%
      mutate(cumulative = row_number())
    
    # Add milestone markers
    milestones <- data %>%
      filter(cumulative %in% c(100, 500, 1000, max(cumulative)))
    # Global Expedition start - May 2023
    global_exp_date <- as.Date("2023-05-01")
    
    cumulative_at_global <- data %>%
      filter(date <= global_exp_date) %>%
      nrow()
    
    plot_ly() %>%
      # Gradient fill under the line
      add_trace(
        data = data,
        x = ~date, 
        y = ~cumulative,
        type = 'scatter',
        mode = 'none',
        fill = 'tozeroy',
        fillcolor = 'rgba(0, 168, 204, 0.2)',
        showlegend = FALSE,
        hoverinfo = 'skip'
      ) %>%
      # Main line with gradient effect
      add_lines(
        data = data,
        x = ~date, 
        y = ~cumulative,
        line = list(
          color = '#003d5c', 
          width = 3,
          shape = 'spline'  # Smooth curves
        ),
        hovertemplate = '<b>%{x|%b %d, %Y}</b><br>Total: %{y:,} dives<extra></extra>',
        showlegend = FALSE
      ) %>%
      # Milestone markers
      add_markers(
        data = milestones,
        x = ~date,
        y = ~cumulative,
        marker = list(
          size = 12,
          color = '#ff6b6b',
          line = list(color = 'white', width = 2),
          symbol = 'circle'
        ),
        text = ~paste0(cumulative, " dives"),
        hovertemplate = '<b>Milestone: %{text}</b><br>%{x|%b %Y}<extra></extra>',
        showlegend = FALSE
      ) %>%
      add_annotations(
        x = global_exp_date,
        y = cumulative_at_global,
        text = 'Global Expedition',
        showarrow = TRUE,
        arrowhead = 3,           # Filled triangle
        arrowsize = 1.2,
        arrowwidth = 3,
        arrowcolor = 'rgba(255, 215, 0, 0.8)',
        standoff = 5,            # Distance from point
        ax = -70,                 
        ay = -60,
        font = list(
          color = '#003d5c',
          size = 12,
          family = 'Arial',
          weight = 600
        ),
        bgcolor = 'white',
        bordercolor = '#ffd700',
        borderwidth = 2,
        borderpad = 8,
        opacity = 1
      ) |> 
      # Annotations for key milestones
      add_annotations(
        data = milestones %>% filter(cumulative %in% c(500, 1000, max(data$cumulative))),
        x = ~date,
        y = ~cumulative,
        text = ~paste0(cumulative),
        showarrow = TRUE,
        arrowhead = 2,
        arrowsize = 0.5,
        arrowcolor = '#ff6b6b',
        ax = 20,
        ay = -40,
        font = list(color = '#003d5c', size = 12, family = 'Arial Black')
      ) %>%
      layout(
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = list(
            text = "Cumulative Dives",
            font = list(size = 14, color = '#003d5c')
          ),
          showgrid = TRUE,
          gridcolor = 'rgba(0, 0, 0, 0.05)',
          zeroline = FALSE
        ),
        plot_bgcolor = 'rgba(248, 249, 250, 0.5)',
        paper_bgcolor = 'white',
        hovermode = 'x unified',
        hoverlabel = list(
          bgcolor = 'white',
          bordercolor = '#003d5c',
          font = list(size = 13)
        )
      )
  })
  
  # Seasonal plot
  output$seasonal_plot <- renderPlotly({
    data <- filtered_dives() %>%
      mutate(month_num = month(date)) %>%
      count(month_num) %>%
      complete(month_num = 1:12, fill = list(n = 0)) %>%  # Fill missing months with 0
      mutate(
        month = month.name[month_num]
      )
    
    plot_ly(
      data,
      type = 'barpolar',
      r = ~n,
      theta = ~month,
      marker = list(
        color = ~n,
        colorscale = list(
          c(0, '#3d1f6b'),
          c(0.5, '#f56d5e'),
          c(1, '#ffd700')
        ),
        line = list(color = 'white', width = 2)
      ),
      text = ~paste0('<b>', month, '</b><br>', n, ' dives'),
      hovertemplate = '%{text}<extra></extra>'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            showticklabels = TRUE,
            gridcolor = 'rgba(0, 0, 0, 0.1)'
          ),
          angularaxis = list(
            direction = 'clockwise',
            rotation = 90
          ),
          bgcolor = 'rgba(248, 249, 250, 0.5)'
        ),
        showlegend = FALSE,
        paper_bgcolor = 'white'
      )
  })
  
  # Yearly heatmap
  output$yearly_heatmap <- renderPlotly({
    data <- filtered_dives() %>%
      mutate(
        month = month(date, label = TRUE),
        year = as.factor(year)
      ) %>%
      count(year, month)
    
    p <- ggplot(data, aes(x = month, y = year, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "plasma", name = "Dives") +
      labs(x = "Month", y = "Year") +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # summary
  
  output$expedition_summary <- renderPlotly({
    data <- filtered_dives() %>%
      group_by(exp_id, ocean) %>%
      summarise(
        dives = n(),
        regions = paste(unique(region_clean), collapse = ", "),  # Combine regions
        start_date = min(date),
        end_date = max(date),
        duration_days = as.numeric(difftime(max(date), min(date), units = "days")),
        .groups = "drop"
      ) %>%
      arrange(start_date)
    
    # Create stunning bubble chart
    plot_ly(data) %>%
      # Main bubbles - ONE PER EXPEDITION
      add_trace(
        type = 'scatter',
        mode = 'markers',
        x = ~start_date,
        y = ~dives,
        size = ~dives,
        color = ~ocean,
        colors = c(
          'Pacific' = '#1e3a8a',
          'Atlantic' = '#3b82f6',
          'Caribbean' = '#06b6d4',
          'Indian' = '#8b5cf6',
          'Southern' = '#64748b',
          'Arctic' = '#cbd5e1'
        ),
        text = ~paste0(
          "<b>", exp_id, "</b><br>",
          regions, "<br>",  # Show all regions for this expedition
          "━━━━━━━━━━━━━━━<br>",
          "🌊 ", ocean, " Ocean<br>",
          "🤿 <b>", dives, " dives</b><br>",
          "📅 ", format(start_date, "%b %Y"), 
          ifelse(duration_days > 0, paste0(" (", duration_days, " days)"), "")
        ),
        hovertemplate = '%{text}<extra></extra>',
        marker = list(
          opacity = 0.85,
          line = list(color = 'white', width = 3),
          sizemode = 'diameter',
          sizeref = 1.2
        ),
        showlegend = TRUE
      ) %>%
      # Add connecting timeline at bottom
      add_segments(
        x = min(data$start_date),
        xend = max(data$start_date),
        y = 0,
        yend = 0,
        line = list(color = 'rgba(0, 61, 92, 0.2)', width = 3),
        showlegend = FALSE,
        hoverinfo = 'skip'
      ) %>%
      # Add trend line
      add_lines(
        data = data %>%
          mutate(year = year(start_date)) %>%
          group_by(year) %>%
          summarise(avg = mean(dives), date = min(start_date), .groups = 'drop'),
        x = ~date,
        y = ~avg,
        line = list(
          color = 'rgba(255, 107, 107, 0.5)',
          width = 2,
          dash = 'dot'
        ),
        name = 'Yearly Average',
        hovertemplate = 'Avg: %{y:.0f} dives/expedition<extra></extra>'
      ) %>%
      # Layout with style
      layout(
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(
            min(data$start_date) - 180,
            max(data$start_date) + 180
          )
        ),
        yaxis = list(
          title = list(
            text = "<b>Dives per Expedition</b>",
            font = list(size = 15, color = '#003d5c', family = 'Arial')
          ),
          showgrid = TRUE,
          gridcolor = 'rgba(0, 0, 0, 0.05)',
          zeroline = FALSE,
          range = c(-5, max(data$dives) * 1.15)
        ),
        plot_bgcolor = 'rgba(248, 249, 250, 0.5)',
        paper_bgcolor = 'white',
        showlegend = TRUE,
        legend = list(
          title = list(text = '<b>Ocean Basin</b>', font = list(size = 13)),
          orientation = 'v',
          x = 1.02,
          y = 0.5,
          bgcolor = 'rgba(255, 255, 255, 0.95)',
          bordercolor = '#003d5c',
          borderwidth = 2
        ),
        hoverlabel = list(
          bgcolor = 'white',
          bordercolor = '#003d5c',
          font = list(size = 12, family = 'Arial')
        ),
        margin = list(r = 120)
      )
  })
  

  # Data table
  output$data_table <- renderDT({
    filtered_dives() %>%
      select(ps_site_id, date, region_clean, subregion, habitat, 
             exposure, site_name, latitude, longitude, team_lead) %>%
      datatable(
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        filter = "top",
        rownames = FALSE
      )
  })
  
  # Filter summary
  output$filter_summary <- renderText({
    paste0("Showing ", nrow(filtered_dives()), " of ", nrow(dives), " total dives")
  })
  
  # Download handler
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("pristine_seas_dives_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_dives(), file, row.names = FALSE)
    }
  )
}

# Run app
shinyApp(ui, server)
