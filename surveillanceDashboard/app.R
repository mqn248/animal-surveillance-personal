# Source the global file
source("global2.R")

ui <- fluidPage(
  #useShinydashboard(),
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", href = "veterinarylogo.jpg")
  ),
  tags$head(
    tags$title("Animal Surveillance")
  ),
  includeCSS("styles.css"),
  includeCSS("nav.css"),
  id = "animal_surveillance",
  
  # Landing Page --------------------------------------------------------------
  tabsetPanel(
    id = "main-tabset",
    type = "pills",
    tabPanel(
      "About",
      
      dashboardPage(
        dashboardHeader(
          title = "About"
        ),
        
        dashboardSidebar(
          width = "200px",
          sidebarMenu(
            menuItem(
              "News Headlines",
              tabName = "Headlines",
              
              # World news tab ----------------------------------------------------------
              menuSubItem(
                "World Updates",
                newtab = T,
                tabName = "world",
                selected = T
              ),
              
              # Local news tab ----------------------------------------------------------
              
              menuSubItem(
                "National Updates",
                newtab = T,
                tabName = "National News"
              ), startExpanded = TRUE
            ),
            
            
            # Animal Surveillance tab -------------------------------------------------
            menuItem(
              'Animal Surveillance',
              tabName = "Biosurveillance"
            )
            
          )
        ),
        
        # Expanding the different tabs --------------------------------------------
        dashboardBody(
          includeCSS("menu.css"),
          tabItems(
            
            
            # world news --------------------------------------------------------------
            tabItem(
              tabName = "world",
              tabsetPanel(
                type = 'pills',
                tabPanel(
                  'World Animal Surveillance Updates',
                  panel(
                    position = 'left',
                    status = 'primary',
                    heading = 'Headlines',
                    HTML('<iframe width="100%" height="1080" src="https://www.woah.org/en/home/"></iframe>')
                  )
                )
              )
            ),
            
            # Biosurveillance tab --------------------------------------------------------------
            tabItem(
              tabName = "Biosurveillance",
              div(
                id = 'intro',
                h4(
                  strong("Summary of the Animal Surveillance Report System"), 
                  style = "color: #27AAE1;text-align:center; margin-bottom:10px;"
                )
              ),
              
              div(
                id = 'intro',
                h4(
                  strong("Disease Occurrence and Risk"), 
                  style = "color: #27AAE1;text-align:center; margin-bottom:10px;"),
                
                # Select diseases ---------------------------------------------------------
                fluidRow(
                  column(1),
                  column(3,
                         selectInput(
                           inputId = "select_disease",
                           label = "Select disease",
                           choices = unique(
                             aggregare_risk$Disease_Condition),
                           selected = unique(
                             aggregare_risk$Disease_Condition
                           )[1]
                         )
                  ),
                  column(1),
                  column(2,
                         prettyRadioButtons(
                           inputId = "owner_selector",
                           label = "Select organisation :",
                           choices = c(
                             "All",
                             "Government/Public Entity",
                             "Private"
                           ),
                           selected = "Government/Public Entity",
                           icon = icon("check"),
                           animation = "smooth",
                           bigger = T,
                           fill = T,
                           thick = T,
                           outline = T,
                           status = "primary",
                           width = "100%"
                           
                         )
                  ),
                  column(1),
                  column(2,
                         pickerInput(
                           inputId = "county_selector",
                           label = "Select county",
                           choices = unique(
                             aggregare_risk$county
                           ),
                           options = pickerOptions(
                             container = "body",
                             liveSearch = TRUE
                           ),
                           width = "100%",
                           selected = unique(
                             aggregare_risk$county
                           )[1]
                         )
                  ),
                  fluidRow(
                    # Kenya map-----------------------------------------
                    column(6,
                           shinycssloaders::withSpinner(
                             highchartOutput(
                               "kenyamap",
                               width = "100%",
                               height = "600px"
                             )
                           )
                    ),
                    
                    # county map ------------------------------------------
                    column(6,
                           shinycssloaders::withSpinner(
                             highchartOutput(
                               "countymap",
                               width = "100%",
                               height = "600px"
                             )
                           )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),  # end About tab
    
    
    # Diseases tab ------------------------------------------------------------
    navbarMenu(
      title =  "Animal Surveillance",
      tabPanel(
        "At a glance",
        
        dashboardPage(
          dashboardHeader(
            title = "Diseases Distribution"
          ),
          
          dashboardSidebar(
            width = "200px",
            sidebarMenu(
              # Diseases at a glance -------------------------------------------------
              menuItem(
                'Diseases at a glance',
                tabName = "glance",
                selected = T
              ),
              
              # Priority diseases -------------------------------------------------------
              menuItem(
                "Priority Diseases",
                tabName = "priority",
                
                # Anthrax ----------------------------------------------------------
                menuSubItem(
                  "Anthrax",
                  newtab = T,
                  tabName = "ant",
                  selected = T
                ),
                
                # Brucellosis ----------------------------------------------------------
                
                menuSubItem(
                  "Brucellosis",
                  newtab = T,
                  tabName = "bru"
                ), startExpanded = TRUE
              )
            )
          ),
          
          
          # Expanding the different tabs -----------------------------------------
          dashboardBody(
            includeCSS("menu.css"),
            tabItems(
              
              # diseases at a glance----------------------------------------------
              tabItem(
                tabName = "glance",
                div(
                  id = 'intro',
                  h4(
                    strong("Summary of the top 10 diseases in Kenya"), 
                    style = "color: #27AAE1;text-align:center; margin-bottom:10px;"
                  ),
                  fluidRow(
                    # Bar graph of priority diseases -------------------------------
                    fluidRow(
                      column(6,
                             shinycssloaders::withSpinner(
                               highchartOutput(
                                 "prioritydiseases",
                                 width = "100%",
                                 height = "600px"
                               )
                             )
                      ),
                      # At a glance map ----------------------------------
                      column(6,
                             div(
                               id = 'sick',
                               h4(
                                 strong("Please click on the bar graph to see the prevalence of the disease in Kenya"), 
                                 style = "color: black; text-align: center; margin-bottom: 10px; font-size: 14px;"
                               )
                             ),
                             shinycssloaders::withSpinner(
                               highchartOutput(
                                 "kenyamap1",
                                 width = "100%",
                                 height = "600px"
                               )
                             )
                      )
                    )
                  )
                )),
              
              
              # Bio-surveillance tab   -----------------------------------------
              tabItem(
                tabName = "bru",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on brucellosis at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Brucellosis Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  tabsetPanel(
                    tabPanel(
                      "Trend Plot",
                      pickerInput(
                        inputId = "indicator_picker",
                        label = "Select Indicator:",
                        choices = c("Total_Number_Risk", "Total_Vaccinated", "Total_Number_Sick"),
                        options = pickerOptions(container = "body", liveSearch = TRUE),
                        width = "100%",
                        selected = "Total_Number_Risk",  
                        multiple = TRUE 
                      ),
                      highchartOutput("trend_plot")
                    ),
                    # Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_bru", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              )
            )
          )
        )
      )# end diseasese tab
    )
  ) 
)# end UI


# Server function  --------------------------------------------------------
server <- function(input,output,session) {
  
  # Render about page maps  -----------------------------------------------
  observeEvent(c(input$select_disease, input$county_selector),{
    df_disease <- aggregare_risk |>
      dplyr::filter(`Disease_Condition` == input$select_disease)
    
    df_disease_sub <- data_subcounty|>
      dplyr::filter(`Disease_Condition` == input$select_disease,
                    county == input$county_selector)
    
    shapefile <- jsonlite::toJSON(county_shapefile) 
    subcounty_shapefile <- subcounty_shapefile |> 
      filter(county == input$county_selector) |> 
      jsonlite::toJSON()
    
    # National/Kenya map ------------------------------------------------------
    output$kenyamap <- renderHighchart({
      ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
      # Create stops for the color axis
      stops <- color_stops(9, colors = ylgn_palette)
      
      highchart(type = "map") |>
        hc_add_series(
          name = "Back to main plot",
          mapData =  shapefile,
          data = list_parse(df_disease),
          value = 'n',
          borderWidth = 0.8,
          nullColor = "white",
          joinBy = "county",
          showInLegend = FALSE,
          borderColor = "black",
          dataLabels = list(enabled = TRUE, format = '{point.county}'),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<p>",
            pointFormat = paste0("<b style=\"color:#1874CD\"> Number at Risk:</b> {point.value:.2f}<br>"),
            footerFormat = "</p>"
          )
        ) |>
        hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
        hc_colorAxis(
          min = min(df_disease$value),
          max = max(df_disease$value), 
          stops = stops  
        ) |> 
        hc_exporting(enabled = TRUE)
    })
    
    # Render county map -------------------------------------------------------
    
    output$countymap <- renderHighchart({
      ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
      # Create stops for the color axis
      stops <- color_stops(9, colors = ylgn_palette)
      
      highchart(type = "map") |>
        hc_add_series(
          name = "Back to main plot",
          mapData =  subcounty_shapefile, # This county shapefile,
          data = list_parse(df_disease_sub),
          value = 'n',
          borderWidth = 0.8,
          nullColor = "white",
          joinBy =  "sub_county",
          showInLegend = FALSE,
          borderColor = "black",
          dataLabels = list(enabled = TRUE, format = '{point.sub_county}'),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<p>",
            pointFormat = paste0("<b style=\"color:#1874CD\"> Number at Risk:</b> {point.value:.2f}<br>"),
            footerFormat = "</p>"
          )
        ) |>
        hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
        hc_colorAxis(
          min = min(df_disease_sub$value),
          max = max(df_disease_sub$value), 
          stops = stops  
        ) |> 
        hc_exporting(enabled = TRUE)
    })
  })    
  
  # filter the top 10 diseases --------------------------------------------
  highburden <- reactive({
    data2 |>
      group_by(county,Disease_Condition) |>
      distinct()|>
      summarise(Number_Sick = sum(Number_Sick, na.rm = TRUE)) |>
      arrange(desc(Number_Sick)) |>
      head(10)
  })
  
  # Plot the bar graph for the top 10 diseases -----------------------------
  output$prioritydiseases <- renderHighchart({
    diseasebar <- highchart() |> 
      hc_chart(type = "bar") |>
      hc_xAxis(categories = highburden()$Disease_Condition) |> 
      hc_add_series(
        name = "Summary of sick animals",
        data =highburden()$Number_Sick
      ) |>
      hc_title(text = "Top 10 Disease Burdens") |> 
      hc_yAxis(title = list(text = "Number Sick")) |>
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS(
            "function() { 
             Shiny.onInputChange('clicked_disease', this.category); 
           }"
          )))
        )
      )
    diseasebar
  })
  
  # Render at glance map for top 10 priority diseases-----------------------
  observeEvent(input$clicked_disease, {
    selected_disease <- input$clicked_disease
    
    # Filter the disease data based on the selected disease
    df_disease1 <- aggregate_breed |>
      dplyr::filter(Disease_Condition == selected_disease)
    
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    # Render the map
    output$kenyamap1 <- renderHighchart({
      ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
      stops <- color_stops(9, colors = ylgn_palette)
      
      highchart(type = "map") |>
        hc_add_series(
          mapData =  shapefile,
          data = list_parse(df_disease1),
          value = 'n',
          joinBy = "county",
          name = paste("Disease Burden for", selected_disease),
          dataLabels = list(enabled = TRUE, format = '{point.county}'),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<p>",
            pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
            footerFormat = "</p>"
          )
        ) |>
        hc_colorAxis(
          min = min(df_disease1$value),
          max = max(df_disease1$value),
          stops = stops
        ) |>
        hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
        hc_exporting(enabled = TRUE)
    })
  })
  
  # Brucellosis trend line --------------------------------------------------
  filtered_data <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Brucellosis",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot <- renderHighchart({
    # Get the filtered data
    data <- filtered_data()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Brucellosis Trend") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Brucellosis bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    bru_data <- aggregate_breed|>
      filter(Disease_Condition == "Brucellosis",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(bru_data$Total_Number_Sick)
    total_vaccinated <- sum(bru_data$Total_Vaccinated)
    total_risk <- sum(bru_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # Render the bar plot with date filter
  output$number_bru <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Brucellosis Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
}

shinyApp(ui = ui, server = server)

