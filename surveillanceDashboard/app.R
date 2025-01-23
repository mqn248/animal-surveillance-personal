library(future)
library(promises)
plan(multisession)
options(future.rng.onMisuse = "ignore")
source("global2.R")

# Define UI for application that draws a histogram
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
  
  # 1.0 Summary -----------------------------------------------------------------
  
  tabsetPanel(
    id = "main-tabset",
    type = "pills",
    tabPanel(
      "Summary",
      dashboardPage(
        dashboardHeader(title = "Summary"),
        dashboardSidebar(width = "200px",  
                         sidebarMenu(
                           menuItem("News Headlines", tabName = "news",
                                    menuSubItem("World", newtab = T, tabName = "world", selected = T),
                                    menuSubItem("Local", newtab = T, tabName = "local"),
                                    startExpanded = TRUE
                                    
                           ),
                           menuItem('Animal Surveillance', tabName = "surveillance")                                   
                         )
        ),
        
        dashboardBody(includeCSS("menu.css"),
                      tabItems(
                        ## 1.1 News Headline -------------------------------------------------
                        
                        tabItem(
                          tabName = "world",
                          tabsetPanel(
                            type = 'pills',
                            tabPanel(
                              'World Animal Health News',
                              panel(
                                position = 'left',
                                status = 'primary',
                                heading = 'Headlines',
                                HTML('<iframe width="100%" height="1080" src="https://www.woah.org/en/home/"></iframe>')
                              )
                            )
                          )
                        )
                      )
        ),
      )
    ),
    
    # Login -------------------------------------------------------------------
    
    tabPanel('Login',
             
             fluidRow(
               style = 'height: 40vh; display: flex; justify-content: center; align-items: center;',
               loginUI(id = "login")
             )
             
    ),
    
    
    # 2.0 Priority Diseases -----------------------------------------------------
    
    navbarMenu(title =  "Zoonotic Diseases",
               tabPanel(
                 "Priority Diseases",
                 dashboardPage(
                   dashboardHeader(title = "Priority Diseases"),
                   dashboardSidebar(width = "200px",
                                    sidebarMenu(
                                      menuItem("Anthrax", tabName = "ant"),        
                                      menuItem('African Swine Fever (ASF)', tabName = "asf"),
                                      menuItem('Avian Influenza (AI)', tabName = "ai"),
                                      menuItem('Brucellosis', tabName = "bru"),
                                      menuItem('Contagious Bovine Pleuro Pneumonia (CBPP)', tabName = "cbpp"), 
                                      menuItem('Contagious Caprine Pleuro Pneumonia (CCPP)', tabName = "ccpp"), 
                                      menuItem('Foot Mouth Disease (FMD)', tabName = "fmd"), 
                                      menuItem('Lumpy Skin Disease (LSD)', tabName = "lsd"), 
                                      menuItem('New Castle Disease (NCD)', tabName = "ncd"), 
                                      menuItem('Peste des Petits Ruminant (PPR)', tabName = "ppr"), 
                                      menuItem('Rabies', tabName = "rabies"), 
                                      menuItem('Rift Valley Fever (RVF)', tabName = "rvf"), 
                                      menuItem('Sheep and Goat Pox (SGP)', tabName = "sgp") 
                                    )),
                   
                   dashboardBody(includeCSS("menu.css"), 
                                 tabItems(
                                   ## 2.1 Anthrax ----------------------------------------------------------
                                   
                                   tabItem(tabName = "ant"
                                           
                                   ), 
                                   ## 2.2 African Swine Fever (ASF) ----------------------------------------------------
                                   
                                   ## 2.4 Brucellosis ----------------------------------------------------
                                   tabItem(
                                     tabName = "bru",
                                     fluidRow(
                                       # First row for filters
                                       column(12,
                                              fluidRow(
                                                column(4, uiOutput("county_filter_ui")),
                                                column(4, 
                                                       selectInput("grouping_filter", "Select Grouping", 
                                                                   choices = c("Number_at_Risk", "Number_Sick", "Number_Humans_Affected_zoonosis","Number_Dead"), multiple = TRUE, selected = "Number_at_Risk")
                                                ),
                                                column(4,
                                                       dateRangeInput("date_range", "Select Date Range", 
                                                                      start = Sys.Date() - 30, end = Sys.Date()))
                                              )
                                       ),
                                       fluidRow(
                                         column(4,
                                                highchartOutput("brucellosis_map", height = "500px")
                                         ),
                                         column(8,
                                                DT::dataTableOutput("brucellosis_summary_table"))
                                       ),
                                       
                                       fluidRow(
                                         column(
                                           12,
                                           plotlyOutput("brucellosis_trend")
                                         ))
                                     )
                                     
                                   ),# End Brucellosis
                                   # 2.3 Avian Influenza (AI) -------------------------------------------------------------
                                   
                                   tabItem(tabName = "ai"
                                           
                                   ),
                                   
                                   ## 2.4 Brucellosis ----------------------------------------------------
                                   
                                   tabItem(tabName = "bru"
                                           
                                   ),
                                   
                                   ## 2.5 Contagious Bovine Pleuro Pneumonia (CBPP) ----------------------------------------------------
                                   
                                   tabItem(tabName = "cbpp"
                                           
                                   ),
                                   
                                   ## 2.6 Contagious Caprine Pleuro Pneumonia (CCPP) ----------------------------------------------------
                                   
                                   tabItem(tabName = "ccpp"
                                           
                                   ),
                                   
                                   ## 2.7 Foot Mouth Disease (FMD) ----------------------------------------------------
                                   
                                   tabItem(tabName = "fmd"
                                           
                                   ),
                                   
                                   ## 2.8 Lumpy Skin Disease (LSD) ----------------------------------------------------
                                   
                                   tabItem(tabName = "lsd"
                                           
                                   ),
                                   
                                   ## 2.9 New Castle Disease (NCD) ----------------------------------------------------
                                   
                                   tabItem(tabName = "ncd"
                                           
                                   ),
                                   
                                   ## 2.10 Peste des Petits Ruminant (PPR) ----------------------------------------------------
                                   
                                   tabItem(tabName = "ppr"
                                           
                                   ),
                                   
                                   ## 2.11 Rabies ----------------------------------------------------
                                   
                                   tabItem(tabName = "rabies"
                                           
                                   ),
                                   
                                   ## 2.12 Rift Valley Fever (RVF) ----------------------------------------------------
                                   
                                   tabItem(tabName = "rvf"
                                           
                                   ),
                                   
                                   ## 2.13 Sheep and Goat Pox (SGP) ----------------------------------------------------
                                   
                                   tabItem(tabName = "sgp"
                                           
                                   )
                                   
                                 )
                   )
                 )
               ),
               
               
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # 0. Authentication ----------------------------------------------------------
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = 'user',
    pwd_col = 'password'
  )
  authenticated <- reactiveVal({})
  observe({
    if (credentials()$user_auth) {
      authenticated(TRUE)  # Set authenticated to TRUE if user_auth is TRUE
      shiny::showTab(inputId = "animal_surveillance", target = "Animal Surveillance")
      shinyalert::shinyalert(
        title = "Logged in successfully!",
        text = "Redirecting to first page",
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        timer = 10000,
        immediate = TRUE,
        closeOnClickOutside = FALSE,
        closeOnEsc = FALSE
      )
      
      # This should switch to the Summary tab
      updateTabsetPanel(session, "main-tabset", selected = "Summary")
      
    } else {
      authenticated(FALSE)  # Set authenticated to FALSE if user_auth is FALSE
      shiny::hideTab(inputId = "animal_surveillance", target = "Animal Surveillance")
    }
  })
  
  #kabs_records Table ------------------------------------------------
    kabs_records <- reactive({
      readxl::read_excel("D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/dummy_data/Cleaned Animal data.xlsx")
    })
  
  # Dynamically render County filter -----------------------------------------
  output$county_filter_ui <- renderUI({
    req(kabs_records())
    selectInput("county_filter", "Filter by County", 
                choices = unique(kabs_records()$County), multiple = TRUE, selected = "Busia")
  })
  
    # Dynamically render Grouping filter ----------------------------------------
  output$grouping_filter_ui <- renderUI({
    req(kabs_records())
    selectInput("grouping_filter", "Select Grouping", 
                choices = c("Number_at_Risk", "Number_Sick","Number_Dead", "Number_Humans_Affected_zoonosis"))
  })
  
  # Brucellosis data filtering ----------------------------------------------
  brucellosis_data <- reactive({
    req(kabs_records())
    data <- kabs_records() |>
      filter(Disease_Condition == "Brucellosis")
    
    if (!is.null(input$county_filter)) {
      data <- data |>
        filter(County %in% input$county_filter)
    }
    
    if (!is.null(input$date_range)) {
      data <- data |>
        filter(Start_Outbreak_Event >= input$date_range[1] & Start_Outbreak_Event <= input$date_range[2])
    }
    
    data
  })
  
  # Prepare the Brucellosis data and merge with shapefile --------------------
  brucellosis_map_data <- reactive({
    req(brucellosis_data(), input$grouping_filter)
    
    # Filter the data based on selected county
    filtered_data <- brucellosis_data()
    
    if (!is.null(input$county_filter)) {
      filtered_data <- filtered_data |>
        filter(County %in% input$county_filter)
    }
    
    # Summarize data by County and selected grouping variable to join with shapefile
    grouping_var <- input$grouping_filter
    summarized_data <- filtered_data |>
      group_by(County) |>
      summarise(Grouping_Value = sum(!!sym(grouping_var), na.rm = TRUE))
    
    # Merge with shapefile
    merged_data <- kenya_shape |>
      left_join(summarized_data, by = c("ADM1_EN" = "County"))
    
    merged_data
  })
  
  # Render the highcharter map -------------------------------------------------
  output$brucellosis_map <- renderHighchart({
    req(brucellosis_map_data())
    
    # Filtered data for mapping
    map_data <- brucellosis_map_data()
    
    # Convert sf object to GeoJSON
    kenya_geojson <- geojsonsf::sf_geojson(map_data)
    
    # Create the interactive map
    highchart(type = "map") |>
      hc_add_series(
        mapData = kenya_geojson,  
        data = map_data,          
        joinBy = c("ADM1_EN", "County"),  
        value = "Grouping_Value"  # Use dynamic grouping value
      ) |>
      hc_colorAxis(min = 0, maxColor = "#006400", minColor = "#FFFFFF") |>
      hc_title(text = paste("Brucellosis Occurrence by County (", input$grouping_filter, ")")) |>
      hc_tooltip(pointFormat = "{point.ADM1_EN}: {point.Grouping_Value} cases")  
  })
  
  
  # Brucellosis Summary  by County ----------------------------------------
  brucellosis_summary <- reactive({
    req(kabs_records(), input$county_filter, input$grouping_filter)
    
    # Filter data for Brucellosis
    data <- kabs_records() |>
      filter(Disease_Condition == "Brucellosis")
    
    if (!is.null(input$county_filter)) {
      data <- data |>
        filter(County %in% input$county_filter)
    }
    
    # Summarize data by County and the selected grouping variable
    grouping_var <- input$grouping_filter
    summary_data <- data |>
      group_by(County) |>
      summarise(
        Number_at_Risk = sum(Number_at_Risk, na.rm = TRUE),
        Number_Sick = sum(Number_Sick, na.rm = TRUE),
        Number_Dead = sum(Number_Dead, na.rm = TRUE),
        Number_Humans_Affected_zoonosis = sum(Number_Humans_Affected_zoonosis, na.rm = TRUE)
      ) |>
      arrange(desc(Number_Sick)) |>  
      head(5)  
    
    summary_data
  })
  
  # Render the summary table -----------------------------------------------
  output$brucellosis_summary_table <- DT::renderDataTable({
    req(brucellosis_summary())
    
    # Render the summarized data using DT
    DT::datatable(brucellosis_summary(), options = list(pageLength = 5))
  })
  
  # Brucellosis Trend Line Plot ----------------------------------------------
  # Brucellosis Trend Line Plot with Multiple Groupings -----------------------
  output$brucellosis_trend <- renderPlotly({
    req(kabs_records(), input$date_range)
    
    # Filter data for Brucellosis only by date (no county filter)
    plot_data <- kabs_records() |>
      filter(Disease_Condition == "Brucellosis") |>
      filter(Start_Outbreak_Event >= input$date_range[1] & Start_Outbreak_Event <= input$date_range[2])
    
    # Check if the required columns are numeric before plotting
    plot_data <- plot_data |>
      mutate(Number_at_Risk = as.numeric(Number_at_Risk),
             Number_Sick = as.numeric(Number_Sick),
             Number_Dead = as.numeric(Number_Dead))
    
    # Create the plot with multiple lines (one for each grouping)
    p <- ggplot(plot_data, aes(x = Start_Outbreak_Event)) +
      geom_line(aes(y = Number_Sick, color = "Number Sick"), size = 1, linetype = "solid") +
      geom_line(aes(y = Number_Dead, color = "Number Dead"), size = 1, linetype = "solid") +
      geom_line(aes(y = Number_at_Risk, color = "Number_at_Risk"), size = 1, linetype = "solid")+
      geom_line(aes(y = Number_Humans_Affected_zoonosis, color ="Number_Humans_Affected_zoonosis"), size = 1, linetype="solid")+
      theme_minimal() +
      scale_color_manual(values = c("Number Sick" = "green", "Number Dead" = "red", "Number at Risk" = "yellow", "Number_Humans_Affected_zoonosis" =  "lightblue")) +
      labs(
        title = "Brucellosis Outbreak Trends by Grouping",
        x = "Start of Outbreak",
        y = "Count",
        color = "Groupings"
      )
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
