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
                        ),
                        # Animal Surveillance -----------------------------------------------------
                        tabItem(
                          tabName = "surveillance",
                          div(
                            id = 'intro',
                            h4(
                              strong("Summary of the Animal Surveillance Report System"), style = "color: #27AAE1;text-align:center; margin-bottom:10px;"),
                            
                            fluidRow(
                              infoBoxOutput("v1"),
                              infoBoxOutput("v2"),
                              infoBoxOutput("v3"),
                              infoBoxOutput("v4"),
                              infoBoxOutput("v5"),
                            )
                          ),
                          
                          div(
                            id = "intro",
                            h4(
                              strong("Disease Occurrence and Risk"), style = "color: #27AAE1;text-align:center; margin-bottom:10px;"),
                            fluidRow(
                              column(1),
                              column(3,
                                     selectInput(
                                       inputId = "select_disease",
                                       label = "Select disease",
                                       choices = unique(data_county$`Disease/ Condition`),
                                       selected = unique(data_county$`Disease/ Condition`)[1]
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
                                       choices = unique(data_county$county),
                                       options = pickerOptions(container = "body", 
                                                               liveSearch = TRUE),
                                       width = "100%",
                                       selected = unique(data_county$county)[1]
                                     )
                              ),
                              
                            ),
                            fluidRow(
                              column(6,
                                     shinycssloaders::withSpinner(
                                       highchartOutput("map1", width = "100%", height = "600px")
                                     )
                              ),
                              column(6,
                                     shinycssloaders::withSpinner(
                                       highchartOutput("map2", width = "100%", height = "600px")
                                     )
                              )
                            )
                          )
                        )
                      )
        )
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
                                      menuItem('Brucellosis', tabName = "bru"),
                                      menuItem('Foot Mouth Disease (FMD)', tabName = "fmd"), 
                                      menuItem('Rabies', tabName = "rabies"), 
                                      menuItem('Rift Valley Fever (RVF)', tabName = "rvf")
                                      
                                    )),
                   
                   dashboardBody(includeCSS("menu.css"), 
                                 tabItems(
                                   tabItem(
                                     tabName = "ant",
                                     div(
                                       id = "intro",
                                       tags$head(
                                         # Link to the custom CSS
                                         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                                       ),
                                       
                                       h3("Number of animals with a Risk from Anthrax per county", 
                                          style = "color: black; text-align:center;")
                                     ),
                                     
                                     # First fluid row for charts
                                     fluidRow(
                                       column(
                                         6,
                                         withSpinner(
                                           highchartOutput("number_ant", width = "100%", height = "800px")
                                         )
                                       ),
                                       column(
                                         6,
                                         highchartOutput("number_ant_bar", width = "100%", height = "800px")
                                       )
                                     ),
                                     
                                     # Second fluid row for dropdowns and map
                                     fluidRow(
                                       column(
                                         6,
                                         selectInput("county_select", "Select County", choices = unique(aggregare_groupings$County))),
                                       column(
                                         6,
                                         selectInput("grouping_select", "Select Grouping", 
                                                     choices = c("Number Sick" = "Total_Number_Sick", 
                                                                 "Number Vaccinated" = "Total_Vaccinated", 
                                                                 "Number at Risk" = "Total_Number_Risk", 
                                                                 "Humans Affected" = "Total_Human_Affected",
                                                                 "Number Dead" = "Total_Dead"))
                                       )),
                                     # Third fluid row for anthrax indicators bar chart
                                     fluidRow(
                                       column(
                                         6,
                                         highchartOutput("anthrax_indicators_bar", width = "100%", height = "600px")
                                       ),
                                       column(
                                         6,
                                         highchartOutput("ant_map", width = "100%", height = "600px")
                                       )
                                       
                                     )
                                   ), # End of tabItem for "ant"
                                   
                                   ## 2.4 Brucellosis ----------------------------------------------------
                                   tabItem(
                                     tabName = "bru",
                                     div(
                                       id = "intro",
                                       tags$head(
                                         # Link to the custom CSS
                                         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                                       ),
                                       
                                       h3("Number of animals with a Risk from Brucellosis per county", 
                                          style = "color: black; text-align:center;")
                                     ),
                                     div(
                                       id = "intro",
                                       fluidRow(
                                         column(
                                           6,
                                           withSpinner(
                                             highchartOutput("number_bru", width = "100%", height = "800px")
                                           )
                                         ),
                                         column(
                                           6,
                                           highchartOutput("number_bru_bar", width = "100%", height = "800px")
                                         )
                                       ),
                                       # Second fluid row for dropdowns and map
                                       fluidRow(
                                         column(
                                           6,
                                           selectInput("county_select", "Select County", choices = unique(aggregare_groupings$County))),
                                         column(
                                           6,
                                           selectInput("grouping_select", "Select Grouping", 
                                                       choices = c("Number Sick" = "Total_Number_Sick", 
                                                                   "Number Vaccinated" = "Total_Vaccinated", 
                                                                   "Number at Risk" = "Total_Number_Risk", 
                                                                   "Humans Affected" = "Total_Human_Affected",
                                                                   "Number Dead" = "Total_Dead"))
                                         )),
                                       # Third fluid row for brucell indicators bar chart
                                       fluidRow(
                                         column(
                                           6,
                                           highchartOutput("county_indicators_bar", width = "100%", height = "600px")
                                         ),
                                         column(
                                           6,
                                           highchartOutput("bru_map", width = "100%", height = "600px")
                                         )
                                         
                                       )
                                     )
                                   ),# end brucellossis
                                   ## 2.4 Foot mouth ----------------------------------------------------
                                   tabItem(
                                     tabName = "fmd",
                                     div(
                                       id = "intro",
                                       tags$head(
                                         # Link to the custom CSS
                                         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                                       ),
                                       
                                       h3("Number of animals with a Risk from Foot mouth disease per county", 
                                          style = "color: black; text-align:center;")
                                     ),
                                     div(
                                       id = "intro",
                                       fluidRow(
                                         column(
                                           6,
                                           withSpinner(
                                             highchartOutput("number_fmd", width = "100%", height = "800px")
                                           )
                                         ),
                                         column(
                                           6,
                                           highchartOutput("number_fmd_bar", width = "100%", height = "800px")
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           6,
                                           selectInput("county_select", "Select County", choices = unique(aggregare_groupings$County))),
                                         column(
                                           6,
                                           selectInput("grouping_select", "Select Grouping", 
                                                       choices = c("Number Sick" = "Total_Number_Sick", 
                                                                   "Number Vaccinated" = "Total_Vaccinated", 
                                                                   "Number at Risk" = "Total_Number_Risk", 
                                                                   "Humans Affected" = "Total_Human_Affected",
                                                                   "Number Dead" = "Total_Dead"))
                                         )),
                                       # Third fluid row for brucell indicators bar chart
                                       fluidRow(
                                         column(
                                           6,
                                           highchartOutput("fmd_indicators_bar", width = "100%", height = "600px")
                                         ),
                                         column(
                                           6,
                                           highchartOutput("foot_map", width = "100%", height = "600px")
                                         )
                                         
                                       )
                                     )
                                   )# end foot
                                 )
                                 
                                 
                   )
                 )
               )
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
  
  # Animal Surveillance Maps ------------------------------------------------
  observeEvent(c(input$select_disease, input$county_selector), {
    df_disease <- data_county |> 
      dplyr::filter(`Disease/ Condition` == input$select_disease)
    
    df_disease_sub <- data_subcounty |> 
      dplyr::filter(`Disease/ Condition` == input$select_disease, county == input$county_selector)
    
    shapefile <- jsonlite::toJSON(county_shapefile) 
    subcounty_shapefile <- subcounty_shapefile |> 
      filter(county == input$county_selector) |> 
      jsonlite::toJSON() 
    
    output$map1 <- renderHighchart({
      ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
      # Create stops for the color axis
      stops <- color_stops(9, colors = ylgn_palette)
      
      highchart(type = "map") |>
        hc_add_series(
          name = "Back to main plot",
          mapData =  shapefile, # This county shapefile,
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
    output$map2 <- renderHighchart({
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
  
  
  
  # Brucellosis Analysis ----------------------------------------------------
  output$number_bru <- renderHighchart({
    
    # Filter for Brucellosis data
    bru_data <- aggregare_groupings |>
      filter(Disease_Condition == "Brucellosis")
    
    # Totals for each grouping
    total_sick <- sum(bru_data$Total_Number_Sick)
    total_vaccinated <- sum(bru_data$Total_Vaccinated)
    total_risk <- sum(bru_data$Total_Number_Risk)
    total_humans <- sum(bru_data$Total_Human_Affected)
    total_dead <- sum(bru_data$Total_Dead)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk", "Humans Affected", "Number Dead"),
      total = c(total_sick, total_vaccinated, total_risk, total_humans, total_dead)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    # Create the bar chart
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = chart_data$category) |> 
      hc_add_series(name = "Total", data = chart_data$total) |> 
      hc_title(text = "Aggregated Brucellosis Occurrences by Grouping Indicators") |> 
      hc_yAxis(visible = FALSE) |>  # Hide Y-axis labels
      hc_xAxis(title = list(text = "Surveillance Indicators")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  output$number_bru_bar <- renderHighchart({
    
    # Filter for Brucellosis data
    bru_data <- aggregare_groupings |>
      filter(Disease_Condition == "Brucellosis")
    
    # Sort the data by Total_Number_Risk in descending order
    bru_data <- bru_data[order(-bru_data$Total_Number_Risk),]
    
    # Create the bar chart for total risk by county
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = bru_data$County) |> 
      hc_add_series(name = "Total Risk", data = bru_data$Total_Number_Risk) |> 
      hc_title(text = "Total Risk of Brucellosis Occurrence by County") |> 
      hc_yAxis(visible = FALSE) |>  # Hide Y-axis labels
      hc_xAxis(title = list(text = "County")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  filtered_data <- reactive({
    aggregare_groupings |> 
      filter(County == input$county_select, Disease_Condition == "Brucellosis")
  })
  
  # Render the highcharter map
  output$bru_map <- renderHighchart({
    
    # Select grouping dynamically based on input
    grouping_column <- input$grouping_select
    
    # Merge shapefile with filtered data
    map_data <- merge(county_shapefile, filtered_data(), by.x = "county", by.y = "County")
    
    # Create the map
    hcmap("countries/ke/ke-all", data = map_data, value = grouping_column, 
          joinBy = c("name", "county")) |>
      hc_colorAxis(minColor = "#E0E0E0", maxColor = "#FF0000") |>
      hc_title(text = paste("Brucellosis Occurrences in", input$county_select)) |>
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.value}")
  })
  
  # Render the reactive bar chart for indicators by county
  output$county_indicators_bar <- renderHighchart({
    
    # Get the filtered data for the selected county
    county_data <- filtered_data()
    
    # Totals for each indicator
    total_sick <- sum(county_data$Total_Number_Sick, na.rm = TRUE)
    total_vaccinated <- sum(county_data$Total_Vaccinated, na.rm = TRUE)
    total_risk <- sum(county_data$Total_Number_Risk, na.rm = TRUE)
    total_humans <- sum(county_data$Total_Human_Affected, na.rm = TRUE)
    total_dead <- sum(county_data$Total_Dead, na.rm = TRUE)
    
    # Create the bar chart
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = c("Number Sick", "Number Vaccinated", "Number at Risk", "Humans Affected", "Number Dead")) |> 
      hc_add_series(name = paste("Indicators in", input$county_select), 
                    data = c(total_sick, total_vaccinated, total_risk, total_humans, total_dead)) |> 
      hc_title(text = paste("Brucellosis Indicators in", input$county_select)) |> 
      hc_yAxis(visible = FALSE) |>  
      hc_xAxis(title = list(text = "Surveillance Indicators")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  # Anthrax Analysis ----------------------------------------------------
  output$number_ant <- renderHighchart({
    
    # Filter for Brucellosis data
    ant_data <- aggregare_groupings |>
      filter(Disease_Condition == "Anthrax")
    
    # Totals for each grouping
    total_sick <- sum(ant_data$Total_Number_Sick)
    total_vaccinated <- sum(ant_data$Total_Vaccinated)
    total_risk <- sum(ant_data$Total_Number_Risk)
    total_humans <- sum(ant_data$Total_Human_Affected)
    total_dead <- sum(ant_data$Total_Dead)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk", "Humans Affected", "Number Dead"),
      total = c(total_sick, total_vaccinated, total_risk, total_humans, total_dead)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    # Create the bar chart
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = chart_data$category) |> 
      hc_add_series(name = "Total", data = chart_data$total) |> 
      hc_title(text = "Aggregated Anthrax Occurrences by Grouping Indicators") |> 
      hc_yAxis(visible = FALSE) |>  # Hide Y-axis labels
      hc_xAxis(title = list(text = "Surveillance Indicators")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  output$number_ant_bar <- renderHighchart({
    
    # Filter for Brucellosis data
    ant_data <- aggregare_groupings |>
      filter(Disease_Condition == "Anthrax")
    
    # Sort the data by Total_Number_Risk in descending order
    ant_data <- ant_data[order(-ant_data$Total_Number_Risk),]
    
    # Create the bar chart for total risk by county
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = ant_data$County) |> 
      hc_add_series(name = "Total Risk", data = ant_data$Total_Number_Risk) |> 
      hc_title(text = "Total Risk of Anthrax Occurrence by County") |> 
      hc_yAxis(visible = FALSE) |>  # Hide Y-axis labels
      hc_xAxis(title = list(text = "County")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  filtered_data <- reactive({
    aggregare_groupings |> 
      filter(County == input$county_select, Disease_Condition == "Anthrax")
  })
  
  # Render the highcharter map
  output$ant_map <- renderHighchart({
    
    # Select grouping dynamically based on input
    grouping_column <- input$grouping_select
    
    # Merge shapefile with filtered data
    map_data <- merge(county_shapefile, filtered_data(), by.x = "county", by.y = "County")
    
    # Create the map
    hcmap("countries/ke/ke-all", data = map_data, value = grouping_column, 
          joinBy = c("name", "county")) |>
      hc_colorAxis(minColor = "#E0E0E0", maxColor = "#FF0000") |>
      hc_title(text = paste("Anthrax Occurrences in", input$county_select)) |>
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.value}")
  })
  
  # Render the reactive bar chart for indicators by county
  output$anthrax_indicators_bar <- renderHighchart({
    
    # Get the filtered data for the selected county
    county_data <- filtered_data()
    
    # Totals for each indicator
    total_sick <- sum(county_data$Total_Number_Sick, na.rm = TRUE)
    total_vaccinated <- sum(county_data$Total_Vaccinated, na.rm = TRUE)
    total_risk <- sum(county_data$Total_Number_Risk, na.rm = TRUE)
    total_humans <- sum(county_data$Total_Human_Affected, na.rm = TRUE)
    total_dead <- sum(county_data$Total_Dead, na.rm = TRUE)
    
    # Create the bar chart
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = c("Number Sick", "Number Vaccinated", "Number at Risk", "Humans Affected", "Number Dead")) |> 
      hc_add_series(name = paste("Indicators in", input$county_select), 
                    data = c(total_sick, total_vaccinated, total_risk, total_humans, total_dead)) |> 
      hc_title(text = paste("Anthrax Indicators in", input$county_select)) |> 
      hc_yAxis(visible = FALSE) |>  
      hc_xAxis(title = list(text = "Surveillance Indicators")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  # Foot-Mouth disease Analysis ----------------------------------------------------
  output$number_fmd <- renderHighchart({
    
    # Filter for Brucellosis data
    fmd_data <- aggregare_groupings |>
      filter(Disease_Condition == "Foot Mouth Disease (FMD)")
    
    # Totals for each grouping
    total_sick <- sum(fmd_data$Total_Number_Sick)
    total_vaccinated <- sum(fmd_data$Total_Vaccinated)
    total_risk <- sum(fmd_data$Total_Number_Risk)
    total_humans <- sum(fmd_data$Total_Human_Affected)
    total_dead <- sum(fmd_data$Total_Dead)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk", "Humans Affected", "Number Dead"),
      total = c(total_sick, total_vaccinated, total_risk, total_humans, total_dead)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    # Create the bar chart
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = chart_data$category) |> 
      hc_add_series(name = "Total", data = chart_data$total) |> 
      hc_title(text = "Aggregated Foot Mouth Disease (FMD) Occurrences by Grouping Indicators") |> 
      hc_yAxis(visible = FALSE) |>  # Hide Y-axis labels
      hc_xAxis(title = list(text = "Surveillance Indicators")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  output$number_fmd_bar <- renderHighchart({
    
    # Filter for Brucellosis data
    fmd_data <- aggregare_groupings |>
      filter(Disease_Condition == "Foot Mouth Disease (FMD)")
    
    # Sort the data by Total_Number_Risk in descending order
    fmd_data <- fmd_data[order(-fmd_data$Total_Number_Risk),]
    
    # Create the bar chart for total risk by county
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = fmd_data$County) |> 
      hc_add_series(name = "Total Risk", data = bru_data$Total_Number_Risk) |> 
      hc_title(text = "Total Risk of Foot Mouth Disease (FMD) Occurrence by County") |> 
      hc_yAxis(visible = FALSE) |>  # Hide Y-axis labels
      hc_xAxis(title = list(text = "County")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  filtered_data <- reactive({
    aggregare_groupings |> 
      filter(County == input$county_select, Disease_Condition == "Foot Mouth Disease (FMD)")
  })
  
  # Render the highcharter map
  output$fmd_map <- renderHighchart({
    
    # Select grouping dynamically based on input
    grouping_column <- input$grouping_select
    
    # Merge shapefile with filtered data
    map_data <- merge(county_shapefile, filtered_data(), by.x = "county", by.y = "County")
    
    # Create the map
    hcmap("countries/ke/ke-all", data = map_data, value = grouping_column, 
          joinBy = c("name", "county")) |>
      hc_colorAxis(minColor = "#E0E0E0", maxColor = "#FF0000") |>
      hc_title(text = paste("Foot-Mouth Disease Occurrences in", input$county_select)) |>
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.value}")
  })
  
  # Render the reactive bar chart for indicators by county
  output$foot_indicators_bar <- renderHighchart({
    
    # Get the filtered data for the selected county
    fmd_data <- filtered_data()
    
    # Totals for each indicator
    total_sick <- sum(fmd_data$Total_Number_Sick, na.rm = TRUE)
    total_vaccinated <- sum(fmd_data$Total_Vaccinated, na.rm = TRUE)
    total_risk <- sum(fmd_data$Total_Number_Risk, na.rm = TRUE)
    total_humans <- sum(fmd_data$Total_Human_Affected, na.rm = TRUE)
    total_dead <- sum(fmd_data$Total_Dead, na.rm = TRUE)
    
    # Create the bar chart
    highchart() |> 
      hc_chart(type = "bar") |> 
      hc_xAxis(categories = c("Number Sick", "Number Vaccinated", "Number at Risk", "Humans Affected", "Number Dead")) |> 
      hc_add_series(name = paste("Indicators in", input$county_select), 
                    data = c(total_sick, total_vaccinated, total_risk, total_humans, total_dead)) |> 
      hc_title(text = paste("Foot mouth disease Indicators in", input$county_select)) |> 
      hc_yAxis(visible = FALSE) |>  
      hc_xAxis(title = list(text = "Surveillance Indicators")) |> 
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
}

shinyApp(ui = ui, server = server)
