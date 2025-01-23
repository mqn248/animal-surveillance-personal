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
                                   
                                   tabItem(tabName = "asf"

                                   ),
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
}


# Run the application 
shinyApp(ui = ui, server = server)
