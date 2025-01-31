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
                #menuSubItem(
                  #"Anthrax",
                  #newtab = T,
                  #tabName = "ant",
                  #selected = T
                #),
                
                # African Swine Fever (ASF) ----------------------------------------------------------
                menuSubItem(
                  "African Swine Fever (ASF)",
                  newtab = T,
                  tabName = "asf"
                ), startExpanded = TRUE,
                
                
                # Brucellosis ----------------------------------------------------------
                menuSubItem(
                  "Brucellosis",
                  newtab = T,
                  tabName = "bru"
                ), 
                
                
                
                # Bovine ----------------------------------------------------------
                menuSubItem(
                  "Contagious Bovine Pleuropneumonia (CBPP)",
                  newtab = T,
                  tabName = "bov"
                ),
                
                
                
                # Caprine ----------------------------------------------------------
                menuSubItem(
                  "Contagious Caprine Pleuropneumonia (CCPP)",
                  newtab = T,
                  tabName = "cap"
                ),
                
                
                
                
                # Foot and Mouth Tab ------------------------------------------------------
                menuSubItem(
                  "Foot and Mouth Disease (FMD)",
                  newtab = T,
                  tabName = "fmd"
                ),
                
                
                
                
                # Lumpy ------------------------------------------------------
                menuSubItem(
                  "Lumpy Skin Disease (LSD)",
                  newtab =  T,
                  tabName = "lump"
                ),
                
                
                
                
                # New Castle -------------------------------------------
                menuSubItem(
                  "Newcastle Disease (NCD)",
                  newtab = T,
                  tabName = "ncd"
                ),

                
                # Peste des Petit Ruminants (PPR) Tab --------------------------
                menuSubItem(
                  "Peste des Petit Ruminants (PPR)",
                  newtab = T,
                  tabName = "ppr"
                ),
                
                
                
                # Rabies ------------------------------------------------------
                menuSubItem(
                  "Rabies",
                  newtab = T,
                  tabName = "rab"
                ),
                
                
                # Rift Valley Fever -------------------------------------------
                menuSubItem(
                  "Rift Valley Fever (RVF)",
                  newtab = T,
                  tabName = "rvf"
                ),
                
                
                # Goat Pox -------------------------------------------
                menuSubItem(
                  "Goat Pox",
                  newtab = T,
                  tabName = "gp"
                )
                
                
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
                  )),
                  
                  box(
                    title = "Animal Health Surveillance",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                  
                  tabsetPanel(
                    tabPanel(
                      "National Summary",
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
                ),

                
                 # County statistics map --------------------------------------------------
                  tabPanel(
                    "County Summary",
                    fluidRow(
                    # Map to display county statistics ----------------------------------------
                      column(6,
                             shinycssloaders::withSpinner(
                               highchartOutput(
                                 "kenyamap2",
                                 width = "100%",
                                 height = "600px"
                               )
                             )
                      ),
                    # Card to show the statistics ----------------------------------------
                      column(6,
                             div(
                               id = 'sick',
                               h4(
                                 strong("County statistics of animal health"), 
                                 style = "color: black; text-align: center; margin-bottom: 10px; font-size: 14px;"
                               )
                             ),
                        # Data table output for county statistics ---------------------------------
                             DTOutput("county_stats", height = "400px")
                      )
                    )
                  ),
                
                # Population statistics map --------------------------------------------------
                  tabPanel(
                    "Population Summary"
                  )
                ))),
              
              # Bio-surveillance tab   -----------------------------------------
              # African Swine Fever (ASF) tab   -----------------------------------------
              tabItem(
                tabName = "asf",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on African Swine Fever (ASF) at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "African Swine Fever (ASF) Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # African Swine Fever (ASF) trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # ANT Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapasf",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_asf")
                    ),
                    # African Swine Fever (ASF) Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_asf", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              
              # Brucellosis tab   -----------------------------------------
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
                  # Brucellosis disease burden plot ---------------------------------------------------------------
                  tabsetPanel(
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapbru",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                    # Brucellosis Bar plot ----------------------------------------------------------------
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
              ),
              
              
              # Bovine tab   -----------------------------------------
              tabItem(
                tabName = "bov",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Bovine at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                 # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Bovine Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Bovine Trend plot ----------------------------------------------------------------
                  tabsetPanel(
                    # Bovine Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapbov",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_bov")
                    ),
                    # Bovine Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_bov", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              
              # Caprine -----------------------------------------------------------------
              tabItem(
                tabName = "cap",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Caprine at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Caprine Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Caprine trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # Caprine Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapcap",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_cap")
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
                            highchartOutput("number_cap", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              
              # Foot and mouth Tab ------------------------------------------------------
              tabItem(
                tabName = "fmd",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Foot and Mouth Disease at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Foot and Mouth Disease Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # FMD trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # FMD Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapfmd",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_fmd")
                    ),
                    # FMD Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_fmd", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              
              # Lumpy Skin disease Tab -----------------------------------------------
              # Lumpy Tab -----------------------------------------------
              tabItem(
                tabName = "lump",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Lumpy Skin Disease (LSD) at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Lumpy Skin Disease (LSD) Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Lumpy trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # Lumpy Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamaplump",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_lump")
                    ),
                    # Lumpy Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_lump", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              # Newcastle Disease (NCD) Tab -----------------------------------------------
              # NCD Tab -----------------------------------------------
              tabItem(
                tabName = "ncd",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Newcastle Disease (NCD) at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Newcastle Disease (NCD) Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # NCD trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # NCD Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapncd",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_ncd")
                    ),
                    # NCD Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_ncd", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              
              # Peste des Petit Ruminants (PPR) Tab -----------------------------------------------
              # PPR Tab -----------------------------------------------
              tabItem(
                tabName = "ppr",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Peste des Petit Ruminants (PPR) at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Peste des Petit Ruminants (PPR)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # PPR trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # Peste Disease burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamapppr",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_ppr")
                    ),
                    # PPR Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_ppr", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              # Rabies Tab -----------------------------------------------
              tabItem(
                tabName = "rab",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Rabies at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Rabies Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Rabies trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # Rabies diseases burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamaprab",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_rab")
                    ),
                    # Rabies Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_rab", width = "100%", height = "500px")
                          )
                        )))
                  )
                )
              ),
              
              
              # Rift Valley Fever (RVF) Tab --------------------------------------------
              tabItem(
                tabName = "rvf",
                div(
                  id = 'intro',
                  h4(
                    strong("Zooming in on Rift Valley Fever (RVF) at national and county level"),
                    style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                  )
                ),
                
                # Tabpanels ---------------------------------------------------------------
                box(
                  title = "Rift Valley Fever (RVF) Surveillance",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Rift Valley Fever (RVF) trend plot ---------------------------------------------------------------
                  tabsetPanel(
                    # RVF diseases burden ---------------------------------------------------------------
                    tabPanel(
                      "Disease Burden",
                      fluidRow(
                        column(12,
                               shinycssloaders::withSpinner(
                                 highchartOutput(
                                   "kenyamaprvf",
                                   width = "100%",
                                   height = "600px"
                                 )
                               )
                        )
                      )
                    ),
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
                      highchartOutput("trend_plot_rvf")
                    ),
                    # Rift Valley Fever (RVF) Bar plot ----------------------------------------------------------------
                    tabPanel(
                      "Bar Plot",
                      fluidRow(
                        column(
                          12,
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = "2012-07-05", end = "2024-12-31", 
                                         min = "2012-07-05", max = "2024-12-31"),
                          withSpinner(
                            highchartOutput("number_rvf", width = "100%", height = "500px")
                          )
                        )))
                  )
                )),
                
              
              # Goat Pox Tab --------------------------------------------
                tabItem(
                  tabName = "gp",
                  div(
                    id = 'intro',
                    h4(
                      strong("Zooming in on Goat Pox at national and county level"),
                      style = "color: #27AAE1; text-align: center; margin-bottom: 10px;"
                    )
                  ),
                  
                  # Tabpanels ---------------------------------------------------------------
                  box(
                    title = "Goat Pox Surveillance",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    # Goat Pox trend plot ---------------------------------------------------------------
                    tabsetPanel(
                      # Goat pox diseases burden ---------------------------------------------------------------
                      tabPanel(
                        "Disease Burden",
                        fluidRow(
                          column(12,
                                 shinycssloaders::withSpinner(
                                   highchartOutput(
                                     "kenyamapgp",
                                     width = "100%",
                                     height = "600px"
                                   )
                                 )
                          )
                        )
                      ),
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
                        highchartOutput("trend_plot_gp")
                      ),
                      # Goat Pox Bar plot ----------------------------------------------------------------
                      tabPanel(
                        "Bar Plot",
                        fluidRow(
                          column(
                            12,
                            dateRangeInput("date_range", "Select Date Range:",
                                           start = "2012-07-05", end = "2024-12-31", 
                                           min = "2012-07-05", max = "2024-12-31"),
                            withSpinner(
                              highchartOutput("number_gp", width = "100%", height = "500px")
                            )
                          )))
                    )
                  ))
              
              
              
                
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
  
  # County statistics map per Indicator ------------------------------------
  output$kenyamap2 <- renderHighchart({
    df_disease1 <- data2 |>
      select(county, Disease_Condition, Number_at_Risk)|>
      group_by(county, Disease_Condition)|>
      summarise(Total_Number_Risk = sum(Number_at_Risk, na.rm = T))
      
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create color stops based on risk values
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        mapData = shapefile,
        data = list_parse(aggregate_breed), 
        joinBy = "county",
        value = "Total_Number_Risk", 
        borderWidth = 0.8,
        nullColor = "white",
        showInLegend = TRUE,
        borderColor = "black",
        dataLabels = list(enabled = TRUE, format = '{point.county}')) |>
      hc_colorAxis(stops = stops) |>
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("function(event) { Shiny.onInputChange('selected_county', event.point.name); }")
            )
          )
        )
      ) |>
      hc_title(text = "Kenyan Counties - Animal Health Statistics")
  })
  
  # Render the county statistics when a county is clicked
  output$county_stats <- renderDT({
    req(input$selected_county)
    
    # Filter data for the selected county
    county_data <- data2 |>
      filter(county == input$selected_county)
    
    # Aggregate statistics for the selected county
    stats_data <- county_data |>
      group_by(county) |>
      summarise(
        Total_Sick = sum(Number_Sick, na.rm = TRUE),
        Total_Vaccinated = sum(Number_Vaccinated, na.rm = TRUE),
        Total_At_Risk = sum(Number_at_risk, na.rm = TRUE),
        Most_Common_Disease = names(sort(table(Disease_Condition), decreasing = TRUE)[1]),
        Most_Affected_Species = names(sort(table(Species_Affected), decreasing = TRUE)[1]),
        Most_Used_Production_System = names(sort(table(Production_System), decreasing = TRUE)[1]),
        Most_Used_Diagnosis = names(sort(table(Nature_of_Diagnosis), decreasing = TRUE)[1])
      )
    
    # Display the statistics as a data table
    datatable(
      stats_data,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        searching = FALSE
      )
    )
  })
  
  # filter the top 10 diseases --------------------------------------------
  highburden <- reactive({
    data2 |>
      group_by(Disease_Condition, Species_Affected) |>
      distinct()|>
      summarise(Number_Sick = sum(Number_Sick, na.rm = TRUE)) |>
      arrange(desc(Number_Sick)) |>
      head(10)
  })
  
  # Plot the bar graph for the top 10 diseases -----------------------------
  output$prioritydiseases <- renderHighchart({
    
    highburden_data <- highburden() |>
      mutate(
        point_data = purrr::map2(Number_Sick, Species_Affected, 
                                 ~ list(y = .x, Species_Affected = .y))
      )
    
    diseasebar <- highchart() |> 
      hc_chart(type = "bar") |>
      hc_xAxis(categories = highburden()$Disease_Condition) |> 
      hc_add_series(
        name = "Summary of sick animals",
        data = highburden_data$point_data
      )|>
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
      )|>
      hc_tooltip(
        pointFormat = "<b>{point.category}</b><br>Species: {point.Species_Affected}<br>Number Sick: {point.y}"
      )
    diseasebar
  })
  
  # Render at glance map for top 10 priority diseases-----------------------
  observeEvent(input$clicked_disease, {
    selected_disease <- input$clicked_disease
    
    # Filter the disease data based on the selected disease
    df_disease1 <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
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
  
  
  # African Swine Fever (ASF) -------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapasf <- renderHighchart({
    asf_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "African Swine Fever (ASF)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(asf_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(asf_map$value),
        max = max(asf_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # African Swine Fever (ASF) trend line --------------------------------------------------
  filtered_data_asf <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "African Swine Fever (ASF)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_asf <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_asf()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "African Swine Fever (ASF) Trend") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # African Swine Fever (ASF) bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    asf_data <- aggregate_breed|>
      filter(Disease_Condition == "African Swine Fever (ASF)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(asf_data$Total_Number_Sick)
    total_vaccinated <- sum(asf_data$Total_Vaccinated)
    total_risk <- sum(asf_data$Total_Number_Risk)
    
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
  output$number_asf <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated African Swine Fever (ASF) Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  

# BRUCELLOSIS -------------------------------------------------------------
  # Brucellosis Disease Burden ----------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapbru <- renderHighchart({
    bru_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Brucellosis")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(bru_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(bru_map$value),
        max = max(bru_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
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
  

# BOVINE ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapbov <- renderHighchart({
    bov_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Contagious Bovine Pleuropneumonia (CBPP)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(bov_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(bov_map$value),
        max = max(bov_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  
  # Bovine trend line --------------------------------------------------
  filtered_data <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Contagious Bovine Pleuropneumonia (CBPP)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_bov <- renderHighchart({
    # Get the filtered data
    data <- filtered_data()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Contagious Bovine Pleuropneumonia (CBPP) Trend") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Bovine bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    bov_data <- aggregate_breed|>
      filter(Disease_Condition == "Contagious Bovine Pleuropneumonia (CBPP)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(bov_data$Total_Number_Sick)
    total_vaccinated <- sum(bov_data$Total_Vaccinated)
    total_risk <- sum(bov_data$Total_Number_Risk)
    
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
  output$number_bov <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Bovine Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # CAPRINE ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapcap <- renderHighchart({
    cap_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Contagious Caprine Pleuropneumonia (CCPP)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(cap_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(cap_map$value),
        max = max(cap_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  
  # Caprine trend line --------------------------------------------------
  filtered_data_cap <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Contagious Caprine Pleuropneumonia (CCPP)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_cap <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_cap()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Contagious Caprine Pleuropneumonia (CCPP)") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Caprine bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    cap_data <- aggregate_breed|>
      filter(Disease_Condition == "Contagious Caprine Pleuropneumonia (CCPP)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(cap_data$Total_Number_Sick)
    total_vaccinated <- sum(cap_data$Total_Vaccinated)
    total_risk <- sum(cap_data$Total_Number_Risk)
    
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
  output$number_cap <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated caprine Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Foot and Mouth Disease (FMD) ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapfmd <- renderHighchart({
    fmd_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Foot and Mouth Disease (FMD)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(fmd_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(fmd_map$value),
        max = max(fmd_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  
  # FMD trend line --------------------------------------------------
  filtered_data_fmd <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Foot and Mouth Disease (FMD)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")

    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_fmd <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_fmd()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Foot and Mouth Disease (FMD)") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # FMD bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    fmd_data <- aggregate_breed|>
      filter(Disease_Condition == "Foot and Mouth Disease (FMD)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(fmd_data$Total_Number_Sick)
    total_vaccinated <- sum(fmd_data$Total_Vaccinated)
    total_risk <- sum(fmd_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # FMD Render the bar plot with date filter
  output$number_fmd <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Foot and Mouth Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Lumpy Skin Disease (LSD) ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamaplump <- renderHighchart({
    lump_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Lumpy Skin Disease (LSD)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(lump_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(lump_map$value),
        max = max(lump_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # Lumpy Skin Disease (LSD) trend line --------------------------------------------------
  filtered_data_lump <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Lumpy Skin Disease (LSD)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_lump <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_lump()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Lumpy Skin Disease (LSD)") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Lumpy Skin Disease (LSD) bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    lump_data <- aggregate_breed|>
      filter(Disease_Condition == "Lumpy Skin Disease (LSD)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(lump_data$Total_Number_Sick)
    total_vaccinated <- sum(lump_data$Total_Vaccinated)
    total_risk <- sum(lump_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # Lumpy Render the bar plot with date filter
  output$number_lump <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Lumpy Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Newcastle Disease (NCD) ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapncd <- renderHighchart({
    ncd_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Newcastle Disease (NCD)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(ncd_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(ncd_map$value),
        max = max(ncd_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # Newcastle Disease (NCD) trend line --------------------------------------------------
  filtered_data_ncd <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Newcastle Disease (NCD)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_ncd <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_ncd()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Newcastle Disease (NCD)") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Newcastle Disease (NCD) bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    ncd_data <- aggregate_breed|>
      filter(Disease_Condition == "Newcastle Disease (NCD)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(ncd_data$Total_Number_Sick)
    total_vaccinated <- sum(ncd_data$Total_Vaccinated)
    total_risk <- sum(ncd_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # ncd Render the bar plot with date filter
  output$number_ncd <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated New Castle Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Peste des Petit Ruminants (PPR) ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapppr <- renderHighchart({
    ppr_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Peste des Petit Ruminants (PPR)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(ppr_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(ppr_map$value),
        max = max(ppr_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # Peste des Petit Ruminants (PPR) trend line --------------------------------------------------
  filtered_data_ppr <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Peste des Petit Ruminants (PPR)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_ppr <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_ppr()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Peste des Petit Ruminants (PPR)") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Peste des Petit Ruminants (PPR) bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    ppr_data <- aggregate_breed|>
      filter(Disease_Condition == "Peste des Petit Ruminants (PPR)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(ppr_data$Total_Number_Sick)
    total_vaccinated <- sum(ppr_data$Total_Vaccinated)
    total_risk <- sum(ppr_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # Peste des Petit Ruminants (PPR) Render the bar plot with date filter
  output$number_ppr <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Peste des Petit Ruminants (PPR) Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Rabies ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamaprab <- renderHighchart({
    rab_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Rabies")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(rab_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(rab_map$value),
        max = max(rab_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # Rabies trend line --------------------------------------------------
  filtered_data_rab <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Rabies",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_rab <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_rab()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Rabies") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Rabies bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    rab_data <- aggregate_breed|>
      filter(Disease_Condition == "Rabies",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(rab_data$Total_Number_Sick)
    total_vaccinated <- sum(rab_data$Total_Vaccinated)
    total_risk <- sum(rab_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # Rabies Render the bar plot with date filter
  output$number_rab <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Rabies Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Rift Valley Fever (RVF) ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamaprvf <- renderHighchart({
    rvf_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Rift Valley Fever (RVF)")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(rvf_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(rvf_map$value),
        max = max(rvf_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # Rift Valley Fever (RVF) trend line --------------------------------------------------
  filtered_data_rvf <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Rift Valley Fever (RVF)",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_rvf <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_rvf()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Rift Valley Fever (RVF)") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Rift Valley Fever (RVF) bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    rvf_data <- aggregate_breed|>
      filter(Disease_Condition == "Rift Valley Fever (RVF)",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(rvf_data$Total_Number_Sick)
    total_vaccinated <- sum(rvf_data$Total_Vaccinated)
    total_risk <- sum(rvf_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # Rift Valley Fever (RVF) Render the bar plot with date filter
  output$number_rvf <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Rift Valley Fever Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })
  
  
  # Goat Pox ------------------------------------------------------------------
  # National/Kenya map ------------------------------------------------------
  output$kenyamapgp <- renderHighchart({
    gp_map <- data2 |>
      select(county, Disease_Condition, Number_Sick)|>
      group_by(county, Disease_Condition)|>
      summarise(value = sum(Number_Sick, na.rm = T))|>
      filter(Disease_Condition == "Goat Pox")
    
    # county shapefile
    shapefile <- jsonlite::toJSON(county_shapefile) 
    
    ylgn_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlGn")
    # Create stops for the color axis
    stops <- color_stops(9, colors = ylgn_palette)
    
    highchart(type = "map") |>
      hc_add_series(
        name = "Back to main plot",
        mapData =  shapefile,
        data = list_parse(gp_map),
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
          pointFormat = paste0("<b style=\"color:#1874CD\"> Number sick:</b> {point.value:.2f}<br>"),
          footerFormat = "</p>"
        )
      ) |>
      hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
      hc_colorAxis(
        min = min(gp_map$value),
        max = max(gp_map$value), 
        stops = stops  
      ) |> 
      hc_exporting(enabled = TRUE)
  })
  # Goat Pox trend line --------------------------------------------------
  filtered_data_gp <- reactive({
    data <- aggregate_breed |>
      filter(Disease_Condition == "Goat Pox",
             Report_Date >= "2012-07-05" & Report_Date <= "2024-12-31")
    
    data$Month <- floor_date(data$Report_Date, "month")
    
    # Aggregate by Month and selected indicator(s)
    data <- data|>
      group_by(Month) |>
      summarise(across(input$indicator_picker, sum, na.rm = TRUE), .groups = 'drop')
    
    return(data)
  })
  
  # Render the trend plot
  output$trend_plot_gp <- renderHighchart({
    # Get the filtered data
    data <- filtered_data_gp()
    
    hc <- highchart() |>
      hc_xAxis(categories = as.Date(data$Month)) |>
      hc_yAxis(title = list(text = "Count")) |>
      hc_title(text = "Goat Pox") |>
      hc_tooltip(shared = TRUE, valueSuffix = " units") |>
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
    # Add series for each selected indicator
    for (indicator in input$indicator_picker) {
      hc <- hc |>
        hc_add_series(data = data[[indicator]], name = indicator, type = 'line')
    }
    
    return(hc)
  })
  
  # Goat Pox bar plot ----------------------------------------------------
  filtered_bar_data <- reactive({
    
    gp_data <- aggregate_breed|>
      filter(Disease_Condition == "Goat Pox",
             Report_Date >= input$date_range[1] & Report_Date <= input$date_range[2])
    
    # Totals for each grouping
    total_sick <- sum(gp_data$Total_Number_Sick)
    total_vaccinated <- sum(gp_data$Total_Vaccinated)
    total_risk <- sum(gp_data$Total_Number_Risk)
    
    # Data for the chart
    chart_data <- data.frame(
      category = c("Number Sick", "Number Vaccinated", "Number at Risk"),
      total = c(total_sick, total_vaccinated, total_risk)
    )
    
    # Sort the data by total in descending order
    chart_data <- chart_data[order(-chart_data$total),]
    
    return(chart_data)
  })
  
  # Goat pox Render the bar plot with date filter
  output$number_gp <- renderHighchart({
    chart_data <- filtered_bar_data()
    
    # Create the bar chart
    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(categories = chart_data$category) |>
      hc_add_series(name = "Sum over time ", data = chart_data$total) |>
      hc_title(text = "Aggregated Goat pox Occurrences") |>
      hc_yAxis(visible = FALSE) |>
      hc_xAxis(title = list(text = "Surveillance Indicators")) |>
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
  })

}

shinyApp(ui = ui, server = server)

