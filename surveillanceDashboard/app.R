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
  id = "vet_units",
  
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
                           menuItem("News Headline", tabName = "news",
                                    # menuSubItem("Location", newtab = T, tabName = "loc", selected = T),
                                    # menuSubItem("Number per county", newtab = T, tabName = "number"),
                                    # menuSubItem("Number per 10,000", newtab = T, tabName = 'density' ),
                                    startExpanded = TRUE
                                    
                           ),
                           menuItem('Ownership and Level of care', tabName = "owner")                                   
                         )
                         ),
      
        dashboardBody(includeCSS("menu.css"),
                      tabItems(
                        ## 1.1 News Headline -------------------------------------------------
                        
                        tabItem(
                          tabName = "news",
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
                      )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
