# Load the required libraries
source('global2.R')
library(shinyauthr)
library(ggplot2)

# set credentials for user authentication ----------------------------------
user_creds <- dplyr::tibble(
  user = c("dvs"),   
  password = c("dvs@2024"),                                   
)

# Define the UI 
tags$head(
  tags$style(HTML("
      .navbar-default .navbar-brand {
        font-size: 28px;
        font-weight: bold;
        color: #2c3e50;
         
      }
       .modal-dialog {
        position: fixed;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        max-width: 400px;
        width: 100%;
      }
      
      /* Style the modal content */
      .modal-content {
        background-color: #ecf0f1;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.2);
      }
      
      /* Style the title */
      .modal-title {
        font-size: 1.5em;
        font-weight: bold;
        color: #34495e;
      }
      
      /* Style the input fields */
      .form-control {
        border-radius: 5px;
        border: 1px solid #bdc3c7;
        padding: 10px;
      }
      
      /* Style the buttons */
      .modal-footer button {
        border-radius: 5px;
        padding: 10px 20px;
        font-size: 1em;
      }
      
      .btn-primary {
        background-color: #3498db;
        border: none;
      }
      
      .btn-primary:hover {
        background-color: #2980b9;
      }
      
      .btn-secondary {
        background-color: #95a5a6;
        border: none;
      }
      
      .btn-secondary:hover {
        background-color: #7f8c8d;
      }
    ")))

ui <- navbarPage(
  id = "navbar",

  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", href = "veterinarylogo.jpg")
  ),
  tags$head(
    tags$title("Essential Pharmaceuticals Package")
  ),
  includeCSS("styles.css"),
  includeCSS("nav.css"),
  
  # About Tab
  tabPanel(title = tagList(icon("home"), "About"),
           fluidRow(
               # Healthcare Fund Cards
               h3("Healthcare Funds", style = "font-size: 22px; color: #2c3e50; margin-top: 30px;text-align: center;"),
               fluidRow(
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
          
  # Datasets
  tabPanel(
    title = tagList(icon("table"), "Datasets"),
    fluidRow(
      column(12,
             div(
               class = "card",
               style = "background-color: #ecf0f1; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
               fluidRow(
                 column(
                   width = 6,
                   p("Please select data from the dropdown menu:",
                     style = "font-size: 20px; color: #006D77;text-align: left;")
                 ),
                 column(
                   width = 6,
                   uiOutput("dataset_dropdown")
                 )
               )
             ),
             div(
               class = "card",
               style = "background-color: #ecf0f1; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
               DTOutput("data_table")
             ),
             fluidRow(
               style = 'height: 40vh; display: flex; justify-content: center; align-items: center;',
               loginUI(id = "login"),                                   
             )
      )
    )),
  
  # Priority Diseses tab
  tabPanel(
    title = tagList(icon("chart-bar"), "Zoonotic Diseases"),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,  
        h3("Priority Disease Summary table", style = "color: #2c3e50; text-align: center;"),
        DT::dataTableOutput("aggregated_table")  
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabs",  
          tabPanel(
            "Priority Diseseases", 
            fluidRow(
              # Title and Description
              column(12, 
                     div(
                       class = "card",
                       style = "background-color: #ecf0f1; padding: 20px; border-radius: 15px; margin-bottom: 20px;",
                       h2("National Summary", style = "font-size: 28px; color: #2c3e50; text-align: center;"),
                       p("Summary of animal surveillance per county in Kenya per priority disease", 
                         style = "font-size: 18px; color: #34495e; text-align: justify; padding-bottom: 20px;")
                     )
              )
            ),
            fluidRow(
              column(4, 
                     div(class = "card", 
                         style = "background-color: #ffffff; padding: 15px; border-radius: 10px;",
                         selectInput(
                           inputId = "county_names", 
                           label = "Select County:", 
                           choices = NULL 
                         )
                     )
              ),
              
              column(4, 
                     div(class = "card", 
                         style = "background-color: #ffffff; padding: 15px; border-radius: 10px;",
                         selectInput(
                           inputId = "animal_dissease", 
                           label = "Select Priority Disease:", 
                           choices = c("Brucellosis", "Lumpy Skin Disease (LSD)", "Rabies",
                                       "Contagious Caprine Pleuro Pneumonia (CCPP)",
                                       "Foot Mouth Disease (FMD)","Contagious Bovine Pleuro Pneumonia (CBPP)","Anthrax",
                                       "Anthrax","New Castle Disease (NCD)","Peste des Petits Ruminant (PPR)",
                                       "Sheep and Goat Pox (SGP)"),
                           selected = "Brucellosis"
                         )
                     )
              ),
              column(4, 
                     div(class = "card", 
                         style = "background-color: #ffffff; padding: 15px; border-radius: 10px;",
                         selectInput(
                           inputId = "group_by", 
                           label = "Group  by different Risk groups:", 
                           choices = NULL
                         )
                     )
              )
            ),
            # Plot aggregated sum of medicine by the different groupings --------------
            fluidRow(
              column(12, 
                     div(
                       class = "card",
                       style = "background-color: #ffffff; padding: 20px; border-radius: 10px; margin-top: 20px;",
                       plotOutput("risk_plot")
                     )
              )
            )
          ),
          tabPanel(
            "Interventions", 
            fluidRow(
              column(12, 
                     h3("Interventions Section"), 
                     p("This section will display information regarding different interventions.")
              )
            )
          )
        )
      )
    )
  ))

# Server: Secure only the Datasets tab
server <- function(input, output, session) {
  
  # Begin Data set panel -----------------------------------------------------
  # Initialize credentials --------------------------------------------------
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_creds,
    user_col = user,
    pwd_col = password
  )
  
  # Authentication to track user  details ------------------------------------
  authenticated <- reactiveVal(FALSE)
  observe({
    if (credentials()$user_auth) {
      authenticated(TRUE)                                            
    } else {
      authenticated(FALSE)                                            
    }
  })
  
  # authenticate the log in on the drop down in dataset tab -------------------
  output$dataset_dropdown <- renderUI({
    req(authenticated())
    selectInput("dataset", "Choose a Dataset:", 
                choices = c("Animal Surveillance Records"))
  })
  
  # Render table after dataset selection ------------------------------------
  observeEvent(input$dataset, {
    req(authenticated())
    output$data_table <- renderDT({
      data <- switch(input$dataset,
                     "Animal Surveillance Records" = dbReadTable(conn, "kabs_records")
      )
     
      datatable(
        data,
        escape = FALSE,
        extensions = 'Buttons',
        rownames = FALSE,
        filter = 'top',
        options = list(
          searching = TRUE,
          paging = TRUE,
          bInfo = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          autoWidth = TRUE,
          dom = 'tB',
          buttons = list(
            I('colvis'), 'copy', 'print',
            list(
              extend = 'collection',
              buttons = list(
                list(extend = "csv", filename = "page", exportOptions = list(
                  columns = ":visible", modifier = list(page = "current"))
                ),
                list(extend = 'excel', filename = "page", title = NULL,
                     exportOptions = list(columns = ":visible", modifier = list(page = "current")))
              ),
              text = 'Download current page'
            )
          ),
          lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All')),
          columnDefs = column_defs
        ),
        class = "cell-border hover"
      )
    })
  })
  # End Dataset panet -------------------------------------------------------
  
  # Begin Costing Analysis panel -------------------------------------------------
  
  # Fetch the datasets per funding model ------------------------------------
  animal_analysis <- reactive({
    switch(input$animal_disease,
           "Animal Surveillance Records" = dbReadTable(conn, "kabs_records"),
           
    )
  })
  
  # Update condition dropdown depending on the funding type ------------------
  observe({
    data <- animal_analysis()
    updateSelectInput(session, "county", choices = unique(data$County))
  })
  
  
  # Grouping for different medicine selected  by dose form, strength, size and intervention
  observe({
    animal_groups <- c("Number of Sick animals" = "Number_Sick",
                         "Number of dead animals" = "Number_Dead", 
                         "Nature of Diagnosis" = "Nature_of_Diagnosis", 
                         "Number of Humans Affected (If zoonosis" = "Number_Humans_Affected_zoonosis"
    )
    updateSelectInput(session, "group_by",
                      choices = names(animal_groups))
  })
  
  # Summarize the medicine to get total quantity per grouping ---------------
  output$risk_plot <- renderPlot({
    req(input$county, input$group_by, input$animal_disease)
    
    data <- animal_analysis()
    
    # filter by medicine ------------------------------------------------------
    filtered_data <- data[data$County == input$county, ]
    filtered_data <- na.omit(filtered_data)
    if (nrow(filtered_data) == 0) {
      showNotification("No data found for the selected medicine and funding model.", type = "error")
      return(NULL)
    }
    
    # Ensure Number_of_Medicines is numeric
    filtered_data$Number_at_Risk <- as.numeric(filtered_data$Number_at_Risk)
    aggregated_animals <- filtered_data |>
      group_by_at(vars(input$group_by, "Number_Sick", "Number_Dead", "Nature_of_Diagnosis", 
                       "Number_Humans_Affected_zoonosis"))|>
      summarise(Total_Quantity = sum(Number_at_Risk), .groups = "drop")
    
    
    # Plot summary table --------------------------------------------------------
    output$aggregated_table <- DT::renderDataTable({
      aggregated_animals
    }, options = list(
      pageLength = 10,  
      autoWidth = TRUE,  
      scrollX = TRUE 
    ))
    
    # Histogram to plot total quantity of medicine per quantity ---------------
    ggplot(aggregated_animals, aes_string(x = "Total_Quantity", y = input$group_by)) +
      geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 0.7) +
      labs(
        title = paste("Animal surveillance Aggregated by", input$group_by),
        x = "Total Animals", 
        y = input$group_by  
      ) +
      theme_light() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(angle = 360, hjust = 1, size = 14, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", color = "black"),
        plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
        panel.grid.major = element_line(color = "#BDC3C7"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      ) +
      scale_fill_brewer(palette = "Blues")
    
  })
}
shinyApp(ui, server)