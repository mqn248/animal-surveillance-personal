# Some functions ----------------------------------------------------------

  source("loading.R")
  
  useShinydashboard <- function() {
    if (!requireNamespace(package = "shinydashboard"))
      message("Package 'shinydashboard' is required to run this function")
    deps <- findDependencies(shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(),
      sidebar = shinydashboard::dashboardSidebar(),
      body = shinydashboard::dashboardBody()
    ))
    attachDependencies(tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
  }
  
# functions ---------------------------------------------------------------
  plot_location <- \(input, output, session, outputID,county_outputID) {

    county <- future_promise(st_read("shapefiles/County.shp", quiet = T))
    #khfa <- (readRDS("RData/khfa.RDS"))
    location <- future_promise(fread("data/clinic_locations.csv")) %...>%
      filter(!is.na(latitude))

    map <- reactiveVal()
    facility <- reactiveVal(NULL)
    selected_county <- reactiveVal(NULL)


    observe({
      selected_county(input$main_county_selector)

      if (is.null(selected_county()) || selected_county() == "All") {
        shinyjs::hide(id = "main_location", anim = T, animType = "fade")
        county <- county %...>% jsonlite::toJSON()
        location <- location
     output$title <- renderUI({
       p(
         strong("Location of clinics"),
            style = "color:black; text-align:center; font-size: 18px;"
          )

      })
       #  output$title <- renderUI({
       # p(
       #   strong("To view more details, please click on a sub-national area (county)"),
       #      style = "color:black; text-align:center; font-size: 18px;"
       #    )
       #  })
      } else {
        map <- reactiveVal()
        shinyjs::show(id = "main_location", anim = T, animType = "fade")

        county <- county  %...>%
          filter(Name %in% selected_county()) %...>%
          jsonlite::toJSON()
        location <- location  %...>%
          filter(county %in% selected_county())

        output$title <- renderUI({})

        output$surveillance_par <- renderUI({
          if (length(selected_county()) > 1) {
            county_text <- paste(selected_county(), collapse = ", ")
            county_text <- paste(county_text, "counties")
            p(
              paste(strong("Location of clinics in", county_text)),
              style = "color:black; text-align:center; font-size: 18px;"  # Adjust the font size as needed
            )
          } else {
            p(
              strong(sprintf("Location of clinics in %s County", selected_county())),
              style = "color:black; text-align:center; font-size: 18px;"  # Adjust the font size as needed
            )
          }
        })
      }
      county  %...>%  {
        shapefile <-.
        location %...>% {
          location_df <-.
          myFuture <- future_promise(seed = NULL, {
            click_js <- JS("function(event) { Shiny.onInputChange('id', event.point.name); }")
            map <- highchart(type = "map") |>
              hc_add_series(
                mapData = shapefile,
                nullColor = "white",
                borderColor = "black",
                showInLegend = FALSE
              ) |>
              hc_add_series(
                type = "mappoint",
                dataLabels = list(enabled = FALSE),
                showInLegend = FALSE,
                data = location_df,
                color = "black",
                hcaes(
                  x = longitude,
                  y = latitude
                ),
                marker = list(
                  lineWidth = 1,
                  radius = 2,
                  lineColor = NULL
                ),
                tooltip = list(
                  headerFormat = "",
                  pointFormat = "<br>County: <b>{point.county}</b><br>
                     <br>Owner: <b>{point.ownership}</b>"
                ),
                states = list(
                  inactive = list(
                    opacity = 1
                  )
                )
              ) |>
              hc_plotOptions(series = list(events = list(click = click_js)))
          }) %...>%
            map()
          output[[outputID]] <- renderHighchart({
            req(map())
            map()
          })

        }
      }

      observeEvent(input$id, {
        services_fac <- khfa |>
          filter(`Facility name` == input$id)
        if (nrow(services_fac) > 0) {
          services <- services_fac |> pull(service)
          services <- na.omit(services)
          services_text <- if (length(services) > 15) {
            column1 <- paste(sprintf("%d. %s", seq_along(services)[1:min(15, length(services))], services[1:min(15, length(services))]), collapse = "<br>")
            column2 <- if (length(services) > 15) {
              paste(sprintf("%d. %s", seq_along(services)[16:min(30, length(services))], services[16:min(30, length(services))]), collapse = "<br>")
            } else {
            }
            column3 <- if (length(services) > 30) {
              paste(sprintf("%d. %s", seq_along(services)[31:length(services)], services[31:length(services)]), collapse = "<br>")
            } else {
            }

            paste0(
              sprintf("<strong>%s</strong> health facility offers the following services:<br><br>", input$id),
              "<div style='display: flex; gap: 5px;'>",
              "<div style='flex: 1;'>", column1, "</div>",
              "<div style='flex: 1;'>", column2, "</div>",
              "<div style='flex: 1;'>", column3, "</div>",
              "</div>",
              "<br><br>The readiness of the services is shown below:"
            )
          } else {
            paste0(
              sprintf("<strong>%s</strong> health facility offers the following services:<br><br>", input$id),
              paste(sprintf("%d. %s", seq_along(services), services), collapse = "<br>"),
              "<br><br>The readiness of the services is shown below:"
            )
          }


          output$county_level <- renderText({
              c(services_text)
          })

          output$county_level_table <- renderReactable({
            services_fac |>
              select(Service = service, Readiness = value) |>
              mutate(Readiness = Readiness/1e2) |>
              reactable(
                bordered = TRUE,
                outlined = TRUE,
                highlight = TRUE,
                fullWidth = FALSE,
                compact = TRUE,
                showPageSizeOptions = TRUE,
                showPageInfo = TRUE,
                showPagination = TRUE,
                pageSizeOptions = c(4, 10, 30, 45),
                defaultPageSize = 10,
                columns = list(
                Readiness = colDef(format = colFormat(percent = TRUE, digits = 1), width = 100),
                Service = colDef(width = 250)
                )
              )
          })
        } else {
          output$county_level <- renderText({
            "Currently, no services available for this facility."
          })
          output$county_level_table <- renderReactable({})
        }
      })

    })
  }

  plot_location1 <- \(input, output, session, outputID,county_outputID) {

    #county <- future_promise(st_read("shapefiles/County.shp", quiet = T))
    #location <- future_promise(fread("data/facilities_location_qoc.csv")) %...>%
      filter(!is.na(latitude))

    map <- reactiveVal()
    countyMap <- reactiveVal(NULL)
    selected_county <- reactiveVal(NULL)


    observe({
      selected_county(input$main_county_selector)

      if (is.null(selected_county()) || selected_county() == "All") {
        shinyjs::hide(id = "main_location", anim = T, animType = "fade")  # Initially hide the button
        county <- county %...>% jsonlite::toJSON()
        location <- location
      } else {
        map <- reactiveVal()
        shinyjs::show(id = "main_location", anim = T, animType = "fade")  # Initially hide the button
        county <- county  %...>%
          filter(Name %in% selected_county()) %...>%
          jsonlite::toJSON()
        location <- location  %...>%
          filter(county %in% selected_county())
      }
      county  %...>%  {
        shapefile <-.
        location %...>% {
          location_df <-.
          myFuture <- future_promise(seed = NULL, {
            map <- highchart(type = "map")  |>
              hc_add_series(
                mapData = shapefile,
                nullColor = "white",
                borderColor = "black",
                showInLegend = F
              )  |>
              hc_add_series(
                type = "mappoint",
                dataLabels = list(enabled = FALSE) ,
                showInLegend = F,
                data = location_df,
                color = "black",
                hcaes(
                  x = longitude,
                  y = latitude
                ),
                marker = list(
                  lineWidth = 1,
                  radius = 2,
                  lineColor = NULL
                ),
               tooltip = list(
                 headerFormat = "",
               pointFormat = "Facility: <b>{point.name}</b><br>County: <b>{point.county}</b><br>
               Level : <b>{point.keph_level}</b>
               <br>Owner: <b>{point.ownwehsip}</b>"
               ),
                states = list(
                  inactive = list(
                    opacity = 1
                  )
                )
              )

          }) %...>%
            map()
          output[[outputID]] <- renderHighchart({
            req(map())
            map()
          })

        }
      }

    })
  }
  plot_location2 <- \(location_df, shapefile,input, output, session, outputID,county_outputID) {
    map <- reactiveVal()
    countyMap <- reactiveVal(NULL)
    selected_county <- reactiveVal(NULL)

    temp_func <- \(location_df, shapefile) {
      plot <- girafe(
        ggobj = ggplot() +
          geom_sf_interactive(data = shapefile,color = "black", fill = "white", aes(data_id = Name, tooltip = paste(Name, "County"))) +
          geom_point_interactive(data = location_df, aes(x = longitude, y = latitude,
                                                         tooltip = paste(
                                                           "Facility:",name
                                                         )
          ), size = 0.5,
          col = "black",
          alpha = .6
          ) +
          theme_void(),
        options = list(opts_zoom(max = 5)),
        width = 6,
        height = 6
      ) |>
        girafe_options(
          opts_sizing(rescale = FALSE, width = 1),
          opts_hover(css = ''),
          opts_zoom(max = 5),
          opts_hover_inv(css = ""),
          opts_selection(
            css = "stroke-width: 5px; stroke: #3daae8; fill-opacity: 1; z-index: 10;"
          )
        )
      return(plot)
    }
    observe({
      if (!is.null(selected_county())) {
        output$county_level <- renderDataTable({
          location_df[county %in% selected_county(), .N, by = keph_level] |>
            setNames(c("Level", "Number of health facilities")) |>
            arrange(Level)
        })
      }
    })


    id <- paste0(outputID, "_selected")

    observe({
      if (is.null(selected_county())) {
        shinyjs::hide(id = "main_location", anim = T, animType = "fade")  # Initially hide the button

        map(
          temp_func(location_df, shapefile)
          )

        output[[outputID]] <- renderGirafe({
          req(map())
          map()
        })
        output$surveillance_par <- renderUI({p(
          "Location of clinics in the country",
          style = "color:black; text-align:center;"
        )
        })

      } else {
        shinyjs::show(id = "main_location", anim = TRUE, animType = "fade")
      }
    })

    observeEvent(input[[id]], {
      selected_county(input[[id]])

      county_shp <- shapefile |>
        filter(Name %in% selected_county())

      county_location_df <- location_df |>
        filter(county %in% selected_county())
      map(
        temp_func(county_location_df, county_shp)
      )

      output[[outputID]] <- renderGirafe({
        #req(map())
        map()
      })
      output$surveillance_par <- renderUI({
        if (length(selected_county()) > 1) {
          county_text <- paste(selected_county(), collapse = ", ")
          county_text <- paste(county_text, "counties")
          p(
            paste("Location of clinics in", county_text),
            style = "color:black; text-align:center;"
          )

        } else {
          p(
            paste( "Location of clinics in", selected_county(), "county"),
            style = "color:black; text-align:center;"
          )

        }
      })
    })

    # Logic to handle "Back to main plot" button click
    observeEvent(input$main_location, {
      # Reset to main plot
      selected_county(NULL)
      shinyjs::hide(id = "main_location")
    })
  }
  plot_owner <- \(location_df, shapefile, input, output, session, outputID, selectInputID, type) {
    temp_func <- \(location_df, shapefile, type = type){
      group <- ifelse(type == "owner", 'ownwehsip', 'keph_level')
      tooltip1 =    list(
        headerFormat = "",
        pointFormat = "{point.name}<br>Owner: {point.ownwehsip}"
      )
      tooltip2 = list(
        headerFormat = "",
        pointFormat = "{point.name}<br>KEPH Level: {point.keph_level}"
      )
      tooltip <- if (type == "owner") tooltip1 else tooltip2
      map <- highchart(type = "map") |>
        hc_add_series(
          mapData = jsonlite::toJSON(shapefile),
          nullColor = "white",
          borderColor = "black",
          showInLegend = F
        )  |>
        hc_add_series(
          type = "mappoint",
          dataLabels = list(enabled = FALSE) ,
          showInLegend = T,
          data = location_df[!is.na(latitude),],
          hcaes(
            x = longitude,
            y = latitude,
            group = !!group
          ),
          marker = list(
            lineWidth = 1,
            radius = 3,
            lineColor = NULL
          ),
          tooltip = tooltip,
          states = list(
            inactive = list(
              opacity = 1
            )
          )
        )
      return(map)
    }
    map <- reactiveVal(NULL)
    selected_county <- reactiveVal(NULL)
    observe({if (any(str_detect(input[[selectInputID]], "All"))) {
      map(temp_func(location_df, shapefile, type = type))

      output[[outputID]] <- renderGirafe({
        req(map())
        map()
      })
    } else {
      # Allow multiple counties to be selected
      selected_county(input[[selectInputID]])

      # Filter location_df and shapefile for the selected counties
      location_df <- location_df[county %in% selected_county(),]
      shapefile <- shapefile |>
        filter(Name %in% selected_county())
      map(temp_func(location_df, shapefile, type = type)
)
      output[[outputID]] <- renderGirafe({
        req(map())
        map()
      })
    }})
  }
  plot_national_tracer <- \(input, output, session, plotID, df, type) {
    temp_func <- \(df, type) {
      x_var = if (type == "services") 'services' else 'items'
      if (type == "services") {
        formatter = JS("function() {
  return this.point.services + ': ' + this.point.p + '%';
      }")

      } else {
        formatter = JS("function() {
  return this.point.items + ': ' + this.point.p + '%';
      }")

      }
      plot <- df |>
        hchart(
          hcaes(
            x = !!x_var,
            y = p
          ),
          type = "bar",
          color = "#3daae8"
        ) |>
        hc_tooltip(
          formatter = formatter
        ) |>
        hc_xAxis(
          labels = list(
            style = list(
              color = "black",
              font = "Arial"
            )
          ),
          title = list(
            text = "Service",
            style = list(
              color = "black",
              font = "Arial"
            )
          )
        ) |>
        hc_yAxis(
          labels = list(
            style = list(
              color = "black",
              font = "Arial"
            )
          ),
          title = list(
            text = "Percent",
            style = list(
              color = "black",
              font = "Arial"
            )
          )
        ) |>
        hc_add_theme(
          hc_theme_google()
        )
      return(plot)
    }
    bar <- reactiveVal(NULL)
    observe({
      bar(temp_func(df, type))
      output[[plotID]] <- renderHighchart({
        req(bar())
        bar()
      })
    })
  }
  plot_county_tracer <- \(input, output, session, plotID, selectID, county_par1, county_par2, service, df, type) {
    selected_county <- reactiveVal(NULL)
    observeEvent(input[[selectID]], {
      selected_county(input[[selectID]])
    })

    observe({
      if (selected_county() == "All" || is.null(selected_county())) {
        output$county_par1 <- renderUI({
        })
        output$county_par2 <- renderUI({
        })
        output[[plotID]] <- renderHighchart({
          highchart() %>%
            hc_chart(type = "text") %>%
            hc_title(text = "Please select one county above to view its summary",
                     align = "center",
                     verticalAlign = "middle",
                     style = list(fontSize = "20px", color = "#333333"))
        })

      } else {

      df <- df |>
        filter(county == selected_county())
      output$county_par1 <- renderUI({
        p(
          strong(sprintf("Availability of %s services tracer services in %s County",service, selected_county())),
          style = "color: black; text-align: center;"
        )

      })
      output$county_par2 <- renderUI({
        p(
          strong(sprintf("Availability of %s services tracer items in %s County",service, selected_county())),
          style = "color: black; text-align: center;"
        )
      })
      temp_func <- \(df, type) {
        x_var = if (type == "services") 'services' else 'items'
        if (type == "services") {
          formatter = JS("function() {
  return this.point.services + ': ' + this.point.p + '%';
      }")

        } else {
          formatter = JS("function() {
  return this.point.items + ': ' + this.point.p + '%';
      }")

        }
        plot <- df |>
          hchart(
            hcaes(
              x = !!x_var,
              y = p
            ),
            type = "bar",
            color = "#3daae8"
          ) |>
          hc_tooltip(
            formatter = formatter
          ) |>
          hc_xAxis(
            labels = list(
              style = list(
                color = "black",
                font = "Arial"
              )
            ),
            title = list(
              text = "Service",
              style = list(
                color = "black",
                font = "Arial"
              )
            )
          ) |>
          hc_yAxis(
            labels = list(
              style = list(
                color = "black",
                font = "Arial"
              )
            ),
            title = list(
              text = "Percent",
              style = list(
                color = "black",
                font = "Arial"
              )
            )
          ) |>
          hc_add_theme(
            hc_theme_google()
          )
        return(plot)
      }
      bar <- reactiveVal(NULL)
      observe({
        bar(temp_func(df, type))
        output[[plotID]] <- renderHighchart({
          req(bar())
          bar()
        })
      })
    }
    })

  }
  plot_county_summary <- \(input, output, session, availPlot, readyPlot, df, facility_df) {
    temp_func_no <- \(df) {
      plot <- df |>
        hchart(
          hcaes(
            x = county,
            y = total
          ),
          type = "bar",
          color = "#3daae8"
        ) |>
        hc_tooltip(
          formatter = JS("function() {
  return this.point.county + ': ' + this.point.total;
      }")
        ) |>
        hc_xAxis(
          labels = list(
            style = list(
              color = "black",
              font = "Arial"
            )
          ),
          title = list(
            text = "County",
            style = list(
              color = "black",
              font = "Arial"
            )
          )
        ) |>
        hc_yAxis(
          labels = list(
            style = list(
              color = "black",
              font = "Arial"
            )
          ),
          title = list(
            text = "Number of health facilities",
            style = list(
              color = "black",
              font = "Arial"
            )
          )
        ) |>
        hc_add_theme(
          hc_theme_google()
        )
      return(plot)
    }
    temp_func_ready <- \(facility_df) {
      df <- data.table(facility_df)[, c(1, 3, 5)] |>
        _[, .(quality = round(mean(.SD[[2]], na.rm = TRUE))), by = County][order(-quality)]
      plot <- df |>
        hchart(
          hcaes(
            x = County,
            y = quality
          ),
          type = "bar",
          color = "#3daae8"
        ) |>
        hc_tooltip(
          formatter = JS("function() {
  return this.point.County + ': ' + this.point.quality + '%';
      }")
        ) |>
        hc_xAxis(
          labels = list(
            style = list(
              color = "black",
              font = "Arial"
            )
          ),
          title = list(
            text = "County",
            style = list(
              color = "black",
              font = "Arial"
            )
          )
        ) |>
        hc_yAxis(
          labels = list(
            style = list(
              color = "black",
              font = "Arial"
            )
          ),
          title = list(
            text = "Average quality of care",
            style = list(
              color = "black",
              font = "Arial"
            )
          )
        ) |>
        hc_add_theme(
          hc_theme_google()
        )
      return(plot)
    }

    noBar <- reactiveVal(NULL)
    readyBar <- reactiveVal(NULL)
    observe({
      noBar(temp_func_no(df))
    output[[availPlot]] <- renderHighchart({
      req(noBar())
      noBar()
    })
    readyBar(temp_func_ready(facility_df))
    output[[readyPlot]] <- renderHighchart({
      req(readyBar())
      readyBar()
    })


    })
  }
  plot_geo <- \(input, output, session, plotID, shapefile, county_selector, location_df) {
    selected_county <- reactiveVal(NULL)
    observe({
      selected_county(input[[county_selector]])
    })
    map <- reactiveVal({})
    observe({
      if (selected_county() == "All") {
        location_df
      } else {
        location_df <- location_df %>%
          filter(county %in% c(selected_county()))
        shapefile <- shapefile |>
          filter(Name %in% c(selected_county())) |>
          st_as_sf()
      }
    temp_func <- \(location_df, shapefile) {
      map <- highchart(type = "map") |>
        hc_add_series(
          mapData = jsonlite::toJSON(shapefile),
          nullColor = "white",
          borderColor = "black",
          showInLegend = F
        )  |>
        hc_add_series(
          type = "mappoint",
          dataLabels = list(enabled = FALSE) ,
          showInLegend = F,
          data = location_df,
          hcaes(
            x = longitude,
            y = latitude
          ),
          color = "black",
          marker = list(
            lineWidth = 1,
            radius = 3,
            lineColor = NULL
          ),
          tooltip = list(
            headerFormat = "",
            pointFormat = "{point.facility}<br>"
          ),
          states = list(
            inactive = list(
              opacity = 1
            )
          )
        )
      return(map)
    }
      map(temp_func(location_df, shapefile))
      output[[plotID]] <- renderGirafe({
        req(map())
        map()
      })
    })
  }
  plot_score <- \(input, output, session, plotID, location_df, shapefile,county_selector, type, output_type) {

    selected_county <- reactiveVal(NULL)
    selected_output <- reactiveVal(NULL)
    observe({
      selected_county(input[[county_selector]])
      selected_output((input[[output_type]]))
    })
    map <- reactiveVal({})

    observe({
      if (selected_county() == "All") {
        location_df
      } else {
        location_df <- location_df %>%
          filter(county %in% c(selected_county()))
        shapefile <- shapefile |>
          filter(Name %in% c(selected_county())) |>
          st_as_sf()
      }
    temp_func <- \(location_df, shapefile, type){
      if (selected_output() == "Facility Level") {
      colorKey = if (type == "services") 'serv_score' else 'item_score'
      if (type == "services") {
        formatter = JS("function() {
  return this.point.facility + ': ' + this.point.serv_score + '%';
      }")

      } else {
        formatter = JS("function() {
  return this.point.facility + ': ' + this.point.item_score + '%';
      }")

      }

      map <- highchart(type = "map") |>
        hc_add_series(
          mapData = jsonlite::toJSON(shapefile),
          nullColor = "white",
          borderColor = "black",
          showInLegend = F
        )  |>
        hc_add_series(
          type = "mappoint",
          dataLabels = list(enabled = FALSE) ,
          showInLegend = F,
          data = location_df,
          colorKey = colorKey,
          hcaes(
            x = longitude,
            y = latitude
          ),
          marker = list(
            lineWidth = 1,
            radius = 3,
            lineColor = NULL
          ),
          states = list(
            inactive = list(
              opacity = 1
            )
          )
        ) |>
        hc_tooltip(
          formatter = formatter
        ) |>
        hc_colorAxis(
          min = min(location_df$serv_score, na.rm = TRUE),
          max = max(location_df$serv_score, na.rm = TRUE),
          stops = list(
            list(0, "#f87274"),
            list(0.5, "#ffd700"),
            list(1, "#63be7b")
          ),
          title = list(text = "Service Score"),
          labels = list(format = '{value}')
        )
      } else {

        colorKey = if (type == "services") 'serv_score' else 'item_score'
        if (type == "services") {
          pointFormat = paste0(
            "<b style=\"color:#1874CD\"> Average availability of tracer service:</b> {point.value:.0f}%<br>"
          )
        } else {
          pointFormat = paste0(
            "<b style=\"color:#1874CD\"> Average availability of tracer items:</b> {point.value:.0f}%<br>"
          )
        }

        location_df <- location_df |>
          _[, .(value = round(mean(get(colorKey)))), by = county] |>
          rename(c(Name = county))

        map <- highchart(type = "map") |>
          hc_add_series(
            mapData =  jsonlite::toJSON(shapefile |> select(Name, ID)),
            data = list_parse(location_df),
            value = 'n',
            borderWidth = 0.8,
            joinBy = "Name",
            showInLegend = FALSE,
            nullColor = "white",
            borderColor = "black",
            dataLabels = list(enabled = TRUE, format = '{point.Name}'),
            tooltip = list(
              useHTML = TRUE,
              headerFormat = "<p>",
              pointFormat = pointFormat,
              footerFormat = "</p>"
            )
          ) |>
          hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) |>
          hc_colorAxis(
            min =  min(location_df$value),
            max = max(location_df$value),
            stops = stops
          ) |>
          hc_exporting(enabled = TRUE)
      }
      return(map)
    }
      map(temp_func(location_df, shapefile, type))
      output[[plotID]] <- renderGirafe({
        req(map())
        map()
      })
    })
  }
  facility_level <- \(input, output, session, tableID, facilityDF, df_color_code) {
    temp_func <- \(facilityDF) {
      table <- facilityDF |>
        datatable(
          escape = FALSE,
          extensions = 'Buttons',
          rownames = FALSE,
          filter = 'top',
          options = list(
            searching = TRUE,
            paging = TRUE,
            bInfo = FALSE,
            scrollX = TRUE,
            autoWidth = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(
                targets = grep("^Quality of care for", colnames(facilityDF)) - 1,
                # Using a gradient to color continuous values
                createdCell = JS(
                  "function(td, cellData, rowData, rowIndex, colIndex) {",
                  "if (cellData !== null) {",
                  "var value = parseFloat(cellData);",
                  "if (value >= 80) {",
                  "$(td).css('background-color', '#63be7b');",
                  "} else if (value >= 50) {",
                  "$(td).css('background-color', '#ffeb84');",
                  "} else {",
                  "$(td).css('background-color', '#f87274');",
                  "}",
                  "}",
                  "}"
                )
              )
            ),
            pageLength = 30
          ),
          class = "cell-border hover"
        ) |>
        formatStyle(
          columns = c(data.table(df_color_code)[type == "Services", ]$item),
          backgroundColor = unique(data.table(df_color_code)[type == "Services", ]$color)
        ) |>
        formatStyle(
          columns = c(data.table(df_color_code)[type == "Diagnostics", ]$item),
          backgroundColor = unique(data.table(df_color_code)[type == "Diagnostics", ]$color)
        ) |>
        formatStyle(
          columns = c(data.table(df_color_code)[type == "Guideline", ]$item),
          backgroundColor = unique(data.table(df_color_code)[type == "Guideline", ]$color)
        ) |>
        formatStyle(
          columns = c(data.table(df_color_code)[type == "Equipment", ]$item),
          backgroundColor = unique(data.table(df_color_code)[type == "Equipment", ]$color)
        ) |>
        formatStyle(
          columns = c(data.table(df_color_code)[type == "Trained staff", ]$item),
          backgroundColor = unique(data.table(df_color_code)[type == "Trained staff", ]$color)
        ) |>
        formatStyle(
          columns = c(data.table(df_color_code)[type == "Commodities", ]$item),
          backgroundColor = unique(data.table(df_color_code)[type == "Commodities", ]$color)
        )


      return(table)
    }
    table <- reactiveVal()
    observe({
      table(temp_func(facilityDF))
    })

    output[[tableID]] <- renderDT({
      req(table())
      table()
    })

  }
  
# Some global pars --------------------------------------------------------
  
  ylgn_palette <- brewer.pal(n = 9, name = "YlGn")
  stops <- color_stops(9, colors = ylgn_palette)

  user_base <- dplyr::tibble(
    user = c("dvs"),
    password = c("dvs@2024"),
  )


# Create database connection ----------------------------------------------

  # Creating the connection -------------------------------------------------
  conn <-  dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "animalsurveillance.craswukoq326.us-east-1.rds.amazonaws.com",
    #dbname = "animalSurveillance",
    #host = "localhost",
    port = 5432,
    user = "postgres",
    password = "c3mA_hUb"
  )
  
  