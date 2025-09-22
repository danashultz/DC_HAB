# Draft shiny app
# September 2025

# If password protecting:
# Add credentials and secure app at end of UI and check credentials at start of server
credentials <- data.frame(
  user = c("r5_staff"), # mandatory
  password = c("microcystins20") # mandatory
)

# load script that calls libraries and imports data and formats
source("LoadData.R")

# Define UI for application that draws a histogram
ui <- page_navbar(

    ### Application title
    title = "Delta Community Data",
    bg = "#9FC7DA",
    theme = bs_theme(
      #hex codes from existing water board webpages
      primary = "#046B99",
      secondary = "#9FC7DA", 
    ),

    ### Sidebar with context and contact information
    ### Text defined in LoadData.R
    sidebar = sidebar(
      sp1,
      icon("flask", "fa-2x", align = "center", style = "color: #707070"),
      sp2,
      icon("chart-simple", "fa-2x", align = "center", style = "color: #707070"),
      sp3,
      includeHTML("html/sidebar_report_a_bloom_button.html")
    ),
    
    ### Data tab
    nav_panel(
      
      # Popup window when page is opened
      modalDialog(
        (
          navset_tab(
            nav_panel("Welcome",
                      includeHTML("html/modal_dialog_welcome.html")),
            nav_panel("Application",
                      includeHTML("html/modal_dialog_application.html"))
          )
        ),
        size = "l",
        easyClose = TRUE
      ), #end modal dialog
      
      class = "bslib-page-dashboard",
      title = "Data",
      
      # Add text above graph/table/stations tabs
      p("Laboratory results for total microcystin concentrations. Visit the Water Board ",
        tags$a(href = "https://mywaterquality.ca.gov/habs/resources/reports-map/", 
               "HAB Reports Map", target = "_blank"), " to view 
                                the latest recreatonal HAB advisory levels."),
      
      # Create a card for graph/table/station tabs
      navset_card_tab(
        full_screen = TRUE,
        
        ### Graph tab (reactive)
        nav_panel(
          "Graph",
          
          layout_sidebar(
            # sidebar with checbox inputs for stations and years
            sidebar = sidebar(
              checkboxGroupInput(
                "stations_selected", "Select station(s):",
                c(s),
                selected = s
              ),
              checkboxGroupInput(
                "years_selected", "Select year(s):",
                c(y),
                selected = y
              )
            ),
            
            # main panel with reactive plotly graph
            plotlyOutput("xy_plot"),
            "A logarithmic scale is used to plot total microcystin concentration 
            (ug/L) on the y-axis.\n
            Non-detect (ND) samples are shown on the plot below 0.1 ug/L in a 
            grey rectangle.\n
            HAB recreational advisory levels of Caution (Tier 1), Warning (Tier 2), 
            and Danger (Tier 3) are plotted at 0.8, 6.0 and 20 ug/L as dashed grey lines."
            
          ) # end layout sidebar
        ), # end Graph nav_panel tab
        
        ### Table tab (not reactive)
        nav_panel(
          "Table",
          card_body(
            DT::dataTableOutput("DataTable")
          )
        ),
        
        ### Map tab (not reactive)
        nav_panel(
          "Stations",
          card_body(
            leafletOutput(
              "StationMap", height = 400
              ),
          )
        )
      )
    ),
    
    ### About tab
    nav_panel(
      class = "bslib-page-dashboard",
      title = "About",
      img(src = "About_header.png", style = "right: 50%"),
      card(
        navlistPanel(
          id = "inTabset",
          fluid = TRUE,
          widths = c(3,9),
        tabPanel(
          "Partners and Project Background",
          includeHTML("html/about_partners.html")
        ),
        tabPanel(
          "Methods",
          includeHTML("html/about_methods.html")
          ),
        tabPanel(
          "HABs in the Delta",
          includeHTML("html/about_delta_habs.html")
        )
        )
      )
    ),
    
    # Related links in upper right of navigation menu
    nav_spacer(),
    nav_menu(
      title = "Related Links",
      align = "right",
      nav_item(
        tags$a("Central Valley Water Board HAB webpage",
               href = "https://www.waterboards.ca.gov/centralvalley/water_issues/harmful_algal_blooms/",
               target = "blank")
      ),
      nav_item(
        tags$a("Restore the Delta webpage",
               href = "https://restorethedelta.org/",
               target = "blank")
      ),
      nav_item(
        tags$a("Water Board HAB Portal",
               href = "https://mywaterquality.ca.gov/habs/",
               target = "blank")
      )
    )
)

# sets password screen
ui <- secure_app(ui)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  ### reactive data frame based on station_selelction
  df_sel <- reactive({

    validate(
      need(input$stations_selected != "", 'Please choose at least one station.')
    )

     validate(
       need(input$years_selected != "", 'Please choose at least one year.')
     )

    file <- df_plot %>%
      filter(Station %in% input$stations_selected) %>%
      filter(Year %in% input$years_selected)
    return(file)

  })

  ### reactive scatter plot of MC lab data based on station and year selection
  output$xy_plot <- renderPlotly({
    
    p <- df_sel() %>%
      ggplot(aes(x = CollectionDate, y = `Total Microcystins`, color = Station, shape = Station,
                 text = paste("Collection Date: ", CollectionDate, 
                              "<br>Total microcystins: ", total_microcystins_nd,
                              "<br>Analyzed by: ", Method)))+
      # grey lines for context at CCHAB trigger thresholds
      geom_hline(yintercept = 20, linetype = "dashed", color = "grey", alpha = 0.5)+
      geom_hline(yintercept = 6, linetype = "dashed", color = "grey", alpha = 0.5)+
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey", alpha = 0.5)+
      geom_hline(yintercept = 0.075, color = "grey", linewidth = 4, lineend = "round", alpha = 0.5)+
      geom_point(size = 3, alpha = 0.5)+
      scale_shape_manual(values = my8shapes)+
      facet_wrap(vars(Year), nrow = 1, scales = "free_x")+
      scale_y_log10(labels = scales::label_comma())+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
      labs(x = "Sample Date",
           y = "Total Microcystins (ug/L)")+
      theme_classic()
    
    #ggplotly(p, tooltip = c("CollectionDate", "total_microcystins_nd"))
    ggplotly(p, tooltip = "text")
    
  })
  
  
  
  ### Data Table
  output$DataTable <- DT::renderDataTable({
    datatable(
      data = df_table,
      extensions = 'Buttons',
      rownames = F,
      options = list(
        dom = "fBti",
        pageLength = 5000,
        buttons =
          list(
            list(
              extend = "collection",
              buttons = c("csv", "excel"),
              text = "Download"
            )
          ) # end of buttons customization
      ) # end of options
    )
  })
  
  
  ### Station Map
  output$StationMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -121.356291, lat = 37.967723, zoom = 13) %>%
      addCircleMarkers(data = df_st,
                       radius = 12,
                       color = "yellow",
                       fillOpacity = 0.6,
                       label = ~StationID,
                       layerId = ~StationID,
                       labelOptions = labelOptions(
                         noHide = TRUE,# Keep labels always visible
                         direction = "center",# Position label intelligently
                         textOnly = TRUE,# Don't show speech bubble around text
                         style = list("font-size" = "10px", "font-weight" = "bold","color"="black")
                       ),
                       popup = tooltip_text
                       )
  })
  


}

# Run the application 
shinyApp(ui = ui, server = server)
