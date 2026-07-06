################################################################################
# This script is the server for the CFEMM app
# capabilities include: viewing tables, maps, and figures

################################################################################

function(input, output, session) {

  #bs_themer()
  
  # ---------------------------- LINKS -----------------------------------------   
  # Open modal on startup
  observe({
    showModal(modalDialog(
      title = "Welcome to the Shark Bycatch Data Summary Dashboard",
      HTML("
        <div style='line-height: 1.6; font-size: 16px;'>
        <p>This dashboard displays catch per unit effort (CPUE) trends derived from electronic monitoring data collected by
        the Center for Fisheries Electronic Monitoring at Mote Marine Laboratory.</p>
        <p>Select species and filters from the sidebar to get started, or click 'About the Data' in the sidebar for more details.</p>
      "),
      easyClose = TRUE,
      footer = modalButton("Okay")
    ))
  })
  
  # Open modal for about the data info 
  observeEvent(input$open_about, {
    showModal(modalDialog(
      title = "About the Data",
      HTML("
      <div style='line-height: 1.6; font-size: 16px;'>
      <p>The data displayed in this app was collected through a 25% review of electronic monitoring (EM) footage from bottom longline vessels in the 
        Gulf of America reef fish fishery. This effort is part of the Center for Fisheries Electronic Monitoring at 
        Mote Marine Laboratory’s voluntary EM program.</p>
      <p>
        For more information, please visit 
        <a href='https://www.mote.org/cfemm' target='_blank' style='color: #00aae7; text-decoration: underline;'>
          mote.org/cfemm.
        </a>
      </p>
    </div>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
     
  # ---------------------------- DATA ------------------------------------------   
  
  # Map point data
  filtered_data <- reactive({
      data_joined %>% 
        filter(
          Retrieval_Year >= input$years[1] &
          Retrieval_Year <= input$years[2] &
          Common_Name %in% input$select_species &
          Shark_Length_Estimate %in% input$size &
          Retrieval_Season %in% input$seasons
        )
  })
  
  # Aggregate within grid cells
  aggregated_data <- reactive({
    filtered_data() %>%
      group_by(Unique_Retrieval) %>%
      reframe(
        GRID_ID = first(GRID_ID),
        Sharks_per_Haul = n(),
        CPUE = mean(Species_CPU_Hook_Hours_BLL1000, na.rm = TRUE),
        Depth.mean = mean(Depth, na.rm = TRUE)
      ) %>%
      group_by(GRID_ID) %>%
      reframe(
        Sharks_per_Haul_mean = mean(Sharks_per_Haul, na.rm = TRUE),
        CPUE = mean(CPUE, na.rm = TRUE),
        Depth.mean = mean(Depth.mean, na.rm = TRUE)
      )
  })
  
  # Merge the dataset with the grid shape
  gridvalues <- reactive({
    st_as_sf(merge(x = gridshp, y = aggregated_data(), by = "GRID_ID", all.x = FALSE))
  })  
  
  # ---------------------------- MAPS ------------------------------------------
  
  # base map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base"), group = "Basemap") %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference"), group = "Basemap") %>%
      setView(lng=-88.5, lat=27, zoom=6)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) %>%
      addMarkers(data = moteport,
                 popup = paste0("<strong>", moteport$Home_Port, "</strong><br>", moteport$City_State),
                 icon = mote_icon,
                 lng= ~Longitude,
                 lat= ~Latitude) %>%
      addMarkers(data=homeport,
                 popup=paste0(homeport$CITY, ", ", homeport$STATE),
                 icon=port_icon,
                 lng= ~LON,
                 lat= ~LAT)
  })

  # reactive catch events
  #observeEvent(input$update, {
  observe({
    
      
      if (nrow(gridvalues()) > 0) {
      cpue_popup <- paste0("<strong>Average CPUE: </strong>", round(gridvalues()$CPUE, digits = 2),
                           "<br><strong>Depth (m): </strong>", round(gridvalues()$Depth.mean, digits = 2))
      num_pal <- colorNumeric(palette = coral_palette(9), domain = gridvalues()$CPUE, reverse = FALSE)

      leafletProxy("map") %>%
        clearGroup("map-text") %>%
        clearGroup("data") %>%
        removeControl("legend") %>%
        
        addPolygons(data = st_zm(gridvalues()),
                  fillColor = ~num_pal(CPUE),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                  popup = cpue_popup,
                  group = "data") %>%
        addLegend(position = 'topright',
                pal = num_pal,
                values = gridvalues()$CPUE,
                opacity = 1,
                title = HTML('Average<br>Catch per 1000<br>Hook Hours<br>(CPUE)'),
                layerId = "legend")  
        
        } else {
          leafletProxy("map") %>%
            clearGroup("map-text") %>%
            clearGroup("data") %>%
            removeControl("legend") 
            
        }
     # }
    })
  
  # --------------------------- PLOTS ------------------------------------------
thematic_shiny()

# render static activity plot
  output$activityplot <- renderPlotly({
    trips_chart
  })
  
# render static species catch plot  
  output$topspeciesplot <- renderPlotly({
    species_chart
  })
  
# Creating a GAM for plotly
  gam_data <- reactive({
    req(length(input$select_species) > 0)

    top_sharks %>%
      filter(
        Common_Name %in% input$select_species
        ) %>%
      group_by(Unique_Retrieval) %>%
      summarise(
        Retrieval_Begin_Date_Time = first(Retrieval_Begin_Date_Time),
        Retrieval_End_Date_Time = first(Retrieval_End_Date_Time),
        Retrieval_Begin_Date_Time_NUM = as.numeric(Retrieval_Begin_Date_Time),
        Soak_Time = first(Soak_Time),
        Number_Caught = n(),
        CPUE = (1000 * Number_Caught) / (750 * mean(Soak_Time)),
        .groups = "drop"
      )
  })
  
# Fit the GAM model
  gam_model <- reactive({
    req(gam_data())
    mgcv::gam(CPUE ~ s(Retrieval_Begin_Date_Time_NUM, bs = "cs", k=5), data = gam_data())
  })
  
# Predict fitted values
  gam_pred <- reactive({
    req(gam_data(), gam_model())
    
    # Create a smooth sequence of time values (100 evenly spaced points)
    new_x <- seq(
      min(gam_data()$Retrieval_Begin_Date_Time_NUM, na.rm = TRUE),
      max(gam_data()$Retrieval_Begin_Date_Time_NUM, na.rm = TRUE),
      length.out = 100
    )
    
    # Predict on that smooth grid
    pred_df <- data.frame(Retrieval_Begin_Date_Time_NUM = new_x)
    pred_df$Retrieval_Begin_Date_Time <- as.POSIXct(new_x, origin = "1970-01-01", tz = "UTC")
    pred_df$y_pred <- predict(gam_model(), newdata = pred_df, type = "response")
    
    pred_df
  }) 
  
# Make a Plotly GAM visualization
  output$cpueplot <- renderPlotly({
    
    req(gam_data(), gam_pred())
    
    plot_ly() %>%
      add_markers(
        data = gam_data(),
        x = ~Retrieval_Begin_Date_Time,
        y = ~CPUE,
        marker = list(color = "#00aae7", opacity = 0.5),
        name = "Observation",
        text = ~paste(
          "<b>CPUE:</b>", round(CPUE, 1),
          "<br><b>Haul Date:</b>", paste0(format(Retrieval_Begin_Date_Time, format="%b %d, %Y"))),
        hoverinfo = "text", showlegend = TRUE
      ) %>%
      add_lines(
        data = gam_pred(),
        x = ~Retrieval_Begin_Date_Time,
        y = ~y_pred,
        line = list(color = "#0054a6", width = 2),
        name = "Smoothed CPUE",
        text = ~paste(
          "<br><b>Haul Date:</b>", paste0(format(Retrieval_Begin_Date_Time, format="%b %d, %Y"))),
        hoverinfo = "text", showlegend = TRUE
      ) %>%
      layout(
        xaxis = list(title = "", tickformat = "%b %Y", type = "date", gridcolor = "#cccccc"),
        yaxis = list(title = "Catch per 1000 Hook Hours", gridcolor = "#cccccc"),
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(orientation = "h"),
        paper_bgcolor = "rgba(0,0,0,0)",  
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c('toImage', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'select2d')
      )
    #}
  }) %>% bindCache(input$select_species)
  
  # ---------------------------- TEXT ------------------------------------------  
  
# print total observations for filtered data in ui
  output$text_obs <- renderText({
    format(nrow(filtered_data()), big.mark=",")
  })

# clarifying text
  output$gam_text <- renderText({
    if (length(input$select_species) > 0) {
      req(input$select_species)
      paste(paste(input$select_species, collapse = "; "), "Catch per 1000 Hook Hours (All Sizes)")
    } else {
      paste("Shark Catch per 1000 Hook Hours (All Sizes)")
    }
  })
  
} #end server

