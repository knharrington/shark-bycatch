################################################################################
# This script is the server for the CFEMM app
# capabilities include: viewing tables, maps, and figures

################################################################################

function(input, output, session) {

  #bs_themer()
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
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
        Gulf of America reef fish fishery (2016–2024). This effort is part of the Center for Fisheries Electronic Monitoring at 
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
     
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

  # # Condition table
  # Condition.Table <- reactive({
  #   top_sub_filtered <- top_sharks[top_sharks$Common_Name %in% input$select_species,]
  #   summary_data <- top_sub_filtered %>%
  #     group_by(Condition_On_Arrival) %>%
  #     rename(`Condition On Arrival` = Condition_On_Arrival) %>%
  #     summarise(`Number Caught` = n()) %>%
  #     mutate(`% of Catch` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
  #     adorn_totals("row") %>%
  #     mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
  #            `% of Catch` = ifelse(as.numeric(`% of Catch`) >= 99.8, "100.00", sprintf("%.2f", `% of Catch`)))
  #   as.data.frame(summary_data)
  # }) %>% bindCache(input$select_species)
  # 
  # # Fate table
  # Fate.Table <- reactive({
  #   top_sub_filtered <- top_sharks[top_sharks$Common_Name %in% input$select_species,]
  #   summary_data <- top_sub_filtered %>%
  #     group_by(Catch_Fate) %>%
  #     rename(`Catch Fate` = Catch_Fate) %>%
  #     summarise(`Number Caught` = n()) %>%
  #     mutate(`% of Catch` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
  #     adorn_totals("row") %>%
  #     mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
  #            `% of Catch` = ifelse(as.numeric(`% of Catch`) >= 99.8, "100.00", sprintf("%.2f", `% of Catch`)))
  #   as.data.frame(summary_data)
  # }) %>% bindCache(input$select_species)
  
  # render the data tables 
  output$topspeciestable <- DT::renderDataTable(All.Species) 

  # output$coatable <- renderTable(Condition.Table(), rownames = FALSE, striped = TRUE)
  
  # output$fatetable <- renderTable(Fate.Table(), rownames = FALSE, striped = TRUE)
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
  
  # Map point data
  filtered_data <- reactive({
      top_sharks %>% 
        filter(
          Retrieval_Year >= input$years[1] &
          Retrieval_Year <= input$years[2] &
          Common_Name %in% input$select_species &
          Shark_Length_Estimate %in% input$size &
          Retrieval_Season %in% input$seasons
        )
  })
  
  # Convert the data frame to a spatial object
  filtered_data_sf <- reactive({
      st_as_sf(filtered_data(), coords = c("Centroid_Longitude", "Centroid_Latitude"), crs = st_crs(gridshp))
    })
  
  # Perform the spatial join
  grid_join <- reactive({
      setDT(st_join(filtered_data_sf(), gridshp, join = st_intersects))
    })
  
  # Aggregate within grid cells
  aggregated_data <- reactive({
    grid_join() %>%
      group_by(GRID_ID) %>%
      reframe(
        CPUE = mean(Species_CPU_Hook_Hours_BLL1000, na.rm = TRUE),
        Depth.mean = mean(Depth, na.rm = TRUE)
      ) 
  })
  
  # Merge the dataset with the grid shape
  gridvalues <- reactive({
    st_as_sf(merge(x = gridshp, y = aggregated_data(), by = "GRID_ID", all.x = FALSE))
  })  
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  # base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base"), group = "Basemap") %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference"), group = "Basemap") %>%
      setView(lng=-88.5, lat=27, zoom=6)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) 
  })

  # reactive catch events
  observeEvent(input$update, {
    
    # if (length(input$seasons) < 1 | length(input$select_species) < 1 | length(input$size) < 1) {
    #   
    #   show_alert("Please ensure at least one species, size, and season is selected.", type="error", showCloseButton=TRUE)
    # 
    # } else {
      
      if (nrow(gridvalues()) > 0) {
      cpue_popup <- paste0("<strong>CPUE: </strong>", round(gridvalues()$CPUE, digits = 2),
                           "<br><strong>Depth (m): </strong>", round(gridvalues()$Depth.mean, digits = 2))
      num_pal <- colorNumeric(palette = coral_palette(9), domain = gridvalues()$CPUE, reverse = FALSE)

      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        
        addMarkers(data = moteport,
                 popup = paste0("<strong>", moteport$Home_Port, "</strong><br>", moteport$City_State),
                 icon = mote_icon,
                 lng= ~Longitude,
                 lat= ~Latitude) %>%
        addMarkers(data=homeport,
                 popup=paste0(homeport$CITY, ", ", homeport$STATE),
                 icon=port_icon,
                 lng= ~LON,
                 lat= ~LAT) %>%
        
        addPolygons(data = st_zm(gridvalues()),
                  fillColor = ~num_pal(CPUE),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                  popup = cpue_popup) %>%
        addLegend(position = 'topright',
                pal = num_pal,
                values = gridvalues()$CPUE,
                opacity = 1,
                title = HTML('Catch per 1000<br>Hook Hours'))  %>%
        addSimpleGraticule(interval = 1, group = "Graticule") %>%
        addLayersControl(
          overlayGroups = c("Graticule"),
          position ="topright",
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Graticule")
        
        } else {
          leafletProxy("map") %>%
            clearShapes() %>%
            clearControls() %>%
            
            addMarkers(data = moteport,
                       popup = paste0("<strong>", moteport$Home_Port, "</strong><br>", moteport$City_State),
                       icon = mote_icon,
                       lng= ~Longitude,
                       lat= ~Latitude) %>%
            addMarkers(data=homeport,
                       popup=paste0(homeport$CITY, ", ", homeport$STATE),
                       icon=port_icon,
                       lng= ~LON,
                       lat= ~LAT) %>%
            addSimpleGraticule(interval = 1, group = "Graticule") %>%
            addLayersControl(
              overlayGroups = c("Graticule"),
              position ="topright",
              options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup("Graticule")
        }
     # }
    })
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
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
      filter(Common_Name %in% input$select_species) %>%
      mutate(
        Retrieval_Begin_Date_Time_NUM = as.numeric(Retrieval_Begin_Date_Time)
      )
  })
  
# Fit the GAM model
  gam_model <- reactive({
    req(gam_data())
    mgcv::gam(Species_CPU_Hook_Hours_BLL1000 ~ s(Retrieval_Begin_Date_Time_NUM, bs = "cs", k=5), data = gam_data())
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
    
    # if (length(input$select_species) < 1) {
    #   
    #   show_alert("Please ensure at least one species is selected.", type="error", showCloseButton=TRUE)
    #   
    # } else {
    
    req(gam_data(), gam_pred())
    
    plot_ly() %>%
      add_markers(
        data = gam_data(),
        x = ~Retrieval_Begin_Date_Time,
        y = ~Species_CPU_Hook_Hours_BLL1000,
        marker = list(color = "#00aae7", opacity = 0.5),
        name = "Observation"
      ) %>%
      add_lines(
        data = gam_pred(),
        x = ~Retrieval_Begin_Date_Time,
        y = ~y_pred,
        line = list(color = "#0054a6", width = 2),
        name = "Smoothed CPUE"
      ) %>%
      layout(
        xaxis = list(title = "", tickformat = "%b %Y", type = "date", gridcolor = "#cccccc"),
        yaxis = list(title = "Catch per 1000 Hook Hours", gridcolor = "#cccccc"),
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(orientation = "h"),
        paper_bgcolor = "rgba(0,0,0,0)",  
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    #}
  }) %>% bindCache(input$select_species)
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
  
# print total observations for filtered data in ui
  output$text_obs <- renderText({
    format(nrow(filtered_data()), big.mark=",")
  })

# clarifying text
  output$gam_text <- renderText({
    req(input$select_species)
    paste(paste(input$select_species, collapse = "; "), "CPUE (All Sizes)")
  })
  
  # output$coa_text <- renderText({
  #   req(input$select_species)
  #   paste(paste(input$select_species, collapse = "; "), "Condition on Arrival")
  # })
  # 
  # output$fate_text <- renderText({
  #   req(input$select_species)
  #   paste(paste(input$select_species, collapse = "; "), "Fate")
  # })
  
} #end server

