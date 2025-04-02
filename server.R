################################################################################
# This script is the server for the CFEMM app
# capabilities include: viewing tables, maps, and figures

################################################################################

function(input, output, session) {
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Condition table
  Condition.Table <- reactive({
    top_sub_filtered <- top.sub[top.sub$Common_Name %in% input$select_species,]
    summary_data <- top_sub_filtered %>%
      group_by(Condition_On_Arrival) %>%
      rename(`Condition On Arrival` = Condition_On_Arrival) %>%
      summarise(`Number Caught` = n()) %>%
      mutate(`% of Catch` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
      adorn_totals("row") %>%
      mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
             `% of Catch` = ifelse(as.numeric(`% of Catch`) >= 99.8, "100.00", sprintf("%.2f", `% of Catch`)))
    as.data.frame(summary_data)
  }) %>% bindCache(input$select_species)

  # Fate table
  Fate.Table <- reactive({
    top_sub_filtered <- top.sub[top.sub$Common_Name %in% input$select_species,]
    summary_data <- top_sub_filtered %>%
      group_by(Catch_Fate) %>%
      rename(`Catch Fate` = Catch_Fate) %>%
      summarise(`Number Caught` = n()) %>%
      mutate(`% of Catch` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
      adorn_totals("row") %>%
      mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
             `% of Catch` = ifelse(as.numeric(`% of Catch`) >= 99.8, "100.00", sprintf("%.2f", `% of Catch`)))
    as.data.frame(summary_data)
  }) %>% bindCache(input$select_species)
  
  # render the data tables 
  output$topspeciestable <- DT::renderDataTable(All.Species) 

  output$coatable <- renderTable(Condition.Table(), rownames = FALSE, striped = TRUE)
  
  output$fatetable <- renderTable(Fate.Table(), rownames = FALSE, striped = TRUE)
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
  
  # Map point data
  filtered_data <- reactive({
      top_sharks %>% 
        filter(
          Retrieval_Year >= input$years[1] &
          Retrieval_Year <= input$years[2] &
          Common_Name %in% input$select_species &
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
    
    if (length(input$seasons) < 1 | length(input$select_species) < 1) {
      
      show_alert("Please ensure at least one species and season is selected.", type="error", showCloseButton=TRUE)
    
    } else {
      
      cpue_popup <- paste0("<strong>CPUE: </strong>", round(gridvalues()$CPUE, digits = 2),
                           "<br><strong>Depth (m): </strong>", round(gridvalues()$Depth.mean, digits = 2))
      num_pal <- colorNumeric(palette = "Reds", domain = gridvalues()$CPUE, reverse = FALSE)

      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addSimpleGraticule(interval = 1, group = "Graticule") %>%
        addLayersControl(
          overlayGroups = c("Graticule"),
          position ="topright",
          options = layersControlOptions(collapsed = FALSE)) %>%
        
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
                title = HTML('Catch per 1000<br>Hook Hours'))  
      }
    })
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# render static plots
  output$activityplot <- renderPlotly({
    trips_chart
  })
  
  output$topspeciesplot <- renderPlotly({
    species_chart
  })
  
# create reactive gam for cpue over time
  output$cpueplot <- renderPlot({
    
    if (length(input$select_species) < 1) {
      
      show_alert("Please ensure at least one species is selected.", type="error", showCloseButton=TRUE)
      
    } else {
    
    gam_data <- top.sub[Common_Name %in% input$select_species]
    ggplot(gam_data, aes(x = Retrieval_Begin_Date_Time, y = Species_CPU_Hook_Hours_BLL1000)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), linewidth = 1.25, colour = "#0054a6") + 
      labs(x = " ",
           y = "Catch per 1000 Hook Hours") +
      scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20),
            axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
            axis.title.x = element_text(size = 18, vjust = 0.5),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 18))
    }
  }) %>% bindCache(input$select_species)
 

# print total observations for filtered data in ui
  output$text_obs <- renderText({
    format(nrow(filtered_data()), big.mark=",")
  })

# clarifying text
  output$gam_text <- renderText({
    req(input$select_species)
    paste(paste(input$select_species, collapse = "; "), "CPUE")
  })
  
  output$coa_text <- renderText({
    req(input$select_species)
    paste(paste(input$select_species, collapse = "; "), "Condition on Arrival")
  })
  
  output$fate_text <- renderText({
    req(input$select_species)
    paste(paste(input$select_species, collapse = "; "), "Fate")
  })
  
} #end server

