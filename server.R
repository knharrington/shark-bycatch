################################################################################
# This script is the server for the CFEMM app
# capabilities include: viewing tables, maps, and figures

################################################################################

function(input, output, session) {
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Tables
  
  Condition.Table <- reactive({
    top_sub_filtered <- top.sub[top.sub$Common_Name %in% input$select_species,]
    summary_data <- top_sub_filtered %>%
      dplyr::group_by(Condition_On_Arrival) %>%
      dplyr::rename(`Condition On Arrival` = Condition_On_Arrival) %>%
      dplyr::summarise(`Number Caught` = n()) %>%
      mutate(`% of Catch` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
      adorn_totals("row") %>%
      mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
             `% of Catch` = ifelse(as.numeric(`% of Catch`) >= 99.8, "100.00", sprintf("%.2f", `% of Catch`)))
    as.data.frame(summary_data)
  }) %>% bindCache(input$select_species)

  Fate.Table <- reactive({
    top_sub_filtered <- top.sub[top.sub$Common_Name %in% input$select_species,]
    summary_data <- top_sub_filtered %>%
      dplyr::group_by(Catch_Fate) %>%
      dplyr::rename(`Catch Fate` = Catch_Fate) %>%
      dplyr::summarise(`Number Caught` = n()) %>%
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
    setDT(top.sub)
    filtered <- top.sub[Common_Name %in% input$select_species &
             Retrieval_Year >= input$years[1] &
             Retrieval_Year <= input$years[2] &
             Retrieval_Season %in% input$seasons, ]
    as_tibble(filtered)
  })
  
  filtered_data_cpue <- reactive({

    pro_grid[!is.na(Species_CPU_Hook_Hours_BLL1000) &
               !is.na(Depth) &
               Trip_Type == "Longline" &
               Retrieval_Year >= input$years[1] &
               Retrieval_Year <= input$years[2] &
               Common_Name %in% input$select_species &
               Retrieval_Season %in% input$seasons,
             .(Species_CPU_Hook_Hours_BLL1000.mean = mean(Species_CPU_Hook_Hours_BLL1000),
               Depth.mean = mean(Depth)),
             by = GRID_ID][, .(GRID_ID, Species_CPU_Hook_Hours_BLL1000.mean, Depth.mean)] %>%
      unique() %>%
      na.omit()
  })
  
  # merge the dataset with the grid shape
  gridvalues <- reactive({
    if (nrow(filtered_data_cpue()) >= 1) {
      st_as_sf(sp::merge(x = gridshp, y = filtered_data_cpue(), by = "GRID_ID", all.x = FALSE)) %>%
        dplyr::rename(c("CPUE" = "Species_CPU_Hook_Hours_BLL1000.mean"))
    }
  })   
  
###############  
    # # trouble-shooting data table
    # troubledata <- reactive({
    #     filtered_data_cpue()
    # })
    # 
    # output$trouble <- renderTable({troubledata()})
###############     

  #base map: catch map
  output$mainmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base"), group = "Basemap") %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference"), group = "Basemap") %>%
      setView(lng=-88.5, lat=27, zoom=6)  %>%
      addScaleBar(position = 'topleft',
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) 
  })

  # reactive catch events
  observe({
    
    if (length(filtered_data_cpue()$Species_CPU_Hook_Hours_BLL1000) >= 1 & length(input$seasons) >=1 & length(input$select_species) >=1 ) {

      cpue_popup <- paste0("<strong>CPUE: </strong>", round(gridvalues()$CPUE, digits = 2),
                           "<br><strong>Depth (m): </strong>", round(gridvalues()$Depth.mean, digits = 2))
      # cpue color palette
      qpal <- colorNumeric(palette = "Reds", domain = gridvalues()$CPUE, reverse = FALSE)

  # create gridded map
    leafletProxy("mainmap") %>%
      clearShapes() %>%
      #clearImages() %>%
      clearControls() %>%
      addSimpleGraticule(interval = 1, group = "Graticule") %>%
      addMarkers(data=moteport,
                 popup=paste0("<strong>", moteport$Home_Port, "</strong><br>", moteport$City_State),
                 icon=mote_icon,
                 lng= ~Longitude,
                 lat= ~Latitude) %>%
      addMarkers(data=homeport,
                 popup=paste0(homeport$CITY, ", ", homeport$STATE),
                 icon=port_icon,
                 lng= ~LON,
                 lat= ~LAT) %>%
      addPolygons(data=st_zm(gridvalues()),
                  fillColor = ~qpal(CPUE),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                  popup = cpue_popup) %>%
      addLegend(position = 'topright',
                pal = qpal,
                values = gridvalues()$CPUE,
                opacity = 1,
                title = HTML('Catch per 1000<br>Hook Hours')) %>%
      addLayersControl(
        overlayGroups = c("Graticule"),
        position ="topright",
        options = layersControlOptions(collapsed = FALSE)) 
    
    } else {
      leafletProxy("mainmap") %>%
        clearShapes() %>%
        clearControls() %>%
        addSimpleGraticule(interval = 1, group = "Graticule")
    }

    })
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# render static plots
  output$activityplot <- renderPlot({
    summarylinechart
  })
  
  output$topspeciesplot <- renderPlot({
    topspeciesbar
  })
  
# create reactive gam for cpue over time
  output$cpueplot <- renderPlot({

    filtered_data <- top.sub[Common_Name %in% input$select_species]
    
    ggplot(filtered_data, aes(x = Retrieval_Begin_Date_Time, y = Species_CPU_Hook_Hours_BLL1000)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), linewidth = 1.25, colour = "#0054a6") + 
      labs(#title = paste0(input$select_species, " CPUE"),
           x = " ",
           y = "Catch per 1000 Hook Hours") +
      scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20),
            axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
            axis.title.x = element_text(size = 18, vjust = 0.5),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 18))
  }) %>% bindCache(input$select_species)
  

 
# print total observations for filtered data in ui
 output$text_obs <- renderText({
 format(nrow(filtered_data()), big.mark=",")
 })

  # output$text_sp <- renderUI({
  #   str1 <- paste0("Showing results for: ")
  #   str2 <- paste0(input$select_species)
  #   HTML(paste(strong(str1), str2))
  # })


  output$text_sp1 <- renderText({
    paste0(input$select_species, collapse = "; ")
  })
  
  output$text_sp2 <- renderText({
    paste0(input$select_species, collapse = "; ")
  })
  
} #end server

