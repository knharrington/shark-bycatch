################################################################################
# This script is the server for the CFEMM app
# capabilities include: viewing tables, maps, and figures

# TO DO
# custom css
# add general stats to the about page
################################################################################
#remove(list = ls())

function(input, output, session) {
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # observe({
  #   # periodically collect
  #   invalidateLater(1000,session)
  #   cleanMem()
  # }) 
  
  # Tables
  
  Condition.Table <- reactive({
    top_sub_filtered <- top.sub[top.sub$Common_Name == input$select_species,]
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
    top_sub_filtered <- top.sub[top.sub$Common_Name == input$select_species,]
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
    filtered <- top.sub[Common_Name == input$select_species &
             Retrieval_Year >= input$years[1] &
             Retrieval_Year <= input$years[2] &
             Retrieval_Season %in% input$seasons, ]
    as_tibble(filtered)
  })
  
  # Convert Data.In to an sf object
  species.sub.SHEs_sf <- st_as_sf(species.sub.SHEs, coords = c("Catch_Longitude", "Catch_Latitude"), crs = st_crs(gridshp))
  
  # Perform spatial join and filtering
  pro_grid <- as.data.frame(st_join(species.sub.SHEs_sf, gridshp, join = st_intersects))
  
  filtered_data_cpue <- reactive({
    setDT(pro_grid)  # Convert to data.table
    
    pro_grid[!is.na(Species_CPU_Hook_Hours_BLL1000) &
               #!is.na(Proportion_Retained),
               !is.na(Depth) &
               Trip_Type == "Longline" &
               Retrieval_Year >= input$years[1] &
               Retrieval_Year <= input$years[2] &
               Common_Name == input$select_species &
               Retrieval_Season %in% input$seasons,
             .(Species_CPU_Hook_Hours_BLL1000.mean = mean(Species_CPU_Hook_Hours_BLL1000),
               #Proportion_Retained.mean = mean(Proportion_Retained),
               Depth.mean = mean(Depth)),
             by = GRID_ID][, .(GRID_ID, Species_CPU_Hook_Hours_BLL1000.mean, Depth.mean)] %>%
      unique() %>%
      na.omit()
  })
  
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
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = TRUE, updateWhenIdle = FALSE)) #%>%
      #addMiniMap(position = 'bottomleft') %>%
      #hideGroup(c("Catch Events", "Seasonal Closure")) %>%
      #leafem::addMouseCoordinates()
  })

  # reactive catch events
  observe({
    
    if (length(filtered_data_cpue()$Species_CPU_Hook_Hours_BLL1000) >= 1 & length(input$seasons) >=1) {

      cpue_popup <- paste0("<strong>CPUE: </strong>", round(gridvalues()$CPUE, digits = 2),
                           #"<br><strong>Proportion Retained: </strong>", round(gridvalues()$Proportion_Retained.mean, digits = 2),
                           "<br><strong>Depth (m): </strong>", round(gridvalues()$Depth.mean, digits = 2))
      # # cpue color palette
      qpal <- colorNumeric(palette = "Reds", domain = gridvalues()$CPUE, reverse = FALSE)

  # point shapes
    leafletProxy("mainmap") %>%
      clearShapes() %>%
      #clearImages() %>%
      clearControls() %>%
      addSimpleGraticule(interval = 1, group = "Graticule") %>%
      addMarkers(data=homeport,
                 popup=paste0("<strong>", homeport$Home_Port, "</strong><br>", homeport$City_State),
                 icon=port_icon,
                 lng= ~Longitude,
                 lat= ~Latitude) %>%
      # addPolygons(data=st_zm(gearrest),
      #             color="black",
      #             fillOpacity=0.25,
      #             group="Gear Restrictions") %>%
      # addPolygons(data=st_zm(seasonalclose),
      #             color="blue",
      #             fillOpacity=0.25,
      #             group="Seasonal Closure") %>%
    # addRasterImage(kdraster(),
    #                colors = pal_heat(),
    #                opacity = .75,
    #                group = "Catch Density") %>%
      addPolygons(data=st_zm(gridvalues()),
                  fillColor = ~qpal(CPUE),
                  weight = 0.5,
                  color = "black",
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                  popup = cpue_popup) %>%
                  #group="CPUE Grid") %>%
      # addCircles(data=filtered_data(),
      #            lng = ~Catch_Longitude,
      #            lat = ~Catch_Latitude,
      #            radius = 6,
      #            color = "black",
      #            group="Catch Events") %>%
      addLegend(position = 'topright',
                pal = qpal,
                values = gridvalues()$CPUE,
                opacity = 1,
                title = HTML('Catch per 1000<br>Hook Hours')) %>%
                #group="CPUE Grid") %>%
      # addLegend(pal = pal_heat(),
      #           values = heat_values(),
      #           opacity = 1,
      #           title = "Density of<br>Catch Events",
      #           group = "Catch Density") %>%
      addLayersControl(
        overlayGroups = c("Graticule"),
        position ="topright",
        options = layersControlOptions(collapsed = FALSE)) #%>%
      #addControl(html = '<div id="combined-legend"></div>', position = "topright")
      #hideGroup(c("Catch Events", "Seasonal Closure"))
    
    } else {
      leafletProxy("mainmap") %>%
        clearShapes() %>%
        clearControls()
    }

    })
  
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


  output$activityplot <- renderPlot({
    summarylinechart
  })
  
  output$topspeciesplot <- renderPlot({
    topspeciesbar
  })
  
  output$cpueplot <- renderPlot({
    # Filter the data directly in data.table
    filtered_data <- top.sub[Common_Name == input$select_species]
    
    ggplot(filtered_data, aes(x = Retrieval_Begin_Date_Time, y = Species_CPU_Hook_Hours_BLL1000)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), linewidth = 1.25, colour = "#0d1687") + 
      labs(title = paste0(input$select_species, " CPUE"),
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
  

 
##### PRINT NUMBER OF OBSERVATIONS DISPLAYED #####
 output$text_obs <- renderText({
 format(nrow(filtered_data()), big.mark=",")
 })

  # output$text_sp <- renderUI({
  #   str1 <- paste0("Showing results for: ")
  #   str2 <- paste0(input$select_species)
  #   HTML(paste(strong(str1), str2))
  # })
  
  output$text_sp <- renderText({
    paste0(input$select_species)
  })
  
} #end server

