################################################################################
# This script builds the user interface for the CFEMM app
# capabilities include: viewing tables, maps, and figures
################################################################################

theme <- bs_theme(
  version = 5,
  bg = "#f4f4f4",
  fg = "#000000",
  primary = "#0054a6",
  secondary = "#00aae7"#,
  #base_font = font_google("Roboto Condensed"),
  #heading_font = font_google("Roboto Condensed")
)

cards <- list(
  card(
    full_screen = TRUE,
    height = "800px",
    card_header("Map"),
    card_body(leafletOutput("map", height = "100%"),
              style = "padding: 0; height: 100%;")
  ), # 1 map card
  card(
    full_screen = TRUE,
    card_header("Trips, Hauls, and Sea Days by Year"),
    card_body(plotlyOutput("activityplot"))
  ), # 2 line graph card
  card(
    full_screen = TRUE,
    card_header("Top Species Catch Events by Year"),
    card_body(plotlyOutput("topspeciesplot"))
  ), # 3 bar graph card
  card(
    full_screen = TRUE,
    #height = "800px",
    card_header(textOutput("gam_text")),
    card_body(
      plotlyOutput("cpueplot")
    )
  )#, # 4 gam graph card
  # card(
  #   full_screen = TRUE,
  #   card_header("All Shark Species Caught"),
  #   card_body(
  #     withSpinner(DT::dataTableOutput("topspeciestable"), type = 4, color = "#00aae7")
  #   )
  # ), # 5 all species table
  # card(
  #   full_screen = TRUE,
  #   card_header(textOutput("coa_text")),
  #   card_body(
  #     withSpinner(tableOutput("coatable"), type = 4, color = "#00aae7")
  #   )
  # ), # 6 coa table
  # card(
  #   full_screen = TRUE,
  #   card_header(textOutput("fate_text")),
  #   card_body(
  #     withSpinner(tableOutput("fatetable"), type = 4, color = "#00aae7")
  #   )
  # ) # 7 fate table
)

tagList(
  # Custom full-width header bar
  tags$div(
    style = "background-color: #0054a6;color: white; padding: 10px 20px;
      display: flex; align-items: center; justify-content: space-between;",
    
    # Left side: logo and title
    tags$div(
      style = "display: flex; align-items: center;",
      img(src = "MoteMarineLaboratory_StackedLogo_White.png", style = "height: 30px; margin-right: 15px;"),
      tags$h4("Shark Bycatch Data Summary", style = "margin: 0;")
    ),
    
    # Right side: actionLink and GitHub link
    tags$div(
      style = "display: flex; align-items: center; gap: 15px;",
      actionLink("open_about", label = HTML('<i class="fas fa-info-circle"></i> About the Data'), style = "color: white; text-decoration: none;"),
      tags$a(
        HTML('<i class="fab fa-github"></i> GitHub'),
        href = "https://github.com/knharrington/shark-bycatch",
        target = "_blank",
        style = "color: white; text-decoration: none;"
      )
    )
  ),
  
page_sidebar(
             theme = theme,                
             sidebar = sidebar(width=400,
                               helpText("Use the following selections to update the data displayed on the map and in the CPUE plot."),
                               pickerInput("select_species", label="Select Species (top 15 available)", choices = sort(unique(top_sharks$Common_Name)), 
                                           selected = "Sandbar Shark",
                                           #selectize = TRUE, 
                                           multiple=TRUE,
                                           options = pickerOptions(container = "body", liveSearch = TRUE, actionsBox = TRUE,
                                                                   style = "btn-outline-primary",
                                                                   selectedTextFormat = "count > 2", countSelectedText = "{0} species selected"), 
                                           width = "100%"),
                               checkboxGroupInput("size", "Shark Length Estimate", 
                                                  choiceNames = list(HTML("<b>Large</b> (Greater than 2m)"),
                                                                     HTML("<b>Medium</b> (Between 1m and 2m)"), 
                                                                     HTML("<b>Small</b> (Less than 1m)")), 
                                                  selected = list("Large (Greater than 2m)", "Medium (Between 1m and 2m)", "Small (Less than 1m)"),
                                                  choiceValues = list("Large (Greater than 2m)", "Medium (Between 1m and 2m)", "Small (Less than 1m)")),
                               sliderInput("years", datetext, min(top_sharks$Retrieval_Year), max(top_sharks$Retrieval_Year),
                                           value = c(min(top_sharks$Retrieval_Year), max(top_sharks$Retrieval_Year)), sep = "", round = TRUE, step = 1),
                               checkboxGroupInput("seasons", "Season", 
                                                  choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
                                                                     HTML("<b>Spring</b> (Apr, May, Jun)"), 
                                                                     HTML("<b>Summer</b> (Jul, Aug, Sep)"), 
                                                                     HTML("<b>Fall</b> (Oct, Nov, Dec)")), 
                                                  selected = list("Winter","Spring", "Summer", "Fall"),
                                                  choiceValues = list("Winter","Spring", "Summer", "Fall")),
                               actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;")#,
                               # hr(),
                               # actionLink("open_about", " About the Data", icon = icon("info-circle")),
                               # tags$a(
                               #   icon("github"), " GitHub",
                               #   href = "https://github.com/knharrington/shark-bycatch",
                               #   target = "_blank",
                               #   style = "display:block; margin-top:5px;"
                               # )
             ), # end sidebar
             
             # Row 1: Value box + 2 small cards
             layout_columns(col_widths = c(2,5,5), 
                                value_box(
                                  title = "Total Observations", 
                                  value = textOutput("text_obs"), 
                                  showcase = bsicons::bs_icon("binoculars"),
                                  theme = "text-primary"
                                ),
                                cards[[2]],  
                                cards[[3]]   
             ),
             
             # Row 2: Map and GAM - 2/3 width card + 1/3 width card
             layout_columns(col_widths = c(8, 4), 
                                cards[[1]],  
                                cards[[4]]   
             )#,
             
             # # Row 3: Tables - 3 equal cards
             # layout_columns(col_widths = c(4, 4, 4), 
             #                    cards[[5]],  
             #                    cards[[6]],  
             #                    cards[[7]]   
             # )
             
) # sidebar page
)