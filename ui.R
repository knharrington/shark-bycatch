################################################################################
# This script builds the user interface for the CFEMM app
# capabilities include: viewing tables, maps, and figures
################################################################################

my_theme <- bs_theme(
  version = 5, 
  primary = "#0054a6"
)

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Map"),
    card_body(leafletOutput("map")),
    height = "800px"
  ), # map card
  card(
    full_screen = TRUE,
    card_header("Trips, Hauls, and Sea Days by Year"),
    card_body(plotlyOutput("activityplot"))
  ), # line graph card
  card(
    full_screen = TRUE,
    card_header("Top Species Catch Events by Year"),
    card_body(plotlyOutput("topspeciesplot"))
  ), # bar graph card
  card(
    full_screen = TRUE,
    card_header(textOutput("gam_text")),
    card_body(
      withSpinner(plotlyOutput("cpueplot", height = "400px"), type = 4, color = "#00aae7")
    ),
    height = "800px"
  ), # gam graph card
  card(
    full_screen = TRUE,
    card_header("All Shark Species Caught"),
    card_body(
      withSpinner(DT::dataTableOutput("topspeciestable"), type = 4, color = "#00aae7")
    )
  ), # all species table
  card(
    full_screen = TRUE,
    card_header(textOutput("coa_text")),
    card_body(
      withSpinner(tableOutput("coatable"), type = 4, color = "#00aae7")
    )
  ), # coa table
  card(
    full_screen = TRUE,
    card_header(textOutput("fate_text")),
    card_body(
      withSpinner(tableOutput("fatetable"), type = 4, color = "#00aae7")
    )
  ) # fate table
)


page_sidebar(title = div(img(src="MoteLogomark.svg", style = "width:30px;height:30px")," Bycatch Data Summary", style = "text-align: center"), 
             theme = my_theme,                
             sidebar = sidebar(width=400,
                               helpText("Use the following selections to update the data displayed on the map."),
                               selectInput("select_species", label="Select Species (top 15 available)", choices = sort(unique(top_sharks$Common_Name)), selected = "Sandbar Shark",
                                           selectize = TRUE, multiple=TRUE),
                               sliderInput("years", datetext, min(top_sharks$Retrieval_Year), max(top_sharks$Retrieval_Year),
                                           value = c(min(top_sharks$Retrieval_Year), max(top_sharks$Retrieval_Year)), sep = "", round = TRUE, step = 1),
                               checkboxGroupInput("seasons", "Season", 
                                                  choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
                                                                     HTML("<b>Spring</b> (Apr, May, Jun)"), 
                                                                     HTML("<b>Summer</b> (Jul, Aug, Sep)"), 
                                                                     HTML("<b>Fall</b> (Oct, Nov, Dec)")), 
                                                  selected = list("Winter","Spring", "Summer", "Fall"),
                                                  choiceValues = list("Winter","Spring", "Summer", "Fall")),
                               actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;"),
                               hr(),
                               actionLink("open_about", "About the Data", icon = icon("info-circle")),
                               tags$a(
                                 icon("github"), " GitHub",
                                 href = "https://github.com/knharrington/shark-bycatch",
                                 target = "_blank",
                                 style = "display:block; margin-top:5px;"
                               )
             ), #sidebar
             
            
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


################################################################################
# OLD UI COMPONENTS & SCRAPS

           
           # tags$head(tags$style(HTML(
           #   '.navbar-static-top {background-color: #023f88;}',
           #   '.navbar-default .navbar-nav>.active>a {background-color: #023f88;}',
           #   '.btn {background-color: #023f88; border-color: #023f88;}',
           #   '.nav-tabs > li > a {color: #00aae7;}',
           #   'a {color: #00aae7;}',
           #   'a:hover {color: #00aae7;}',
           #   '.help-block {color: #63666a;}'
           # ))),


                          # tabPanel("User Guide",
                          #          h3("General Use"),
                          #          p("This app displays shark bycatch data collected via video review from electronic monitoring systems
                          #            onboard commercial fishing vessels in the Gulf of Mexico. Data is displayed in three formats located
                          #            on three tabs: an interactive map, figures depicting effort, and tables describing catch disposition.
                          #            In the panel on the left (if viewing on a desktop) there are inputs that can be used to filter the
                          #            data presented in the three tabs. You can filter the data based on species (top 10 caught available),
                          #            range in years the catch occurred, and season the catch occurred. You can view multiple species and their 
                          #            combined data by selecting more than one from the dropdown menu. Remove species by clicking on them in the input bar 
                          #            and pressing your 'Delete' key or by pressing 'Backspace'. Once your input selections have been
                          #            made, click the 'Update' button to see the data change. The 'Total Observations' counter will also update
                          #            to display how many records are associated with the input filters. Please note that all data reflects
                          #            approximately 25% of fishing activities."),
                          #          p(" "),
                          #          h4("Map"),
                          #          p("The interactive map displays the data filtered by the inputs in a gridded format (10 min by 10 min) depicting
                          #            catch per 1000 hook hours. In longline fisheries, effort is usually determined in 'hook-hours' where the number of
                          #            hooks multiplied by the amount of time the hooks were in the water can be used to estimate effort. This metric
                          #            is also called catch-per-unit-effort (CPUE). In general, the darker the grid cell on the map, the more catch has 
                          #            been recorded there."),
                          #          h4("Figures"),
                          #          p("Three figures depict effort. The first two figures depicting trip information and top catch events do not change 
                          #            when the filters are updated. The bottom figure depicting catch-per-unit-effort over time changes based on the 
                          #            species selected. To update the figure, select a new species from the dropdown menu and click the 'Update' button. 
                          #            The lines are color-coded to denote whether the catch was discarded or retained, and the gray halo surrounding the 
                          #            line represents the confidence interval."),
                          #          h4("Tables"),
                          #          p("The tables depict catch disposition. The first table does not change when the filters are updated and shows 
                          #            the proportion of species caught to the total catch. The bottom tables change when new species are selected 
                          #            and the 'Update' button is clicked."),
                          #          h4("Feedback"),
                          #          HTML("<p>Please report any issues, feedback, or requests by submitting a 
                          #               <a href='https://github.com/knharrington/shark-bycatch'>GitHub issue</a> 
                          #               or emailing <a href='mailto:knharrington@mote.org'>knharrington@mote.org</a>.</p>")


