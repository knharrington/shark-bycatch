################################################################################
# This script builds the user interface for the CFEMM app
# capabilities include: viewing tables, maps, and figures
################################################################################

# page appearance

navbarPage(title=div(img(src="mote-logo.png", style = "width:30px;height:27px")," Bycatch Data Summary", style = "text-align: center"), theme = shinytheme("flatly"), 
           
           tags$head(tags$style(HTML(
             '.navbar-static-top {background-color: #023f88;}',
             '.navbar-default .navbar-nav>.active>a {background-color: #023f88;}',
             '.btn {background-color: #023f88; border-color: #023f88;}',
             '.nav-tabs > li > a {color: #00aae7;}',
             'a {color: #00aae7;}',
             'a:hover {color: #00aae7;}',
             '.help-block {color: #63666a;}'
           ))),
           
           tabPanel(title=div("Sharks"),
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(h4("About This App"),
                                   HTML("<p>The data displayed in this app was collected via 25% review of electronic monitoring (EM) footage from bottom longline vessels
                                      in the Gulf of Mexico reef fish fishery (2016-2024) as a part of the Center for Fisheries Electronic Monitoring at Mote Marine Laboratory's
                                      voluntary EM program. For more information, please visit 
                                      <a href='https://www.mote.org/cfemm'>mote.org/cfemm</a>.</p>"),
                                   hr(style = "border-top: 1px solid #000000;"),
                                   helpText("Use the following selections to update the data displayed."),
                                   strong("Total Observations"),
                                   verbatimTextOutput("text_obs", placeholder=FALSE),
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
                                   actionButton("update", "Update")
                      ), #sidebarPanel
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Map",
                                   withLoader(leafletOutput("map", height = "85vh"), type="html", loader="loader4")
                                   ),
                          tabPanel("Figures",
                                   fluidRow(
                                     column(
                                       h4("Trips, Sea Days, and Hauls by Year"),
                                       withLoader(plotlyOutput("activityplot"), type="html", loader="loader4"), width=6),
                                     column(
                                       h4("Top Species Catch Events by Year"),
                                       withLoader(plotlyOutput("topspeciesplot"), type="html", loader="loader4"), width=6)
                                   ),
                                   br(),
                                   h4(textOutput("gam_text")),
                                   withLoader(plotOutput("cpueplot"), type="html", loader="loader4"),
                                   br()
                          ), #tabPanel 
                          tabPanel("Tables",
                                   h4("All Shark Species Caught"),
                                   withLoader(DT::dataTableOutput("topspeciestable"), type="html", loader="loader4"),
                                   br(),
                                   fluidRow(
                                     column(h4(textOutput("coa_text")),
                                            withLoader(tableOutput("coatable"), type="html", loader="loader4"), width=6),
                                     column(h4(textOutput("fate_text")),
                                            withLoader(tableOutput("fatetable"), type="html", loader="loader4"), width=6)
                                   ) #fluidRow
                                   ), # tabPanel
                          tabPanel("User Guide",
                                   h3("General Use"),
                                   p("This app displays shark bycatch data collected via video review from electronic monitoring systems
                                     onboard commercial fishing vessels in the Gulf of Mexico. Data is displayed in three formats located
                                     on three tabs: an interactive map, figures depicting effort, and tables describing catch disposition.
                                     In the panel on the left (if viewing on a desktop) there are inputs that can be used to filter the
                                     data presented in the three tabs. You can filter the data based on species (top 10 caught available),
                                     range in years the catch occurred, and season the catch occurred. You can view multiple species and their 
                                     combined data by selecting more than one from the dropdown menu. Remove species by clicking on them in the input bar 
                                     and pressing your 'Delete' key or by pressing 'Backspace'. Once your input selections have been
                                     made, click the 'Update' button to see the data change. The 'Total Observations' counter will also update
                                     to display how many records are associated with the input filters. Please note that all data reflects
                                     approximately 25% of fishing activities."),
                                   p(" "),
                                   h4("Map"),
                                   p("The interactive map displays the data filtered by the inputs in a gridded format (10 min by 10 min) depicting
                                     catch per 1000 hook hours. In longline fisheries, effort is usually determined in 'hook-hours' where the number of
                                     hooks multiplied by the amount of time the hooks were in the water can be used to estimate effort. This metric
                                     is also called catch-per-unit-effort (CPUE). In general, the darker the grid cell on the map, the more catch has 
                                     been recorded there."),
                                   h4("Figures"),
                                   p("Three figures depict effort. The first two figures depicting trip information and top catch events do not change 
                                     when the filters are updated. The bottom figure depicting catch-per-unit-effort over time changes based on the 
                                     species selected. To update the figure, select a new species from the dropdown menu and click the 'Update' button. 
                                     The lines are color-coded to denote whether the catch was discarded or retained, and the gray halo surrounding the 
                                     line represents the confidence interval."),
                                   h4("Tables"),
                                   p("The tables depict catch disposition. The first table does not change when the filters are updated and shows 
                                     the proportion of species caught to the total catch. The bottom tables change when new species are selected 
                                     and the 'Update' button is clicked."),
                                   h4("Feedback"),
                                   HTML("<p>Please report any issues, feedback, or requests by submitting a 
                                        <a href='https://github.com/knharrington/shark-bycatch'>GitHub issue</a> 
                                        or emailing <a href='mailto:knharrington@mote.org'>knharrington@mote.org</a>.</p>")
                                   ) #tabPanel 3
                        ) #tabsetPanel
                      ) #mainPanel
                    ) #sidebarLayout
                  )  #fluidPage
           ) # tabPanel
           
) #navbarPage

