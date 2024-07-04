################################################################################
# This script builds the user interface for the CFEMM app
# capabilities include: viewing tables, maps, and figures
################################################################################

# page appearance

navbarPage(title=div(img(src="mote-logo.png", style = "width:30px;height:27px")," Bycatch Data Summary", style = "text-align: center"), theme = shinytheme("flatly"), 
           
           tags$head(tags$style(HTML(
             '.navbar-static-top {background-color: #023f88;}',
             '.navbar-default .navbar-nav>.active>a {background-color: #023f88;}',
             '.btn {background-color: #023f88}',
             '.nav-tabs > li > a {color: #00aae7;}',
             'a {color: #00aae7;}',
             'a:hover {color: #00aae7;}'
           ))),
           
           tabPanel(title=div("Sharks"),
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(h4("About This App"),
                                   HTML("<p>The data displayed in this app was collected via 25% review of electronic monitoring (EM) footage from bottom longline vessels
                                      in the Gulf of Mexico reef fish fishery (2016-2023) as a part of the Center for Fisheries Electronic Monitoring at Mote Marine Laboratory's
                                      voluntary EM program. For more information, please visit 
                                      <a href='https://www.mote.org/cfemm'>mote.org/cfemm</a>.</p>"),
                                   hr(style = "border-top: 1px solid #000000;"),
                                   helpText("Use the following selections to update the data displayed."),
                                   strong("Total Observations:"),
                                   verbatimTextOutput("text_obs", placeholder=FALSE),
                                   p(" "),
                                   selectInput("select_species", label = "Select Species", choices = sort(unique(top.sub$Common_Name)), selected = sort(unique(top.sub$Common_Name))[1]),
                                   sliderInput("years", "Time Range", min(top.sub$Retrieval_Year), max(top.sub$Retrieval_Year),
                                                  value = c(min(top.sub$Retrieval_Year), max(top.sub$Retrieval_Year)), sep = "", round = TRUE, step = 1),
                                   checkboxGroupInput("seasons", "Season", 
                                                      choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
                                                                         HTML("<b>Spring</b> (Apr, May, Jun)"), 
                                                                         HTML("<b>Summer</b> (Jul, Aug, Sep)"), 
                                                                         HTML("<b>Fall</b> (Oct, Nov, Dec)")), 
                                                      selected = list("Winter","Spring", "Summer", "Fall"),
                                                      choiceValues = list("Winter","Spring", "Summer", "Fall")),
                                   submitButton("Update")
                      ), #sidebarPanel
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Maps",
                                   withLoader(leafletOutput("mainmap", height = "85vh"), type="html", loader="loader4")
                                   ),
                          tabPanel("Figures",
                                   fluidRow(
                                     column(withLoader(plotOutput("activityplot"), type="html", loader="loader4"), width=6),
                                     column(withLoader(plotOutput("topspeciesplot"), type="html", loader="loader4"), width=6)
                                   ),
                                   p(" "),
                                   wellPanel(helpText("Use the dropdown box on the left to change the species displayed in the following figure.")),
                                   withLoader(plotOutput("cpueplot"), type="html", loader="loader4"),
                                   p(" ")
                          ), #tabPanel 2
                          #textOutput("radio.txt"),
                          #tableOutput("trouble"),
                          #tableOutput("trouble2")
                          tabPanel("Tables",
                                   h4("All Shark Species Caught"),
                                   withLoader(DT::dataTableOutput("topspeciestable"), type="html", loader="loader4"),
                                   p(" "),
                                   wellPanel(helpText("Use the dropdown box on the left to change the species displayed in the following tables."),
                                             htmlOutput("text_sp")),
                                   fluidRow(
                                     column(h4("Condition on Arrival"),
                                            withLoader(tableOutput("coatable"), type="html", loader="loader4"), width=6),
                                     column(h4("Catch Fate"),
                                            withLoader(tableOutput("fatetable"), type="html", loader="loader4"), width=6)
                                   ) #fluidRow
                                   ), # tabPanel
                          tabPanel("User Guide",
                                   h3("How to Use this App"),
                                   #p(""),
                                   h4("General Use"),
                                   p('This app displays shark bycatch data collected via video review from electronic monitoring systems
                                     onboard commercial fishing vessels in the Gulf of Mexico. Data is displayed in three formats located
                                     on three tabs: an interactive map, figures depicting effort, and tables describing catch disposition.
                                     In the panel on the left (if viewing on a desktop) there are inputs that can be used to filter the
                                     data presented in the three tabs. You can filter the data based on species (top 10 caught available),
                                     range in years the catch occurred, and season the catch occurred. Once you rinput selections have been
                                     made, click the "Update" button to see the data change. The "Total Observations" counter will also update
                                     to display how many records are associated with the input filters. Please note that all data reflects
                                     approximately 25% of fishing activities.'),
                                   p(" "),
                                   h4("Feedback"),
                                   HTML("<p>Please report any issues, feedback, or requests by submitting a 
                                        <a href='https://github.com/knharrington/shark-bycatch'>GitHub issue</a> 
                                        or emailing knharrington@mote.org.</p>")
                                   ) #tabPanel 3
                        ) #tabsetPanel
                      ) #mainPanel
                    ) #sidebarLayout
                  )  #fluidPage
           ) # tabPanel
           
) #navbarPage

