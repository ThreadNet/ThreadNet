##########################################################################################################
# THREADNET:  SHINY UI

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# Sept 7, 2017 using architecture and advice from Ezra Brooks & Pat Bills

 pdf(NULL) # prevent plotly errors

ui <- fluidPage(

  # Application title
  tags$h3(align='center', "ThreadNet 3 Prototype"),

  #  tags$audio( src='tellusastory.mp3',type='audio/mpeg', controls='TRUE'),

  tabsetPanel(type = "tabs",
              tabPanel("Occurrences",
                       uiOutput("Data_Tab_Controls_1"),
                       uiOutput("Data_Tab_Controls_2"),
                       verbatimTextOutput("Data_Tab_Output_1" ),
                       uiOutput("Data_Tab_Controls_3"),
                       tableOutput("Data_Tab_Output_2")
              ),
              tabPanel("Choose POV",
                       tags$h3("Select columns from your data to define your point of view."),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Define Threads",
                                            tags$h4("Threads are defined by contextual features that STAY THE SAME during the thread. At least ONE is required."),
                                            uiOutput("POV_Tab_Controls_2"),
                                            plotlyOutput("ContextFlowers_2")
                                            ),
                                   tabPanel("Define Events",
                                            tags$h4("Events are marked by contextual features that CHANGE within the threads. At least ONE is required."),
                                            uiOutput("POV_Tab_Controls_3"),
                                            plotlyOutput("ContextFlowers_3")
                                            ),
                                   tabPanel("Comparison Groups",
                                            tags$h4("Choose dimensions for comparing patterns (OPTIONAL)."),
                                            tags$h4("For clarity of interpretation, these should stay the same for a group of threads."),
                                            uiOutput("POV_Tab_Controls_1"),
                                            plotlyOutput("ContextFlowers_1")
                                            ),
                                   tabPanel("Preview Threads",
                                            tags$h4("Threads based on selected POV"),
                                            verbatimTextOutput("Preview_Thread_Output_1" ),
                                            plotOutput("rawOccurrenceThreadMap"),
                                            plotlyOutput("rawOccurrenceThreadMap_2")
                                   ),
                                   tabPanel("Preview Network",
                                            tags$h4("Network based on sequential adjacency of raw occurrences"),
                                            visNetworkOutput("rawOccurrenceNetwork")
                                            ),
                                   tabPanel("Intermediate Data",
                                            tags$h4("This table shows the data threaded from your chosen POV"),
                                            tableOutput("Thread_Tab_Output_1")
                                   )
                                   )
              ),
              tabPanel("Occurrences to Events",
                       tags$h4("Map occurrences into events"),
                       uiOutput("Event_Tab_Controls_1"),
                       uiOutput("Event_Tab_Controls_2"),
                       uiOutput("Event_Tab_Controls_3"),
                       plotOutput("Event_Tab_Output_3"),
                       tableOutput("Event_Tab_Output_2")

              ),
              tabPanel("Zooming in-out",
                       tags$h3("Move slider to adjust granularity of event categories."),
                       uiOutput("Thread_Tab_Controls_1"),
                       tabsetPanel(type = "tabs",
                          tabPanel("Repetitive Sub-sequences",
                                  plotlyOutput("nGramBarchart"),
                                  uiOutput("nGramControls")
                                  ),
                          tabPanel("Event Sequences",
                                  tags$h4("Visualize threads"),
                                  plotOutput("threadMapEvents")
                                  ),
                          tabPanel("Event Networks" ,
                                    tabsetPanel(type = "tabs",
                                          tabPanel("Circular layout",
                                            visNetworkOutput("eventNetwork")),
                                          tabPanel("Force layout",
                                            uiOutput("Network_Tab_Controls_2"),
                                            forceNetworkOutput("eventNetworkD3")))
                          )
                        )
              ),
              tabPanel("Comparisons",
                       uiOutput("Comparison_Tab_Controls_1"),
                       uiOutput("Comparison_Tab_Controls_2"),
                       plotlyOutput("Comparison_Plots")
               ),
              tabPanel("Moving Window",
                       uiOutput("Moving_Window_Tab_Controls_1"),
                       uiOutput("Moving_Window_Tab_Controls_2"),
                       visNetworkOutput("MovingWindow_Plot")
              ),

              tabPanel("Parameter Settings",
                        tableOutput("currentParameterSettings")
                       ),
               tabPanel("Acknowledgements",
                        tags$h4("Code advisors:"),
                              tags$p("Yu Lucy Han, Ezra Brooks, Patrick Bills"),
                        tags$h4("Collaborators:"),
                             tags$p("Jan Recker, George Wyner, Martha Feldman, Thorvald Haerem, Waldemar Kremser, Julie Ryan Wolf, Ken Frank, Alice Pentland,  Inkyu Kim, Sudhanshu Srivastava"))

             )
  )


