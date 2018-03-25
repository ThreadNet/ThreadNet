##########################################################################################################
# THREADNET:  SHINY UI

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# Sept 7, 2017 New Shiny R version using architecture and advice from Ezra Brooks & Pat Bills

# pdf(NULL) # prevent plotly errors

ui <- fluidPage(

  # Application title
  tags$h3(align='center', "ThreadNet 3 Prototype"),

  #  tags$audio( src='tellusastory.mp3',type='audio/mpeg', controls='TRUE'),

  tabsetPanel(type = "tabs",
              tabPanel("Read Data",
                       tags$h4("Read data from CSV. First column should be tStamp (mm/dd/yyyy hh:mm:ss)"),
                       uiOutput("Data_Tab_Controls_1"),
                       uiOutput("Data_Tab_Controls_2"),
                       DT::dataTableOutput("Data_Tab_Output_2")
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

                                   tabPanel("Preview Threads",
                                            tags$h4("Threads based on selected POV"),
                                            verbatimTextOutput("Preview_Thread_Output_1" ),
                                            # plotOutput("rawOccurrenceThreadMap"),
                                            plotlyOutput("rawOccurrenceThreadMap_2")
                                   ),

                                   tabPanel("Intermediate Data",
                                            tags$h4("This table shows the data threaded from your chosen POV"),
                                            DT::dataTableOutput("Thread_Tab_Output_1")
                                   )
                                   )
              ),
              tabPanel("Occurrences to Events",
                       tags$h4("Map occurrences into events"),
                       tabsetPanel(type = "tabs",
                                   tabPanel("One-to-One",
                                            uiOutput("One_to_One_controls")

                                   ),
                                   tabPanel("Contextual Chunks",
                                            uiOutput("Contextual_Chunk_controls")
                                   ),

                                   tabPanel("Regular Expressions",
                                            uiOutput("Regular_Expression_controls")
                                   ),

                                   tabPanel("Frequent Ngrams",
                                            uiOutput("Frequent_Ngram_controls")
                                   ),

                                   tabPanel("Maximal Patterns",
                                            uiOutput("Maximal_Pattern_controls")
                                   ),

                                   tabPanel("Manage Event Maps",
                                            uiOutput("Manage_Event_Map_controls")
                                   )
                       )


              ),
              tabPanel("Visualize",
                       uiOutput("Visualize_Tab_Controls_1"),
                       uiOutput("Visualize_Tab_Controls_2"),
                       tabsetPanel(type = "tabs",
                          tabPanel("Repetitive Sub-sequences",
                                  uiOutput("nGramControls"),
                                  plotlyOutput("nGramBarchart")
                                  ),

                          tabPanel("Whole Sequences",
                                  tags$h4("Put plotly threadmap here..."),
                                  plotOutput("threadMapEvents")
                                  ),

                          tabPanel("Circular layout",
                                   tags$p("Put plotly circular layout here")),

                          tabPanel("Force layout",
                                   uiOutput("Network_Tab_Controls_2"),
                                   forceNetworkOutput("eventNetworkD3", width = "100%", height = "1200px")),

                          tabPanel("Custom layout",
                                tags$p("Put plotly custom layout here")))

              ),

              tabPanel("Comparisons",
                       uiOutput("Comparison_Tab_Controls_1"),
                       uiOutput("Comparison_Tab_Controls_2"),
                       fluidRow(
                         column(6,"SubsetA",
                                tags$p("subset A"),
                                plotlyOutput("Comparison_Plots_A")
                                ),
                         column(6,"Subsetb",
                                tags$p("subset B"),
                                plotlyOutput("Comparison_Plots_B")
                                )
                                )
               ),

              tabPanel("Moving Window",
                       uiOutput("Moving_Window_Tab_Controls_1"),
                       uiOutput("Moving_Window_Tab_Controls_2"),
                       uiOutput("Moving_Window_Tab_Controls_3"),
                       uiOutput("Moving_Window_Tab_Controls_4"),
                       fluidRow(
                         column(6,"SubsetA",
                                plotlyOutput("MovingWindow_Plot_A")
                         ),
                         column(6,"SubsetB",
                                plotlyOutput("MovingWindow_Plot_B")
                         )
                       )

              ),

              tabPanel("Parameter Settings",
                        tableOutput("currentParameterSettings")
                       ),
               tabPanel("Acknowledgements",
                        tags$h4("Support:"),
                          tags$a(href="https://www.nsf.gov/awardsearch/showAward?AWD_ID=1734237","NSF SES-1734237"),
                          tags$p("Antecedents of Complexity in Healthcare Routines"),
                        tags$h4("Code Gurus:"),
                              tags$p("Yu Lucy Han, Ezra Brooks, Patrick Bills, Danielle Barnes, Morgan Patterson"),
                        tags$h4("Collaborators:"),
                             tags$p("Jan Recker, George Wyner, Martha Feldman, Thorvald Haerem, Waldemar Kremser, Julie Ryan Wolf, Ken Frank, Alice Pentland,  Inkyu Kim, Sudhanshu Srivastava"),
                        tags$h4("Related Publications:"),
                        tags$a(href="http://routines.broad.msu.edu/resources/","http://routines.broad.msu.edu/resources/" ),
                        tags$h4("ThreadNet 2 (MatLab version):"),
                        tags$a(href="http://routines.broad.msu.edu/ThreadNet/","http://routines.broad.msu.edu/ThreadNet/" )
                        )
             )
  )


