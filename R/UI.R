##########################################################################################################
# THREADNET:  SHINY UI

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# Sept 7, 2017 New Shiny R version using architecture and advice from Ezra Brooks & Pat Bills
# March 21, 2018 New organization of tabs.

# pdf(NULL) # prevent plotly errors

ui <- fluidPage(

  # Application title
  tags$h3(align='center', "ThreadNet 3 Development"),

  #  tags$audio( src='tellusastory.mp3',type='audio/mpeg', controls='TRUE'),

  tabsetPanel(type = "tabs",
              tabPanel("Read Data",
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
                                            plotlyOutput("previewThreadMap")
                                   ),
                                   # Maybe take this out -- it's confusing
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
                                            uiOutput("One_to_One_controls"),
                                            conditionalPanel(
                                              condition = "input.One_to_One_Output_Button == 'Data table (display only)'",
                                                          DT::dataTableOutput("One_to_one_Tab_Output_1")),
                                            conditionalPanel(
                                              condition = "input.One_to_One_Output_Button == 'Thread Map'",
                                                          plotlyOutput("One_to_one_Tab_Output_2"))

                                   ),
                                   tabPanel("Contextual Chunks",
                                            uiOutput("Contextual_Chunk_controls"),
                                            conditionalPanel(
                                              condition = "input.Chunks_Output_Button == 'Data table (display only)'",
                                              DT::dataTableOutput("Contextual_Chunks_Tab_Output_1") ),
                                            conditionalPanel(
                                              condition = "input.Chunks_Output_Button == 'Thread Map'",
                                              plotlyOutput("Contextual_Chunks_Tab_Output_2"))

                                   ),

                                   tabPanel("Frequent Ngrams",
                                            fluidRow(
                                              column(3, uiOutput("Regular_Expression_controls_1")),
                                              column(3, uiOutput("Regular_Expression_controls_2")),
                                              column(3, uiOutput("Regular_Expression_controls_7"))
                                              ),
                                            uiOutput("Regular_Expression_controls_3"),
                                            verbatimTextOutput("Regular_Expression_controls_4"),
                                            uiOutput("Regular_Expression_controls_5"),  # how many regex?
                                            uiOutput("Regular_Expression_controls_6")

                                   ),

                                   # tabPanel("Regular Expressions",
                                   #          uiOutput("Frequent_Ngram_controls")
                                   # ),

                                   # tabPanel("Maximal Patterns",
                                   #          uiOutput("Maximal_Pattern_controls")
                                   # ),

                                   tabPanel("Cluster Events",
                                            uiOutput("Cluster_Event_controls_1"),
                                            uiOutput("Cluster_Event_controls_2"),
                                            dendroNetworkOutput("dendroClusterResult")
                                   ),

                                   tabPanel("Manage Event Maps",
                                            uiOutput("Manage_Event_Map_controls"),
                                            verbatimTextOutput("delete_confirm")
                                   )
                       )

              ),
              tabPanel("Visualize",
                       fluidRow(
                         column(3,
                                uiOutput("Visualize_Tab_Controls_1")),
                       column(4,
                              uiOutput("Visualize_Tab_Controls_2"))
                       ),

                       tabsetPanel(type = "tabs",
                          tabPanel("Select Subset",
                                            uiOutput("SelectSubsetControls"),
                                            DT::dataTableOutput("SelectSubsetDataTable")
                                   ),

                          tabPanel("Repetitive Sub-sequences",
                                  uiOutput("nGramControls"),
                                  plotlyOutput("nGramBarchart")
                                  ),

                          tabPanel("Whole Sequences",
                                   radioButtons("ChoosePanelButton_1", label = h4("Display threads using:"),
                                                choices = c("Event time (sequence)", "Actual time", "Relative time"),
                                                inline=TRUE),
                                   conditionalPanel(
                                      condition = "input.ChoosePanelButton_1 == 'Event time (sequence)'",
                                      plotlyOutput("WholeSequenceThreadMap_Sequence") ),
                                   conditionalPanel(
                                      condition = "input.ChoosePanelButton_1 == 'Actual time'",
                                      plotlyOutput("WholeSequenceThreadMap_ActualTime") )
                                  ),

                          tabPanel("Circular layout",
                                   tags$p("Put plotly circular layout here")),

                          tabPanel("Force layout",
                                   uiOutput("Network_Tab_Controls_2"),
                                   forceNetworkOutput("forceNetworkD3", width = "100%", height = "1200px")),

                          tabPanel("Custom layout",
                                   plotlyOutput("VisualizeCustomNetwork"))
                          )

              ),

              tabPanel("Comparisons",
                       tabsetPanel(type = "tabs",
                         tabPanel("Compare Mappings (synchronic)",
                       fluidRow(
                         column(6, tags$h3("Mapping A"),
                                uiOutput("Comparison_Tab_Controls_A1"),
                                uiOutput("Comparison_Tab_Controls_A2"),
                                plotlyOutput("Comparison_Plots_A")),
                         column(6,tags$h3("Mapping B"),
                                uiOutput("Comparison_Tab_Controls_B1"),
                                uiOutput("Comparison_Tab_Controls_B2"),
                                plotlyOutput("Comparison_Plots_B"))
                                )),
                       tabPanel("Compare time periods (diachronic)",
                                fluidRow(
                                  column(3,
                                         uiOutput("Diachronic_Comparison_Tab_Controls_1")),
                                  column(4,
                                         uiOutput("Diachronic_Comparison_Tab_Controls_2"))
                                ),
                                uiOutput("Diachronic_Comparison_Tab_Controls_3"),
                                uiOutput("Diachronic_Comparison_Tab_Controls_4"),
                                uiOutput("Diachronic_Comparison_Tab_Controls_5"),
                                plotlyOutput("DiachronicComparisonPlots")
                                )

                                )
               ),

              tabPanel("Moving Window",
                       fluidRow(
                         column(3,
                              uiOutput("Moving_Window_Tab_Controls_1")),
                         column(4,
                                uiOutput("Moving_Window_Tab_Controls_2"))
                       ),
                       uiOutput("Moving_Window_Tab_Controls_3"),


                       fluidRow(
                         column(6,"SubsetA",
                                uiOutput("Moving_Window_Tab_Controls_4_A"),
                                plotlyOutput("MovingWindow_Plot_A")
                         ),
                         column(6,"SubsetB",
                                uiOutput("Moving_Window_Tab_Controls_4_B"),
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


