##########################################################################################################
# THREADNET:  SHINY UI

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# Sept 7, 2017 New Shiny R version using architecture and advice from Ezra Brooks & Pat Bills
# March 21, 2018 New organization of tabs.

# pdf(NULL) # prevent plotly errors
library(shiny)
library(networkD3)
library(visNetwork)

ui <- fluidPage(

  # Application title
  tags$h3(align='center', "ThreadNet 3 Development"),

  #  tags$audio( src='tellusastory.mp3',type='audio/mpeg', controls='TRUE'),

  tabsetPanel(type = "tabs",
              tabPanel("Read Data",
                       helpText('Select a file that contains your data.'),
                       tags$hr(),
                       uiOutput("Data_Tab_Controls_1"),
                       uiOutput("Data_Tab_Controls_2"),
                       DT::dataTableOutput("Data_Tab_Output_2")
              ),
              tabPanel("Choose POV",
                       helpText('Select columns from your data to define your point of view. You MUST click on either Preview Threads or Preview Data before proceeding.'),
                       tags$hr(),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Define Threads",
                                            tags$h4("Threads are defined by contextual features that STAY THE SAME during a thread. At least ONE is required."),
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
                                            # ** add conditional panels here to choose output **
                                            plotlyOutput("previewThreadMap_1")
                                            ),
                                   tabPanel("Preview Data",
                                            tags$h4("This table shows the data threaded from your chosen POV"),
                                            DT::dataTableOutput("Thread_Tab_Output_1")
                                            )
                                   )
              ),
              tabPanel("Visualize",
                       fluidRow(
                         column(3,
                                uiOutput("Visualize_Tab_Controls_1")),
                                conditionalPanel(
                                        condition = "input.tabs !== 'Custom'",
                           column(4,
                                  uiOutput("Visualize_Tab_Controls_2") ))
                           ),

                       tabsetPanel(type = "tabs",
                                   tabPanel("N-grams",
                                            uiOutput("nGramControls"),
                                            plotlyOutput("nGramBarchart")
                                   ),

                                   tabPanel("Whole Sequences",
                                            radioButtons("ChoosePanelButton_1", label = h4("Display threads using:"),
                                                         choices = c("Event time (sequence)", "Actual time", "Relative time"),
                                                         inline=TRUE),
                                            conditionalPanel(
                                              condition = "input.ChoosePanelButton_1 == 'Event time (sequence)'",
                                              plotlyOutput("WholeSequenceThreadMap_Sequence")),
                                            conditionalPanel(
                                              condition = "input.ChoosePanelButton_1 == 'Actual time'",
                                              plotlyOutput("WholeSequenceThreadMap_ActualTime")),
                                            conditionalPanel(
                                              condition = "input.ChoosePanelButton_1 == 'Relative time'",
                                              plotlyOutput("WholeSequenceThreadMap_RelativeTime"))
                                   ),

                                   tabPanel("Event network (circle)",
                                            uiOutput("Circle_Network_Tab_Controls"),
                                            visNetworkOutput("circleVisNetwork", width = "100%", height = "1200px")),

                                   tabPanel("Event network (force)",
                                            uiOutput("Force_Network_Tab_Controls"),
                                            forceNetworkOutput("forceNetworkD3", width = "100%", height = "1200px")),

                                   tabPanel("View events", value = 'Custom',
                                            fluidRow(
                                              column(3, uiOutput("VisualizeCustomNetwork_Controls_0")),
                                              column(3, uiOutput("VisualizeCustomNetwork_Controls_1")) ),
                                            plotlyOutput("VisualizeCustomNetwork"),
                                            verbatimTextOutput("hover"),
                                            verbatimTextOutput("click")),

                                   tabPanel("Other networks",
                                            uiOutput("Other_Network_Tab_Controls"),
                                            visNetworkOutput("otherVisNetwork", width = "100%", height = "1200px")),

                                   tabPanel("Role Maps",
                                            uiOutput("Role_map_controls"),
                                            plotlyOutput("Role_map_output")),

                                   tabPanel("Thread Trajectories",
                                            plotlyOutput("ThreadTrajectoriesOutput"))
                       )

              ),
              tabPanel("Subsets and mapping",
                       helpText('Create subsets of your data and alternative mappings'),
                       tags$hr(),
                       tabsetPanel(type = "tabs",

                                   tabPanel("Contextual Chunks",
                                            helpText('This is some help text for this tab...'),
                                            tags$hr(),
                                            fluidRow(
                                              column(3, uiOutput("chunk_controls_0")),
                                              column(3,
                                                     # add method for RLE -- remove sequential runs
                                                     radioButtons("Chunks_method_Button", label = h4("Choose method for chunking:"),
                                                                                        choices = c( "Changes", "Time Gap","Fixed Size"),
                                                                                        inline=TRUE),
                                                                           conditionalPanel(
                                                                             condition = "input.Chunks_method_Button == 'Changes'",
                                                                             uiOutput("chunk_controls_2") ),
                                                                           conditionalPanel(
                                                                              condition = "input.Chunks_method_Button == 'Time Gap'",
                                                                              uiOutput("chunk_controls_3") ),
                                                                          conditionalPanel(
                                                                              condition = "input.Chunks_method_Button == 'Fixed Size'",
                                                                              uiOutput("chunk_controls_4") )
                                                        ),

                                              column(3, uiOutput("chunk_controls_5"))
                                            ),
                                            uiOutput("chunk_controls_1"),
                                            uiOutput("chunk_controls_6"),
                                            verbatimTextOutput("chunk_controls_7")
                                   ),

                                   tabPanel("Cluster for Zooming",
                                            helpText('This is some help text for this tab...'),
                                            tags$hr(),
                                            fluidRow(
                                              column(3,  uiOutput("Cluster_Event_controls_1") ),
                                              column(3,  uiOutput("Cluster_Event_controls_2") ),
                                              column(3,  uiOutput("Cluster_Event_controls_3") )
                                            ),
                                            dendroNetworkOutput("dendroClusterResult")
                                   ),


                                   tabPanel("Select Subset",
                                            helpText('Select and save a subset of data for visualization and comparison.'),
                                            tags$hr(),
                                            fluidRow(
                                              column(3, uiOutput("SelectSubsetControls_1")),
                                              column(3, uiOutput("SelectSubsetControls_2")) ),
                                            DT::dataTableOutput("SelectSubsetDataTable")
                                   ),

                                   tabPanel("Find/replace patterns",
                                            helpText('Find/replace frequently occurring n-grams with the label of your choice'),
                                            tags$hr(),
                                            fluidRow(
                                              column(3, uiOutput("Frequent_Ngram_controls_1")),
                                              column(3, uiOutput("Frequent_Ngram_controls_2"),
                                                     uiOutput("Frequent_Ngram_controls_21")),
                                              column(3, uiOutput("Frequent_Ngram_controls_7"))
                                              ),
                                            uiOutput("Frequent_Ngram_controls_3"),
                                            verbatimTextOutput("Frequent_Ngram_controls_4"),
                                            tags$h4("Select patterns by clicking on the table:"),
                                            DT::dataTableOutput("freqnGramTable")
                                   ),

                                   tabPanel("Input your pattern",
                                            helpText('Enter your own patterns to replace with the label of your choice'),
                                            tags$hr(),
                                            fluidRow(
                                              column(3, uiOutput("Regular_Expression_controls_1")),
                                              column(3, uiOutput("Regular_Expression_controls_2")),
                                              column(3, uiOutput("Regular_Expression_controls_7"))
                                            ),
                                            uiOutput("Regular_Expression_controls_3"),
                                            verbatimTextOutput("Regular_Expression_controls_4"),
                                            uiOutput("Regular_Expression_controls_5"),  # how many rows?
                                            uiOutput("Regular_Expression_controls_6")
                                          ),

                                   tabPanel("Manage Event Maps",
                                            helpText('Delete or export event maps'),
                                            tags$hr(),
                                            uiOutput("Manage_Event_Map_controls"),
                                            verbatimTextOutput("action_confirm")
                                            )
                       )

              ),

              tabPanel("Comparisons",
                       tabsetPanel(type = "tabs",
                         tabPanel("Compare Mappings (synchronic)",
                       fluidRow(
                         column(6,
                                fluidRow(
                                  column(3, uiOutput("Comparison_Tab_Controls_A1")),
                                  column(3, uiOutput("Comparison_Tab_Controls_A2"))) ,
                                selectizeInput('comparePanelSelect_A','Choose visualization:',
                                               c('Threads (event time)','Threads (actual time)','Threads (relative time)',
                                                 'Event network (circle)','Event network (force)','Other networks',
                                                 'Role Maps','Thread Trajectories')),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Threads (event time)'",
                                  plotlyOutput("Comp_A_1") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Threads (actual time)'",
                                  plotlyOutput("Comp_A_2") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Threads (relative time)'",
                                  plotlyOutput("Comp_A_3") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Event network (circle)'",
                                  uiOutput("Comp_A_4_controls"),
                                  visNetworkOutput("Comp_A_4") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Event network (force)'",
                                  uiOutput("Comp_A_5_controls"),
                                  forceNetworkOutput("Comp_A_5") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Other networks'",
                                  uiOutput("Comp_A_6_controls"),
                                  visNetworkOutput("Comp_A_6") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Role Maps'",
                                  uiOutput("Comp_A_7_controls"),
                                  plotlyOutput("Comp_A_7") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_A == 'Thread Trajectories'",
                                  plotlyOutput("Comp_A_8") )
                                ),
                         column(6,
                                fluidRow(
                                  column(3, uiOutput("Comparison_Tab_Controls_B1")),
                                  column(3, uiOutput("Comparison_Tab_Controls_B2"))) ,
                                selectizeInput('comparePanelSelect_B','Choose visualization:',
                                               c('Threads (event time)','Threads (actual time)','Threads (relative time)',
                                                 'Event network (circle)','Event network (force)','Other networks',
                                                 'Role Maps','Thread Trajectories')),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Threads (event time)'",
                                  plotlyOutput("Comp_B_1") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Threads (actual time)'",
                                  plotlyOutput("Comp_B_2") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Threads (relative time)'",
                                  plotlyOutput("Comp_B_3") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Event network (circle)'",
                                  uiOutput("Comp_B_4_controls"),
                                  visNetworkOutput("Comp_B_4") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Event network (force)'",
                                  uiOutput("Comp_B_5_controls"),
                                  forceNetworkOutput("Comp_B_5") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Other networks'",
                                  uiOutput("Comp_B_6_controls"),
                                  visNetworkOutput("Comp_B_6") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Role Maps'",
                                  uiOutput("Comp_B_7_controls"),
                                  plotlyOutput("Comp_B_7") ),
                                conditionalPanel(
                                  condition = "input.comparePanelSelect_B == 'Thread Trajectories'",
                                  plotlyOutput("Comp_B_8") )
                                ))
                       ),

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
                              uiOutput("Moving_Window_Tab_Controls_1"),
                              selectizeInput('Moving_Window_Viz','Choose visualization:',
                                             c('Threads (event time)','Threads (actual time)','Threads (relative time)',
                                               'Event network (circle)','Event network (force)','Other networks',
                                               'Role Maps','Thread Trajectories')) ),
                         column(3,
                                uiOutput("Moving_Window_Tab_Controls_3"),
                                uiOutput("Moving_Window_Tab_Controls_2")),
                         column(3,
                                # add controls that apply to both subsets here
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Event network (circle)'",
                                  uiOutput("Moving_4_controls") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Event network (force)'",
                                  uiOutput("Moving_5_controls") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Other networks'",
                                  uiOutput("Moving_6_controls") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Role Maps'",
                                  uiOutput("Moving_7_controls") ) )
                       ),
                       fluidRow(
                         column(6,"SubsetA",
                                uiOutput("Moving_Window_Tab_Controls_4_A"),

                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Threads (event time)'",
                                  plotlyOutput("Moving_A_1") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Threads (actual time)'",
                                  plotlyOutput("Moving_A_2") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Threads (relative time)'",
                                  plotlyOutput("Moving_A_3") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Event network (circle)'",
                                  visNetworkOutput("Moving_A_4") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Event network (force)'",
                                  forceNetworkOutput("Moving_A_5") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Other networks'",
                                  visNetworkOutput("Moving_A_6") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Role Maps'",
                                  plotlyOutput("Moving_A_7") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Thread Trajectories'",
                                  plotlyOutput("Moving_A_8") )

                         ),
                         column(6,"SubsetB",
                                uiOutput("Moving_Window_Tab_Controls_4_B"),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Threads (event time)'",
                                  plotlyOutput("Moving_B_1") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Threads (actual time)'",
                                  plotlyOutput("Moving_B_2") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Threads (relative time)'",
                                  plotlyOutput("Moving_B_3") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Event network (circle)'",
                                  visNetworkOutput("Moving_B_4") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Event network (force)'",
                                  forceNetworkOutput("Moving_B_5") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Other networks'",
                                  visNetworkOutput("Moving_B_6") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Role Maps'",
                                  plotlyOutput("Moving_B_7") ),
                                conditionalPanel(
                                  condition = "input.Moving_Window_Viz == 'Thread Trajectories'",
                                  plotlyOutput("Moving_B_8") )
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
                              tags$p("Yu Lucy Han, Ezra Brooks, Patrick Bills, Danielle Barnes, Morgan Patterson, Douglas Krum"),
                        tags$h4("Collaborators:"),
                             tags$p("Jan Recker, George Wyner, Martha Feldman, Thorvald Haerem, Waldemar Kremser, Julie Ryan Wolf, Ken Frank, Alice Pentland,  Inkyu Kim, Sudhanshu Srivastava"),
                        tags$h4("Related Publications:"),
                        tags$a(href="http://routines.broad.msu.edu/resources/","http://routines.broad.msu.edu/resources/" ),
                        tags$h4("ThreadNet 2 (MatLab version):"),
                        tags$a(href="http://routines.broad.msu.edu/ThreadNet/","http://routines.broad.msu.edu/ThreadNet/" )
                        )
             )
  )


