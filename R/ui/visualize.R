tabPanel(
    "Visualize",
    fluidRow(
        column(3,uiOutput("Visualize_Tab_Controls_1")),
        conditionalPanel(
            condition = "input.tabs !== 'Custom'",
            column(4,uiOutput("Visualize_Tab_Controls_2") )
        )
    ),
    tabsetPanel(
        type = "tabs",
        tabPanel("N-grams", uiOutput("nGramControls"), plotlyOutput("nGramBarchart")),
        tabPanel(
            "Whole Sequences",
            radioButtons(
                "ChoosePanelButton_1",
                label = h4("Display threads using:"),
                choices = c("Event time (sequence)", "Actual time", "Relative time"),
                inline=TRUE
            ),
            conditionalPanel(
                condition = "input.ChoosePanelButton_1 == 'Event time (sequence)'",
                plotlyOutput("WholeSequenceThreadMap_Sequence")
            ),
            conditionalPanel(
                condition = "input.ChoosePanelButton_1 == 'Actual time'",
                plotlyOutput("WholeSequenceThreadMap_ActualTime")
            ),
            conditionalPanel(
                condition = "input.ChoosePanelButton_1 == 'Relative time'",
                plotlyOutput("WholeSequenceThreadMap_RelativeTime")
            )
        ),
        tabPanel(
            "Event network (circle)",
            uiOutput("Circle_Network_Tab_Controls"),
            visNetworkOutput("circleVisNetwork", width = "100%", height = "1200px")
        ),
        tabPanel(
            "Event network (force)",
            uiOutput("Force_Network_Tab_Controls"),
            fluidRow(
                column(9, forceNetworkOutput("forceNetworkD3", width = "100%", height = "1200px")),
                column(3, plotlyOutput("networkPie"))
            )
        ),
        tabPanel(
            "View events",
            value = 'Custom',
            fluidRow(
                column(3, uiOutput("VisualizeCustomNetwork_Controls_0")),
                column(3, uiOutput("VisualizeCustomNetwork_Controls_1"))
            ),
            plotlyOutput("VisualizeCustomNetwork"),
            verbatimTextOutput("hover"),
            verbatimTextOutput("click")
        ),
        tabPanel(
            "Other networks",
            uiOutput("Other_Network_Tab_Controls"),
            visNetworkOutput("otherVisNetwork", width = "100%", height = "1200px")
        ),
        tabPanel("Role Maps",uiOutput("Role_map_controls"), plotlyOutput("Role_map_output") ),
        tabPanel("Thread Trajectories", plotlyOutput("ThreadTrajectoriesOutput") )
    )                    
)
