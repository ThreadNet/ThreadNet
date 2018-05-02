tabPanel(
    "Choose POV",
    helpText('Select columns from your data to define your point of view. You MUST click on either Preview Threads or Preview Data before proceeding.'),
    tags$hr(),
    tabsetPanel(
        type = "tabs",
        tabPanel(
            "Define Threads",
            tags$h4("Threads are defined by contextual features that STAY THE SAME during a thread. At least ONE is required."),
            uiOutput("POV_Tab_Controls_2"),
            plotlyOutput("ContextFlowers_2")
        ),
        tabPanel(
            "Define Events",
            tags$h4("Events are marked by contextual features that CHANGE within the threads. At least ONE is required."),
            uiOutput("POV_Tab_Controls_3"),
            plotlyOutput("ContextFlowers_3")
        ),
        # tabPanel(
        #     "Preview Threads",
        #     tags$h4("Threads based on selected POV"),
        #     verbatimTextOutput("Preview_Thread_Output_1" ),
        #     # ** add conditional panels here to choose output **
        #     plotlyOutput("previewThreadMap_1")
        # ),
        tabPanel(
            "*** CLICK HERE TO PROCEED ***",
            tags$h4("This table shows the data threaded from your chosen POV"),
            DT::dataTableOutput("Thread_Tab_Output_1")
        )
    )
)

