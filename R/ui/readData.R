tabPanel(
    "Read Data",
    helpText('Select a file that contains your data.'),
    tags$hr(),
    uiOutput("Data_Tab_Controls_1"),
    uiOutput("Data_Tab_Controls_2"),
    DT::dataTableOutput("Data_Tab_Output_2")
)
