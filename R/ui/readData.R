# UI: Read Data tab definitions

tabPanel(
  value = "readData",
  "Read Data",
  helpText('Select a file that contains your data.'),
  tags$hr(),
  uiOutput("fileSelector"),
  uiOutput("timeSelector"),
  uiOutput("columnSelector"),
  DT::dataTableOutput("dataFilter")
)
