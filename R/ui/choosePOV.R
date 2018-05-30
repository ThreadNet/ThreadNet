# UI: Choose POV tab definitions

tabPanel(

  value = "choosePOV",

  "Choose POV",

  tags$hr(),

  tabsetPanel(

    id = "tabs",

    tabPanel(
      value = "defineThreads",
      "Define Threads",
      tags$h4("Threads are defined by contextual features that STAY THE SAME during a thread. At least ONE is required."),
      uiOutput("povThreadSelector"),
      plotlyOutput("ContextFlowers_Threads")
    ),

    tabPanel(
      value = "defineEvents",
      "Define Events",
      tags$h4("Events are marked by contextual features that CHANGE within the threads. At least ONE is required."),
      uiOutput("povEventSelector"),
      plotlyOutput("ContextFlowers_Events")
    ),

    tabPanel(
      value = "showThreads",
      "Review Data",
      tags$h4("This table shows the data threaded from your chosen POV."),
      DT::dataTableOutput("povDataThreads")
    ),

    tabPanel(
      value = "saveThreads",
      "Save Data",
      tags$h4("Save this dataset to continue."),
      uiOutput("addPOV")
    )
  )
)
