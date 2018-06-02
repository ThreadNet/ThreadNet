# UI: Choose POV tab definitions

tabPanel(

  value = "choosePOV",

  "Choose POV",

  tags$hr(),

  tabsetPanel(

    id = "tabs",

    tabPanel(
      value = "defineThreads",
      "Step 1: Define Threads",
      tags$h4("Threads are defined by contextual features that STAY THE SAME during a thread. At least ONE is required."),
      uiOutput("povThreadSelector"),
      plotlyOutput("ContextFlowers_Threads")
    ),

    tabPanel(
      value = "defineEvents",
      "Step 2: Define Events",
      tags$h4("Events are marked by contextual features that CHANGE within the threads. At least ONE is required."),
      uiOutput("povEventSelector"),
      plotlyOutput("ContextFlowers_Events")
    ),

    tabPanel(
      value = "showThreads",
      "Step 3: Select Subset",
      tags$h4("Optional: you can choose a subset of this POV for comparison."),
      DT::dataTableOutput("povDataThreads")
    ),

    tabPanel(
      value = "saveThreads",
      "Step 4: Save POV",
      tags$h4("Save this POV to continue. You can create and compare multiple POVs.") ,
      fluidRow(
        column(3, uiOutput("addPOV1") ),
        column(3, uiOutput("addPOV2") ) )
    )
  )
)
