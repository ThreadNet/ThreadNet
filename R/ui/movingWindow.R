tabPanel(value = "movingWindow",
    "Moving Window",
    fluidRow(
        column(3,
            uiOutput("Moving_Window_Tab_Controls_1"),
            selectizeInput(
                'Moving_Window_Viz',
                'Choose visualization:',
                visualizations # references list defined in UI.R
            ),
            # radioButtons(
            #     "Moving_Window_Type",
            #     "Moving window type:",
            #     choices = c('Single Window','Dual Window'),
            #     selected = c('Single Window')
            # )
        ),
        column(3,
            uiOutput("Moving_Window_Tab_Controls_3"),
            uiOutput("Moving_Window_Tab_Controls_3a"),
            uiOutput("Moving_Window_Tab_Controls_2")
        ),
        column(3,
            # add controls that apply to both subsets here
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Event network (circle)'",
                uiOutput("Moving_4_controls")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Event network (force)'",
                uiOutput("Moving_5_controls")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Other networks'",
                uiOutput("Moving_6_controls")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Role Maps'",
                uiOutput("Moving_7_controls")
            )
        )
    ),

    conditionalPanel(
        condition = "input.Moving_Window_Type == 'Single Window'",
        plotlyOutput("single_moving_window_timeline")
    ),
    conditionalPanel(
        condition = "input.Moving_Window_Type == 'Dual Window'",
        plotlyOutput("dual_moving_window_timeline")
    ),
    fluidRow(
        column(6,
            "SubsetA",
            uiOutput("Moving_Window_Tab_Controls_4_A"),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Threads (event time)'",
                plotlyOutput("Moving_A_1")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Threads (actual time)'",
                plotlyOutput("Moving_A_2")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Threads (relative time)'",
                plotlyOutput("Moving_A_3")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Event network (circle)'",
                visNetworkOutput("Moving_A_4")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Event network (force)'",
                forceNetworkOutput("Moving_A_5")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Other networks'",
                visNetworkOutput("Moving_A_6")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Role Maps'",
                plotlyOutput("Moving_A_7")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Thread Trajectories'",
                plotlyOutput("Moving_A_8")
            )

        ),
        column(6,
            "SubsetB",
            uiOutput("Moving_Window_Tab_Controls_4_B"),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Threads (event time)'",
                plotlyOutput("Moving_B_1")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Threads (actual time)'",
                plotlyOutput("Moving_B_2")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Threads (relative time)'",
                plotlyOutput("Moving_B_3")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Event network (circle)'",
                visNetworkOutput("Moving_B_4")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Event network (force)'",
                forceNetworkOutput("Moving_B_5")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Other networks'",
                visNetworkOutput("Moving_B_6")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Role Maps'",
                plotlyOutput("Moving_B_7")
            ),
            conditionalPanel(
                condition = "input.Moving_Window_Viz == 'Thread Trajectories'",
                plotlyOutput("Moving_B_8")
            )
        )
    )
)
