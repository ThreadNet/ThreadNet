tabPanel(
    "Comparisons",
    tabsetPanel(
        type = "tabs",
        tabPanel(
            "Compare Mappings (synchronic)",
            fluidRow(
                column(6,
                    fluidRow(
                        column(3, uiOutput("Comparison_Tab_Controls_A1")),
                        column(3, uiOutput("Comparison_Tab_Controls_A2"))
                    ),
                    selectizeInput(
                        'comparePanelSelect_A',
                        'Choose visualization:',
                        visualizations # references list defined in UI.R
                    ),
                    conditionalPanel(condition = "input.comparePanelSelect_A == 'Threads (event time)'",    plotlyOutput("Comp_A_1") ),
                    conditionalPanel(condition = "input.comparePanelSelect_A == 'Threads (actual time)'",   plotlyOutput("Comp_A_2") ),
                    conditionalPanel(condition = "input.comparePanelSelect_A == 'Threads (relative time)'", plotlyOutput("Comp_A_3") ),
                    conditionalPanel(
                        condition = "input.comparePanelSelect_A == 'Event network (circle)'",
                        uiOutput("Comp_A_4_controls"),
                        visNetworkOutput("Comp_A_4") 
                    ),
                    conditionalPanel(
                        condition = "input.comparePanelSelect_A == 'Event network (force)'",
                        uiOutput("Comp_A_5_controls"),
                        forceNetworkOutput("Comp_A_5"
                    ),
                    conditionalPanel(
                        condition = "input.comparePanelSelect_A == 'Other networks'",
                        uiOutput("Comp_A_6_controls"),
                        visNetworkOutput("Comp_A_6")
                    ),
                    conditionalPanel(
                        condition = "input.comparePanelSelect_A == 'Role Maps'",
                        uiOutput("Comp_A_7_controls"),
                        plotlyOutput("Comp_A_7")
                    ),
                    conditionalPanel(
                        condition = "input.comparePanelSelect_A == 'Thread Trajectories'",
                        plotlyOutput("Comp_A_8")
                    )
                ),
                column(6,
                    fluidRow(
                        column(3, uiOutput("Comparison_Tab_Controls_B1")),
                        column(3, uiOutput("Comparison_Tab_Controls_B2"))) ,
                        selectizeInput(
                            'comparePanelSelect_B',
                            'Choose visualization:',
                            visualizations # references list defined in UI.R
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Threads (event time)'",
                            plotlyOutput("Comp_B_1") 
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Threads (actual time)'",
                            plotlyOutput("Comp_B_2")
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Threads (relative time)'",
                            plotlyOutput("Comp_B_3")
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Event network (circle)'",
                            uiOutput("Comp_B_4_controls"),
                            visNetworkOutput("Comp_B_4")
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Event network (force)'",
                            uiOutput("Comp_B_5_controls"),
                            forceNetworkOutput("Comp_B_5")
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Other networks'",
                            uiOutput("Comp_B_6_controls"),
                            visNetworkOutput("Comp_B_6")
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Role Maps'",
                            uiOutput("Comp_B_7_controls"),
                            plotlyOutput("Comp_B_7")
                        ),
                        conditionalPanel(
                            condition = "input.comparePanelSelect_B == 'Thread Trajectories'",
                            plotlyOutput("Comp_B_8")
                        )
                    )
                )
            ),

            tabPanel(
                "Compare time periods (diachronic)",
                fluidRow(
                    column(3,uiOutput("Diachronic_Comparison_Tab_Controls_1") ),
                    column(3,uiOutput("Diachronic_Comparison_Tab_Controls_4"),uiOutput("Diachronic_Comparison_Tab_Controls_5") ),
                    column(3,uiOutput("Diachronic_Comparison_Tab_Controls_3") )
                ),
                plotlyOutput("DiachronicComparisonPlots") 
            )
        )
    )
)
