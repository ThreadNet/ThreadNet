# Server Output Functions for Comparisons Tab

# Make two parallel sets of input and data. Different mapping on each side

  # ####### SUBSET A   ##########
  output$Comparison_Tab_Controls_A1 <- renderUI({
    selectizeInput("CompareMapInputID_A",label = h4("Choose mapping A:"),  get_event_mapping_names() )
  })

  output$Comparison_Tab_Controls_A2 <- renderUI({
    req(input$CompareMapInputID_A)
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$CompareMapInputID_A))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("CompareZoomID_A",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })

 # just one type of plot for now -- need to select different plot types
  output$Comparison_Plots_A <- renderPlotly({
    threadMap(threadedEventsComp_A(), "threadNum", "seqNum", get_Zoom_COMP_A(), 15  )
  })

  # ####### SUBSET B   ##########
  output$Comparison_Tab_Controls_B1 <- renderUI({
    selectizeInput("CompareMapInputID_B",label = h4("Choose mapping B:"),  get_event_mapping_names() )
  })

  output$Comparison_Tab_Controls_B2 <- renderUI({
    req(input$CompareMapInputID_B)
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$CompareMapInputID_B))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("CompareZoomID_B",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })


 # just one type of plot for now -- need to select different plot types
  output$Comparison_Plots_B <- renderPlotly({
    threadMap(threadedEventsComp_B(), "threadNum", "seqNum", get_Zoom_COMP_B(), 15  )
  })

  #### Put all eight here #####
  output$Comp_A_1 <- renderPlotly({ threadMap(threadedEventsComp_A(), "threadNum", "seqNum", get_Zoom_COMP_A(), 15  ) })
  output$Comp_B_1 <- renderPlotly({ threadMap(threadedEventsComp_B(), "threadNum", "seqNum", get_Zoom_COMP_B(), 15  ) })

  output$Comp_A_2 <- renderPlotly({ threadMap(threadedEventsComp_A(), "threadNum", "tStamp", get_Zoom_COMP_A(), 15 ) })
  output$Comp_B_2 <- renderPlotly({ threadMap(threadedEventsComp_B(), "threadNum", "tStamp", get_Zoom_COMP_B(), 15 ) })

  output$Comp_A_3 <- renderPlotly({ threadMap(threadedEventsComp_A(), "threadNum", "relativeTime", get_Zoom_COMP_A(), 15 ) })
  output$Comp_B_3 <- renderPlotly({ threadMap(threadedEventsComp_B(), "threadNum", "relativeTime", get_Zoom_COMP_B(), 15 ) })

  output$Comp_A_4_controls <- renderUI({sliderInput("A_4_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_B_4_controls <- renderUI({sliderInput("B_4_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})

  output$Comp_A_4 <- renderVisNetwork({
    req(input$A_4_Theshold)
    n = threads_to_network_original( threadedEventsComp_A(), "threadNum", get_Zoom_COMP_A() )
    n=filter_network_edges(n,input$A_4_Theshold)
    circleVisNetwork( n ) })

  output$Comp_B_4 <- renderVisNetwork({
    req(input$B_4_Theshold)
    n = threads_to_network_original( threadedEventsComp_B(), "threadNum", get_Zoom_COMP_B() )
    n=filter_network_edges(n,input$B_4_Theshold)
    circleVisNetwork( n  ) })

  output$Comp_A_5_controls <- renderUI({sliderInput("A_5_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_B_5_controls <- renderUI({sliderInput("B_5_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_A_5 <- renderForceNetwork({
    req(input$A_5_Theshold)
    n = threads_to_network_original( threadedEventsComp_A(), 'threadNum', get_Zoom_COMP_A(), 'threadNum' )
    n = filter_network_edges(n,input$A_5_Theshold)
    forceNetworkD3( n )  })

  output$Comp_B_5 <- renderForceNetwork({
    req(input$B_5_Theshold)
    n = threads_to_network_original( threadedEventsComp_B(), 'threadNum', get_Zoom_COMP_B(), 'threadNum' )
    n = filter_network_edges(n,input$B_5_Theshold)
    forceNetworkD3( n )  })


output$Comp_A_6_controls <- renderUI({button_choices = get_EVENT_CF()
  tags$div(
    radioButtons("A_6_OtherNetworkCF","Graph co-occurrence relation between:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE),
    sliderInput("A_6_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))})

  output$Comp_B_6_controls <- renderUI({button_choices = get_EVENT_CF()
  tags$div(
    radioButtons("B_6_OtherNetworkCF","Graph co-occurrence relation between:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE),
    sliderInput("B_6_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))})

  output$Comp_A_6 <- renderVisNetwork({
    req(input$A_6_Theshold)
    n = normalNetwork( threadedEventsComp_A(), selectOccFilter(), input$A_6_OtherNetworkCF )
    n=filter_network_edges(n,input$A_6_Theshold)
    circleVisNetwork( n )  })

  output$Comp_B_6 <- renderVisNetwork({
    req(input$B_6_Theshold)
    n = normalNetwork( threadedEventsComp_B(), selectOccFilter(), input$B_6_OtherNetworkCF )
    n=filter_network_edges(n,input$B_6_Theshold)
    circleVisNetwork( n ) })

  output$Comp_A_7_controls <- renderUI({checkboxGroupInput("A_7_CFs","Pick Two:", get_EVENT_CF() )})
  output$Comp_B_7_controls <- renderUI({checkboxGroupInput("B_7_CFs","Pick Two:", get_EVENT_CF() )})
  output$Comp_A_7 <- renderPlotly({ role_map( threadedEventsComp_A(), selectOccFilter(), input$A_7_CFs ) })
  output$Comp_B_7 <- renderPlotly({ role_map( threadedEventsComp_A(), selectOccFilter(), input$B_7_CFs ) })

  output$Comp_A_8 <- renderPlotly({ threadTrajectory(threadedEventsComp_A() ) })
  output$Comp_B_8 <- renderPlotly({ threadTrajectory(threadedEventsComp_B() ) })

# ##########  DIACHRONIC Comparison sub-tab   ###########
  output$Diachronic_Comparison_Tab_Controls_1 <- renderUI({
    tagList(  selectizeInput("DiaCompareMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names() ),
              selectizeInput('comparePanelViz',label = h4('Choose visualization:'),
                             c('Role Maps','Thread Trajectories','Threads (event time)','Ngrams' )))  })

  output$Diachronic_Comparison_Tab_Controls_3 <- renderUI({
    tagList(
      radioButtons("DiaCompareTimeSubsetID", "How many time intervals to compare:", choices = c(1, 2, 3, 4, 5, 6), selected="1", inline=TRUE),
      checkboxGroupInput("role_map_cfs","Pick Two for role map:", get_EVENT_CF() ) )
  })


# controls for the comparison input panels
  # Use all of the column names here...
  output$Diachronic_Comparison_Tab_Controls_4 <- renderUI({
    selectizeInput("selectComparisonID","Compare by:", get_COMPARISON_CF() ) })

  output$Diachronic_Comparison_Tab_Controls_5 <- renderUI({
    selectizeInput("selectComparisonGroupsID","Compare specific groups:",
                   CF_levels(), multiple=TRUE) })

 # Get subsets of events and create sub-plots for them

  output$DiachronicComparisonPlots <- renderPlotly({
    req(input$selectComparisonGroupsID)
    Comparison_Plots(threadedEventsDiaComp(),
                     selectOccFilter(),
                     input$selectComparisonID,
                     input$selectComparisonGroupsID,
                     input$DiaCompareTimeSubsetID,
                     input$comparePanelViz,
                     input$role_map_cfs)})
