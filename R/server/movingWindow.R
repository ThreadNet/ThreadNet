# Server Output Functions for Moving Window Tab

  output$Moving_Window_Tab_Controls_1 <- renderUI({
    selectizeInput("MovingWindowMapInputID",label = h4("Choose mapping:"), get_event_mapping_names() )
  })

  output$Moving_Window_Tab_Controls_2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$MovingWindowMapInputID))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("MovingWindowZoomID",
                 label = h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })

  output$Moving_Window_Tab_Controls_3 <- renderUI({
    sliderInput("MovingWindowSizeID","Window Size", 1, numThreads(threadedEventsMove(),"threadNum" ),5,step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_3a <- renderUI({
    sliderInput("MovingWindowIncrement","Move window by", 1, input$MovingWindowSizeID, 1, step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_4_A <- renderUI({
    sliderInput("WindowLocation_A_ID","Window Location", 1,numThreads(threadedEventsMove(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_4_B <- renderUI({
    sliderInput("WindowLocation_B_ID","Window Location", 1,numThreads(threadedEventsMove(),"threadNum" ),1,step=1,ticks=FALSE )
  })

output$single_moving_window_timeline = renderPlotly({
    trace = window_correlation( threadedEventsMove(), input$MovingWindowSizeID, input$MovingWindowIncrement, 2)
    movingWindowCorrelation(  trace )  })

  output$dual_moving_window_timeline = renderPlotly({
    trace = dual_window_correlation( threadedEventsMove(), input$MovingWindowSizeID, input$MovingWindowIncrement, 2)
    dualmovingWindowCorrelation(  trace )  })

  ## Conditional controls for both side A and B
  output$Moving_4_controls <- renderUI({sliderInput("M_4_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})

  output$Moving_5_controls <- renderUI({sliderInput("M_5_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})

  output$Moving_6_controls <- renderUI({button_choices = get_EVENT_CF()
  tags$div(
    radioButtons("M_6_OtherNetworkCF","Graph co-occurrence relation between:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE),
    sliderInput("M_6_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))})


  output$Moving_7_controls <- renderUI({checkboxGroupInput("M_7_CFs","Pick Two:", get_EVENT_CF() )})


 ##  New moving window outputs
  #### Put all eight here #####
  output$Moving_A_1 <- renderPlotly({ threadMap(threadedEventsMove_A(), "threadNum", "seqNum", get_Zoom_MOVE(), 15  ) })
  output$Moving_B_1 <- renderPlotly({ threadMap(threadedEventsMove_B(), "threadNum", "seqNum", get_Zoom_MOVE(), 15  ) })

  output$Moving_A_2 <- renderPlotly({ threadMap(threadedEventsMove_A(), "threadNum", "tStamp", get_Zoom_MOVE(), 15 ) })
  output$Moving_B_2 <- renderPlotly({ threadMap(threadedEventsMove_B(), "threadNum", "tStamp", get_Zoom_MOVE(), 15 ) })

  output$Moving_A_3 <- renderPlotly({ threadMap(threadedEventsMove_A(), "threadNum", "relativeTime", get_Zoom_MOVE(), 15 ) })
  output$Moving_B_3 <- renderPlotly({ threadMap(threadedEventsMove_B(), "threadNum", "relativeTime", get_Zoom_MOVE(), 15 ) })

  output$Moving_A_4 <- renderVisNetwork({
    n = threads_to_network_original( threadedEventsMove_A(), "threadNum", get_Zoom_MOVE() )
    n=filter_network_edges(n,input$M_4_Theshold)
    circleVisNetwork( n ) })

  output$Moving_B_4 <- renderVisNetwork({
    n = threads_to_network_original( threadedEventsMove_B(), "threadNum", get_Zoom_MOVE() )
    n=filter_network_edges(n,input$M_4_Theshold)
    circleVisNetwork( n  ) })

  output$Moving_A_5 <- renderForceNetwork({
    n = threads_to_network_original( threadedEventsMove_A(), 'threadNum', get_Zoom_MOVE(), 'threadNum' )
    n = filter_network_edges(n,input$M_5_Theshold)
    forceNetworkD3( n )  })

  output$Moving_B_5 <- renderForceNetwork({
    n = threads_to_network_original( threadedEventsMove_B(), 'threadNum', get_Zoom_MOVE(), 'threadNum' )
    n = filter_network_edges(n,input$M_5_Theshold)
    forceNetworkD3( n )  })


  output$Moving_A_6 <- renderVisNetwork({
    n = normalNetwork( threadedEventsMove_A(), selectOccFilter(), input$M_6_OtherNetworkCF )
    n=filter_network_edges(n,input$M_6_Theshold)
    circleVisNetwork( n )  })

  output$Moving_B_6 <- renderVisNetwork({
    n = normalNetwork( threadedEventsMove_B(), selectOccFilter(), input$M_6_OtherNetworkCF )
    n=filter_network_edges(n,input$M_6_Theshold)
    circleVisNetwork( n ) })

  output$Moving_A_7 <- renderPlotly({ role_map( threadedEventsMove_A(), selectOccFilter(), input$M_7_CFs ) })
  output$Moving_B_7 <- renderPlotly({ role_map( threadedEventsMove_B(), selectOccFilter(), input$M_7_CFs ) })

  output$Moving_A_8 <- renderPlotly({ threadTrajectory(threadedEventsMove_A() ) })
  output$Moving_B_8 <- renderPlotly({ threadTrajectory(threadedEventsMove_B() ) })
