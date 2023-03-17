# connected_ab_detailed module creates and displays ConnectEd A/B detailed results

connected_detailed_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Detailed Results',
    br(),
    uiOutput(ns('ui_input')),
    uiOutput(ns('round_text')),
    plotOutput(ns('plot_detailed'))
  )
}

connected_detailed_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    # A/B detailed results for a single round
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$rounds)

      tagList(
        pickerInput(
          inputId = ns('round_ids'),
          choices = choices,
          selected = tail(choices, n = 1L)
        ),
        checkboxInput(
          inputId = ns('show_narrative'),
          label = 'Show narrative',
          value = TRUE)
      )
    })

    data_filt = reactive({
      req(data_proc, input$round_ids)
      filt = CJ(round_id = input$round_ids)
      get_data_filtered(data_proc(), filt)
    })

    # narrative text for the selected round
    output$round_text = renderUI({
      req(data_filt)
      if (isTRUE(input$show_narrative)) get_round_text(data_filt())
    })

    # plot for A/B detailed results
    output$plot_detailed = renderPlot({
      req(data_filt)
      data_long_now = data_filt()$data_long
      get_detailed_barplot(
        data_long_now, col = 'level_name', fills = get_levels_fills(),
        title = 'All levels', by_treatment = TRUE)
    }) |>
      bindCache(input$round_ids)
  })
}
