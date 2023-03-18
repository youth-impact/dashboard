# connected_ab_detailed module creates and displays ConnectEd A/B detailed results

connected_detailed_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Detailed Results',
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('ui_input')),
        width = 3
      ),
      mainPanel(
        br(),
        uiOutput(ns('round_text')),
        plotlyOutput(ns('plot_detailed')),
        width = 9
      )
    )
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
          inputId = ns('by_treatment'),
          label = 'Split results by treatment',
          value = TRUE),
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
    output$plot_detailed = renderPlotly({
      req(data_filt, input$by_treatment)

      y = if (input$by_treatment) 1.1 else 1
      annos = list(c(list(x = 0, y = y, text = 'All levels'), anno_base))
      marg = if (input$by_treatment) list(t = 50) else NULL

      fig = get_barplot_detailed(
        data_filt()$data_long, col = 'level_name', fills = get_fills('full'),
        by_treatment = input$by_treatment)
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = annos, margin = marg)
    }) |>
      bindCache(input$round_ids, input$by_treatment)
  })
}
