# connected_pooled module creates and displays ConnectEd pooled results

connected_pooled_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Pooled Summary Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')), # output$ui_input
        width = 2),

      mainPanel(
        br(),
        plotOutput(ns('plot_all')), # output$plot_all
        tableOutput(ns('round_students')), # output$round_students
        width = 10)
    )
  )
}

connected_pooled_server = function(id, data_proc, keep_missing) {
  moduleServer(id, function(input, output, session) {

    # pooled results can include one or more rounds
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$rounds)

      checkboxGroupInput(
        inputId = ns('round_ids'),
        label = strong('Round'),
        choices = choices,
        selected = choices)
    })

    # combined plots for pooled results
    output$plot_all = renderPlot({
      req(data_proc, input$round_ids)

      data = data_proc()$data[round_id %in% input$round_ids]
      data_long = data_proc()$data_long[round_id %in% input$round_ids]

      p_imp = get_summary_barplot(
        data, col = 'improved', fill_vals = '#fdbf6f',
        title = str_wrap('Improved: learned a new operation', 18))

      p_div = get_summary_barplot(
        data_long, col = 'can_divide', fill_vals = c('#b2df8a', '#33a02c'),
        title = str_wrap(
          'Numeracy: can add, subtract, multiply, and divide', 30)) +
        theme(axis.title.y = element_blank())

      p_add = get_summary_barplot(
        data_long, col = 'cannot_add', fill_vals = c('#a6cee3', '#1f78b4'),
        title = str_wrap('Innumeracy: cannot add', 30)) +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      plot_grid(
        p_imp, p_div, p_add, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(0.7, 1, 1))
    }) |>
      bindCache(input$round_ids, keep_missing())

    output$round_students = renderTable({
      req(data_proc, input$round_ids)
      get_counts_by_round(data_proc()$data[round_id %in% input$round_ids])
    }, align = 'r')
  })
}
