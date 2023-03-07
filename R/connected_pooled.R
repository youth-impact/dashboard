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
        plotOutput(ns('plot_all'), height = '800px'), # output$plot_all
        width = 10)
    )
  )
}

connected_pooled_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    # pooled results can include one or more rounds
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$data)
      checkboxGroupInput(
        inputId = ns('round_ids'),
        label = 'Round(s)',
        choices = choices,
        selected = choices)
    })

    # combined plots for pooled results
    output$plot_all = renderPlot({
      req(data_proc, input$round_ids)

      data = data_proc()$data[round_id %in% input$round_ids]
      data_long = data_proc()$data_long[round_id %in% input$round_ids]

      p_add = get_summary_barplot(
        data_long, col = 'cannot_add', title = 'Innumeracy: cannot add',
        nudge_y = 0.007, fill_vals = c('#a6cee3', '#1f78b4'))

      p_div = get_summary_barplot(
        data_long, col = 'can_divide',
        title = 'Numeracy: can add,\nsubtract, multiply, and divide',
        nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'))

      p_imp = get_summary_barplot(
        data, col = 'improved', title = 'Improved: learned\na new operation',
        nudge_y = 0.04, fill_vals = '#fdbf6f')

      p_tot = get_summary_barplot(
        data_long, col = 'present', title = 'Totals', nudge_y = 0,
        fill_vals = c('#cab2d6', '#6a3d9a'), by_treatment = FALSE,
        percent = FALSE)

      # use cowplot::plot_grid() to arrange plots
      p_add_div = plot_grid(p_add, p_div, nrow = 1L, align = 'h', axis = 'tblr')

      p_imp_tot = plot_grid(
        p_imp, grid::nullGrob(), p_tot, nrow = 1L,
        align = 'h', axis = 'tb', rel_widths = c(0.7, 0.32, 0.98))

      plot_grid(p_add_div, p_imp_tot, ncol = 1L)
    }) |>
      bindCache(input$round_ids)
  })
}
