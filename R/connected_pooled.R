connected_pooled_ui = function(id) {
  ns = NS(id)

  tabPanel(
    'Pooled Summary Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')),
        width = 2),

      mainPanel(
        plotOutput(ns('plot_all'), height = '800px'),
        width = 10)
    )
  )
}

connected_pooled_server = function(id, connected) {
  moduleServer(id, function(input, output, session) {

    d = copy(connected$data)[, time := 'Sensitization\nto Endline']
    d_long = copy(connected$data_long)
    rounds_avail = sort(unique(connected$data$round_id))

    output$ui_input = renderUI({
      ns = session$ns
      checkboxGroupInput(
        inputId = ns('round_ids'),
        label = 'Round(s)',
        choices = rounds_avail,
        selected = rounds_avail)
    })

    output$plot_all = renderPlot({
      req(input$round_ids)

      p_add = get_summary_barplot(
        d_long, input$round_ids, col = 'can_add', col_val = FALSE,
        title = 'Innumeracy: cannot add',
        nudge_y = 0.007, fill_vals = c('#a6cee3', '#1f78b4'))

      p_div = get_summary_barplot(
        d_long, input$round_ids, col = 'can_divide', col_val = TRUE,
        title = 'Numeracy: can add,\nsubtract, multiply, and divide',
        nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'))

      p_imp = get_summary_barplot(
        d, input$round_ids, col = 'improved', col_val = TRUE,
        title = 'Improved: learned\na new operation',
        nudge_y = 0.04, fill_vals = '#fdbf6f')

      p_tot = get_summary_barplot(
        d_long, input$round_ids, col = 'present', col_val = TRUE,
        title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
        by_arm = FALSE, percent = FALSE)

      p_row1 = plot_grid(p_add, p_div, nrow = 1L, align = 'h', axis = 'tblr')

      p_row2 = plot_grid(
        p_imp, grid::nullGrob(), p_tot, nrow = 1L,
        align = 'h', axis = 'tb', rel_widths = c(0.7, 0.32, 0.98))

      plot_grid(p_row1, p_row2, ncol = 1L)
    }) |>
      bindCache(input$round_ids)
  })
}
