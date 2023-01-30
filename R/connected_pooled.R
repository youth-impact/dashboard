connected_pooled_ui = function(id) {
  ns = NS(id)

  tabPanel(
    'Pooled Summary Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')),
        width = 2),

      mainPanel(
        plotOutput(ns('plot1'), height = '800px'),
        width = 10)
    )
  )
}

connected_pooled_server = function(id, conn) {
  moduleServer(id, function(input, output, session) {

    d = copy(conn$data)[, time := 'Sensitization\nto Endline']
    d_long = copy(conn$data_long)
    rounds_avail = sort(unique(conn$data$round_id))

    output$ui_input = renderUI({
      ns = session$ns
      checkboxGroupInput(
        inputId = ns('round_ids'),
        label = 'Round(s)',
        choices = rounds_avail,
        selected = rounds_avail)
    })

    output$plot1 = renderPlot({
      req(input$round_ids)

      p1 = get_summary_barplot(
        d_long, input$round_ids, col = 'can_add', col_val = FALSE,
        title = 'Innumeracy: cannot add',
        nudge_y = 0.007, fill_vals = c('#a6cee3', '#1f78b4'))

      p2 = get_summary_barplot(
        d_long, input$round_ids, col = 'can_divide', col_val = TRUE,
        title = 'Numeracy: can add,\nsubtract, multiply, and divide',
        nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'))

      p3 = get_summary_barplot(
        d, input$round_ids, col = 'improved', col_val = TRUE,
        title = 'Improved: learned\na new operation',
        nudge_y = 0.04, fill_vals = '#fdbf6f')

      p4 = get_summary_barplot(
        d_long, input$round_ids, col = 'present', col_val = TRUE,
        title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
        by_arm = FALSE, percent = FALSE)

      p12 = plot_grid(p1, p2, nrow = 1L, align = 'h', axis = 'tblr')

      p34 = plot_grid(
        p3, grid::nullGrob(), p4, nrow = 1L,
        align = 'h', axis = 'tb', rel_widths = c(0.7, 0.32, 0.98))

      plot_grid(p12, p34, ncol = 1L)
    }) |>
      bindCache(input$round_ids)
  })
}
