server = function(input, output, session) {

  output$plot1 = renderPlot({
    p1 = get_summary_barplot(
      conn$data_long, input$round_ids, col = 'can_add', col_val = FALSE,
      title = 'Innumeracy:\ncannot add',
      nudge_y = 0.007, fill_vals = c('#a6cee3', '#1f78b4'))

    p2 = get_summary_barplot(
      conn$data_long, input$round_ids, col = 'can_divide', col_val = TRUE,
      title = 'Numeracy:\ncan add, subtract,\nmultiply, and divide',
      nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'))

    plot_grid(p1, p2, nrow = 1L, align = 'h', axis = 'tblr')
  })

  output$plot2 = renderPlot({
    d = copy(conn$data)[, time := 'Sensitization\nto Endline']
    p1 = get_summary_barplot(
      d, input$round_ids, col = 'improved', col_val = TRUE,
      title = 'Improved:\nlearned a new\noperation',
      nudge_y = 0.04, fill_vals = '#fdbf6f')

    d = copy(conn$data_long)[, present := TRUE]
    p2 = get_summary_barplot(
      d, input$round_ids, col = 'present', col_val = TRUE,
      title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
      by_arm = FALSE, percent = FALSE)

    plot_grid(
      p1, grid::nullGrob(), p2, nrow = 1L,
      align = 'h', axis = 'tb', rel_widths = c(0.7, 0.32, 0.98))
  })

  output$plot3 = renderPlot({
    d = copy(conn$data_long)
    setattr(d$time, 'levels', c('Sens.', 'Endline'))

    p1 = get_summary_barplot(
      d, input$round_id_summ, col = 'can_add', col_val = FALSE,
      title = 'Innumeracy', nudge_y = 0.01, fill_vals = c('#a6cee3', '#1f78b4'),
      by_arm = TRUE)

    p2 = get_summary_barplot(
      d, input$round_id_summ, col = 'can_divide', col_val = TRUE,
      title = 'Numeracy', nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'),
      by_arm = TRUE)

    plot_grid(p1, p2, nrow = 1L, align = 'h', axis = 'tblr')
  })

  output$plot4 = renderPlot({
    d = copy(conn$data)[, time := 'Sens. to\nEndline']
    p1 = get_summary_barplot(
      d, input$round_id_summ, col = 'improved', col_val = TRUE,
      title = 'Improved', nudge_y = 0.04, fill_vals = '#fdbf6f', by_arm = TRUE)

    d = copy(conn$data_long)[, present := TRUE]
    setattr(d$time, 'levels', c('Sens.', 'Endline'))
    p2 = get_summary_barplot(
      d, input$round_id_summ, col = 'present', col_val = TRUE,
      title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
      by_arm = TRUE, percent = FALSE)

    plot_grid(
      p1, grid::nullGrob(), p2, nrow = 1L,
      align = 'h', axis = 'tb', rel_widths = c(0.8, 0.23, 0.97))
  })

  output$plot5 = renderPlot({
    d = copy(conn$data_long)
    setattr(d$time, 'levels', c('Sens.', 'Endline'))
    p = get_detailed_barplot(
      d, input$round_id_detl, col = 'level_name', by_arm = TRUE,
      percent = input$y_detl == 'percentages')
    p
  })

}
