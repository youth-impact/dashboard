server = function(input, output, session) {

  output$plot1 = renderPlot({
    p1 = get_summary_barplot(
      conn$data_long, rounds = input$rounds, col = 'can_add', col_val = FALSE,
      title = 'Innumeracy:\ncannot add',
      nudge_y = 0.007, fill_vals = c('#a6cee3', '#1f78b4'))

    p2 = get_summary_barplot(
      conn$data_long, rounds = input$rounds, col = 'can_divide', col_val = TRUE,
      title = 'Numeracy:\ncan add, subtract,\nmultiply, and divide',
      nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'))

    d = copy(conn$data)[, time := 'Sensitization\nto Endline']
    p3 = get_summary_barplot(
      d, rounds = input$rounds, col = 'improved', col_val = TRUE,
      title = 'Improved:\nlearned a new\noperation',
      nudge_y = 0.04, fill_vals = '#fdbf6f')

    plot_grid(
      p1, p2, p3, nrow = 1L, align = 'h', axis = 'tblr',
      rel_widths = c(1, 1, 0.75))
  })

  output$plot2 = renderPlot({
    d = copy(conn$data_long)[, present := TRUE]
    p = get_summary_barplot(
      d, rounds = input$rounds, col = 'present', col_val = TRUE,
      title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
      by_arm = FALSE, percent = FALSE)
    p
  })

  output$plot3 = renderPlot({
    d = copy(conn$data_long)
    setattr(d$time, 'levels', c('Sens.', 'Endline'))

    p1 = get_summary_barplot(
      d, rounds = input$round_summ, col = 'can_add', col_val = FALSE,
      title = 'Innumeracy', nudge_y = 0.01, fill_vals = c('#a6cee3', '#1f78b4'),
      by_arm = TRUE)

    p2 = get_summary_barplot(
      d, rounds = input$round_summ, col = 'can_divide', col_val = TRUE,
      title = 'Numeracy', nudge_y = 0.02, fill_vals = c('#b2df8a', '#33a02c'),
      by_arm = TRUE)

    d = copy(conn$data)[, time := 'Sens. to\nEndline']
    p3 = get_summary_barplot(
      d, rounds = input$round_summ, col = 'improved', col_val = TRUE,
      title = 'Improved', nudge_y = 0.04, fill_vals = '#fdbf6f', by_arm = TRUE)

    plot_grid(
      p1, p2, p3, nrow = 1L, align = 'h', axis = 'tblr',
      rel_widths = c(1, 1, 0.75))
  })

  output$plot4 = renderPlot({
    d = copy(conn$data_long)[, present := TRUE]
    setattr(d$time, 'levels', c('Sens.', 'Endline'))
    p = get_summary_barplot(
      d, rounds = input$round_summ, col = 'present', col_val = TRUE,
      title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
      by_arm = TRUE, percent = FALSE)
    p
  })

  output$plot5 = renderPlot({
    d = copy(conn$data_long)
    setattr(d$time, 'levels', c('Sens.', 'Endline'))
    p = get_detailed_barplot(
      d, rounds = input$round_detl, col = 'level_name',
      by_arm = TRUE, percent = input$y_detl == 'percentages')
    p
  })


}
