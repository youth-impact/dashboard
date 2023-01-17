server = function(input, output, session) {

  output$plot1 = renderPlot({

    p1 = get_pooled_barplot(
      results_long, rounds = input$rounds, col = 'can_add', col_val = FALSE,
      title = 'Cannot add', nudge_y = 0.007, fill_vals = c('#a6cee3', '#1f78b4'))

    p2 = get_pooled_barplot(
      results_long, rounds = input$rounds, col = 'can_divide', col_val = TRUE,
      title = 'Can add, subtract,\nmultiply, and divide', nudge_y = 0.02,
      fill_vals = c('#b2df8a', '#33a02c'))

    p3 = get_pooled_barplot(
      results, rounds = input$rounds, col = 'improved', col_val = TRUE,
      title = 'Learned a\nnew operation', nudge_y = 0.05, fill_vals = '#fdbf6f',
      by_time = FALSE)

    plot_grid(
      p1, p2, p3, nrow = 1L, align = 'h', axis = 'tblr',
      rel_widths = c(1, 1, 0.75))
  })

}
