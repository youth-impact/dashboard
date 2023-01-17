server = function(input, output, session) {

  # x = get_results()
  # results = x[[1L]]
  # results_long = x[[2]]

  output$plot1 = renderPlot({
    r = results_long[round_id %in% input$rounds, .N, keyby = .(time, can_add)]
    r[, frac_students := N / sum(N), keyby = time]
    r[, perc_label := paste0(round(100 * frac_students), '%')]

    p1 = ggplot(r[can_add == FALSE], aes(x = time, y = frac_students)) +
      geom_col(aes(fill = time), width = 0.7) +
      geom_text(aes(label = perc_label), size = 5.5, nudge_y = 0.007) +
      labs(x = 'Timepoint', y = 'Percentage of students',
           title = 'Cannot add') +
      scale_y_continuous(labels = scales::label_percent()) +
      scale_fill_manual(values = c('#a6cee3', '#1f78b4')) +
      theme(legend.position = 'none', axis.title.x = element_blank())

    r = results_long[
      round_id %in% input$rounds, .N, keyby = .(time, can_divide)]
    r[, frac_students := N / sum(N), keyby = time]
    r[, perc_label := paste0(round(100 * frac_students), '%')]

    p2 = ggplot(r[can_divide == TRUE], aes(x = time, y = frac_students)) +
      geom_col(aes(fill = time), width = 0.7) +
      geom_text(aes(label = perc_label), size = 5.5, nudge_y = 0.02) +
      labs(x = 'Timepoint', y = 'Percentage of students',
           title = 'Can add, subtract,\nmultiply, and divide') +
      scale_y_continuous(labels = scales::label_percent()) +
      scale_fill_manual(values = c('#b2df8a', '#33a02c')) +
      theme(legend.position = 'none', axis.title.x = element_blank())

    r = results[round_id %in% input$rounds, .N, keyby = improved]
    r[, frac_students := N / sum(N)]
    r[, perc_label := paste0(round(100 * frac_students), '%')]
    r[, time := 'Sensitization →\nEndline']

    p3 = ggplot(r[improved == TRUE], aes(x = time, y = frac_students)) +
      geom_col(width = 0.7, fill = '#fdbf6f') +
      geom_text(aes(label = perc_label), size = 5.5, nudge_y = 0.05) +
      labs(x = 'Timepoint', y = 'Percentage of students',
           title = 'Learned a\nnew operation') +
      scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
      theme(axis.title.x = element_blank())

    plot_grid(
      p1, p2, p3, nrow = 1L, align = 'h', axis = 'tblr',
      rel_widths = c(1, 1, 0.75))
  })

}
