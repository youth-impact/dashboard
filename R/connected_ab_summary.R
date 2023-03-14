# connected_ab_summary module creates and displays ConnectEd A/B summary results

connected_ab_summary_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Detailed Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')), # output$ui_input
        width = 2),

      mainPanel(
        uiOutput(ns('round_header')),
        uiOutput(ns('round_text')),
        plotOutput(ns('plot_all'), height = '800px'), # output$plot_all
        width = 10)
    )
  )
}

connected_ab_summary_server = function(id, data_proc, keep_missing) {
  moduleServer(id, function(input, output, session) {

    # A/B summary results for a single round
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$rounds)

      tagList(
        radioButtons(
          inputId = ns('round_ids'),
          label = strong('Round'),
          choices = choices,
          selected = tail(choices, n = 1L)),
        checkboxInput(
          inputId = ns('show_details'),
          label = 'Show narrative',
          value = TRUE),
      )
    })

    output$round_header = renderUI({
      req(data_proc, input$round_ids)
      get_round_header(data_proc()$rounds, input$round_ids)
    })

    # narrative text for the selected round
    output$round_text = renderUI({
      req(data_proc, input$round_ids)
      if (isTRUE(input$show_details)) {
        get_round_text(
          data_proc()$rounds, data_proc()$arms, data_proc()$treatments,
          data_proc()$data_wide, input$round_ids)
      }
    })

    # combined plots for A/B summary results
    output$plot_all = renderPlot({
      req(data_proc, input$round_ids)

      data_wide = data_proc()$data_wide[round_id %in% input$round_ids]
      data_wide[, treatment_name := str_wrap(treatment_name, 20)]

      data_long = data_proc()$data_long[round_id %in% input$round_ids]
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      p_imp = get_summary_barplot(
        data_wide, col = 'improved', fill_vals = '#fdbf6f',
        title = str_wrap('Improved: learned a new operation', 40),
        by_treatment = TRUE, bar_width = 0.6)

      p_div = get_summary_barplot(
        data_long, col = 'can_divide', fill_vals = c('#b2df8a', '#33a02c'),
        title = str_wrap(
          'Numeracy: can add, subtract, multiply, and divide', 40),
        by_treatment = TRUE)

      p_add = get_summary_barplot(
        data_long, col = 'cannot_add', fill_vals = c('#a6cee3', '#1f78b4'),
        title = str_wrap('Innumeracy: cannot add', 40), by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      p_stack = get_detailed_barplot(
        data_long, col = 'level_name', title = 'All levels',
        by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      p_div_add = plot_grid(p_div, p_add, nrow = 1L, align = 'h', axis = 'tb')

      p_imp_stack = plot_grid(
        p_imp, p_stack, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(0.8, 1.2))

      plot_grid(
        plot_grid(p_div_add, grid::nullGrob(), rel_widths = c(1, 0.13)),
        p_imp_stack, ncol = 1L)

      # plot_grid(
      #   p_div, p_add, p_imp, nrow = 1L, align = 'h', axis = 'tb',
      #   rel_widths = c(1, 0.95, 0.9))
    }) |>
      bindCache(input$round_ids, keep_missing())
  })
}
