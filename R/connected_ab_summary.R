# connected_ab_summary module creates and displays ConnectEd A/B summary results

connected_ab_summary_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Single-round Results',
    br(),
        uiOutput(ns('ui_input')),
        uiOutput(ns('round_text')),
        plotOutput(ns('plot_all'), height = '800px')
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
        pickerInput(
          inputId = ns('round_ids'),
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

      str_wd = 40
      # https://waldyrious.net/viridis-palette-generator/
      # https://mdigi.tools/lighten-color/

      p_div = get_summary_barplot(
        data_long, col = 'level_division', #fills = c('#b2df8a', '#33a02c'),
        fills = c('#fef17c', '#fde725'),
        title = str_wrap('Numeracy: division level', str_wd),
        by_treatment = TRUE)

      p_beg = get_summary_barplot(
        data_long, col = 'level_beginner', #fills = c('#a6cee3', '#1f78b4'),
        fills = c('#66027e', '#440154'),
        title = str_wrap('Innumeracy: beginner level', str_wd),
        by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      p_imp = get_summary_barplot(
        data_wide, col = 'level_improved', fills = '#56B4E9',
        title = str_wrap('Learned a new operation', str_wd),
        by_treatment = TRUE, bar_width = 0.6)

      p_stack = get_detailed_barplot(
        data_long, col = 'level_name', title = 'All levels',
        by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      p_div_beg = plot_grid(p_div, p_beg, nrow = 1L, align = 'h', axis = 'tb')

      p_imp_stack = plot_grid(
        p_imp, p_stack, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(0.8, 1.2))

      # plot_grid(
      #   plot_grid(p_div_beg, grid::nullGrob(), rel_widths = c(1, 0.13)),
      #   p_imp_stack, ncol = 1L)
      plot_grid(p_div_beg, p_imp_stack, ncol = 1L)
    }) |>
      bindCache(input$round_ids, keep_missing())
  })
}
