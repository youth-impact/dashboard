# connected_ab_summary module creates and displays ConnectEd A/B summary results

connected_ab_summary_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'A/B Summary Results',
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
          label = 'Show details')
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
          data_proc()$data, input$round_ids)
      }
    })

    # combined plots for A/B summary results
    output$plot_all = renderPlot({
      req(data_proc, input$round_ids)

      data = data_proc()$data[round_id %in% input$round_ids]
      data_long = data_proc()$data_long[round_id %in% input$round_ids]

      data[, treatment_name := str_wrap(treatment_name, 20)]
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      p_imp = get_summary_barplot(
        data, col = 'improved', fill_vals = '#fdbf6f',
        title = str_wrap('Improved: learned a new operation', 40),
        by_treatment = TRUE)

      p_div = get_summary_barplot(
        data_long, col = 'can_divide', fill_vals = c('#b2df8a', '#33a02c'),
        title = str_wrap(
          'Numeracy: can add, subtract, multiply, and divide', 40),
        by_treatment = TRUE)

      p_add = get_summary_barplot(
        data_long, col = 'cannot_add', fill_vals = c('#a6cee3', '#1f78b4'),
        title = str_wrap('Innumeracy: cannot add', 40), by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      p_add_div = plot_grid(p_div, p_add, nrow = 1L, align = 'h', axis = 'tblr')

      p_imp_null = plot_grid(
        p_imp, grid::nullGrob(), nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 1))

      plot_grid(p_imp_null, p_add_div, ncol = 1L)
    }) |>
      bindCache(input$round_ids, keep_missing())
  })
}
