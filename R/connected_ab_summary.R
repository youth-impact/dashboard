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
        h6(textOutput(ns('round_text'))), # output$round_text
        tableOutput(ns('treatment_students')), # output$treatment_students
        br(),
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
      choices = get_choices(data_proc()$data)

      radioButtons(
        inputId = ns('round_ids'),
        label = 'Round',
        choices = choices,
        selected = tail(choices, n = 1L))
    })

    # narrative text for the selected round
    output$round_text = renderText({
      req(data_proc, input$round_ids)
      get_round_text(data_proc()$rounds, input$round_ids)
    }) |>
      bindCache(input$round_ids)

    # combined plots for A/B summary results
    output$plot_all = renderPlot({
      req(data_proc, input$round_ids)

      data = data_proc()$data[round_id %in% input$round_ids]
      data_long = data_proc()$data_long[round_id %in% input$round_ids]

      data[, treatment_name := str_wrap(treatment_name, 20)]
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      p_add = get_summary_barplot(
        data_long, col = 'cannot_add', title = 'Innumeracy', nudge_y = 0.01,
        fill_vals = c('#a6cee3', '#1f78b4'), by_treatment = TRUE)

      p_div = get_summary_barplot(
        data_long, col = 'can_divide', title = 'Numeracy', nudge_y = 0.01,
        fill_vals = c('#b2df8a', '#33a02c'), by_treatment = TRUE)

      p_imp = get_summary_barplot(
        data, col = 'improved', title = 'Improved', nudge_y = 0.02,
        fill_vals = '#fdbf6f', by_treatment = TRUE)

      # use cowplot::plot_grid() to arrange plots
      p_add_div = plot_grid(p_div, p_add, nrow = 1L, align = 'h', axis = 'tblr')

      p_imp_null = plot_grid(
        p_imp, grid::nullGrob(), nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 1))

      plot_grid(p_imp_null, p_add_div, ncol = 1L)
    }) |>
      bindCache(input$round_ids, keep_missing())

    output$treatment_students = renderTable({
      req(data_proc, input$round_ids)
      get_counts_by_treatment(data_proc()$data[round_id %in% input$round_ids])
    }, align = 'lr')
  })
}
