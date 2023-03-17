#

connected_kpis_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Key Performance Indicators',
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('ui_input')),
        width = 3
      ),

      mainPanel(
        br(),
        uiOutput(ns('round_text')),
        plotOutput(ns('plot_kpis'), height = '800px'),
        width = 9
      )
    )
  )
}

connected_kpis_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    # A/B detailed results for a single round
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$rounds)

      tagList(
        pickerInput(
          inputId = ns('round_ids'),
          choices = choices,
          selected = tail(choices, n = 1L)
        ),
        checkboxInput(
          inputId = ns('by_treatment'),
          label = 'Split results by treatment',
          value = TRUE),
        checkboxInput(
          inputId = ns('show_narrative'),
          label = 'Show narrative',
          value = TRUE)
      )
    })

    data_filt = reactive({
      req(data_proc, input$round_ids)
      filt = CJ(round_id = input$round_ids)
      get_data_filtered(data_proc(), filt)
    })

    # narrative text for the selected round
    output$round_text = renderUI({
      req(data_filt)
      if (isTRUE(input$show_narrative)) get_round_text(data_filt())
    })

    # plot for KPIs
    output$plot_kpis = renderPlot({
      req(data_filt)

      data_wide = copy(data_filt()$data_wide)
      data_wide[, treatment_name := str_wrap(treatment_name, 20)]

      data_long = copy(data_filt()$data_long)
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      p_div = get_summary_barplot(
        data_long, col = 'level_division', fills = c('#a6cee3', '#1f78b4'),
        title = 'Numeracy: division level', by_treatment = input$by_treatment)

      p_beg = get_summary_barplot(
        data_long, col = 'level_beginner', fills = c('#fb9a99', '#e31a1c'),
        title = 'Innumeracy: beginner level',
        by_treatment = input$by_treatment) +
        theme(axis.title.y = element_blank())

      p_imp = get_summary_barplot(
        data_wide, col = 'level_improved', fills = '#33a02c',
        title = 'Learned a new operation', by_treatment = input$by_treatment)

      # use cowplot::plot_grid() to arrange plots
      p_div_beg = plot_grid(
        p_div, p_beg, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 0.95))

      p_imp_null = plot_grid(
        p_imp, grid::nullGrob(), nrow = 1L, rel_widths = c(1, 1))

      plot_grid(p_div_beg, p_imp_null, ncol = 1L)
    }) |>
      bindCache(input$round_ids, input$by_treatment)
  })
}
