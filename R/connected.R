# TODO

connected_ui = function(id) {
  ns = NS(id)

  sidebarLayout(
    sidebarPanel(
      h5('Display options'),
      uiOutput(ns('ui_input')),
      # uiOutput(ns('ui_counts')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Key Outcomes',
          uiOutput(ns('round_text_kpis')),
          h4('Overall'),
          plotOutput(ns('plot_kpis_overall'), height = '800px')#,
          # h4('Trends'),
          # plotlyOutput(
          #   ns('plot_kpis_trends_ace'), width = '70%', height = '350px'),
          # plotlyOutput(
          #   ns('plot_kpis_trends_beg'), width = '70%', height = '350px'),
          # plotlyOutput(
          #   ns('plot_kpis_trends_imp'), width = '56%', height = '350px')
        ),
        tabPanel(
          title = 'Detailed Outcomes',
          uiOutput(ns('round_text_detailed')),
          br(),
          plotlyOutput(ns('plot_detailed'))
        )
      ),
      width = 9
    )
  )
}

connected_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    data_proc = reactive({
      req(data_raw)
      get_data_connected(data_raw())
    })

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
    output$round_text_kpis = output$round_text_detailed = renderUI({
      req(data_filt)
      if (isTRUE(input$show_narrative)) get_round_text(data_filt())
    })

    # plot for KPIs
    output$plot_kpis_overall = renderPlot({
      req(data_filt)

      data_wide = copy(data_filt()$data_wide)
      data_wide[, treatment_name := str_wrap(treatment_name, 20)]

      data_long = copy(data_filt()$data_long)
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      fig_ace = get_barplot_summary(
        data_long, col = 'level_ace', fills = get_fills('ace'),
        title = 'Numeracy: division level', by_treatment = input$by_treatment)

      fig_beg = get_barplot_summary(
        data_long, col = 'level_beginner', fills = get_fills('beginner'),
        title = 'Innumeracy: beginner level',
        by_treatment = input$by_treatment) +
        theme(axis.title.y = element_blank())

      fig_imp = get_barplot_summary(
        data_wide, col = 'level_improved', fills = get_fills('improved'),
        title = 'Improved at least one level',
        by_treatment = input$by_treatment)

      # use cowplot::plot_grid() to arrange plots
      fig_ace_beg = plot_grid(
        fig_ace, fig_beg, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 0.95))

      fig_imp_null = plot_grid(
        fig_imp, grid::nullGrob(), nrow = 1L, rel_widths = c(1, 1))

      plot_grid(fig_ace_beg, fig_imp_null, ncol = 1L)
    }) |>
      bindCache(input$round_ids, input$by_treatment)

    # plot for A/B detailed results
    output$plot_detailed = renderPlotly({
      req(data_filt, !is.null(input$by_treatment))

      y = if (input$by_treatment) 1.1 else 1
      anno = list(x = 0, y = y, text = 'All levels')
      marg = if (input$by_treatment) list(t = 50) else NULL

      fig = get_barplot_detailed(
        data_filt()$data_long, col = 'level_name', fills = get_fills('full'),
        by_treatment = input$by_treatment)
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marg)
    }) |>
      bindCache(input$round_ids, input$by_treatment)
  })
}
