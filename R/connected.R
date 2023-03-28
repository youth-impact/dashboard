# TODO

connected_ui = function(id) {
  ns = NS(id)

  ht = 300
  height = glue('{ht}px')

  sidebarLayout(
    sidebarPanel(
      h5('Display options'),
      uiOutput(ns('ui_input')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Key Outcomes',
          uiOutput(ns('round_text_kpis')),
          plotlyOutput(
            ns('plot_kpis_overall'), height = glue('{ht * 3}px'), width = '60%')
        ),
        tabPanel(
          title = 'Detailed Outcomes',
          uiOutput(ns('round_text_detailed')),
          plotlyOutput(ns('plot_detailed'), height = glue('{ht + 50}px'))
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
        # checkboxInput(
        #   inputId = ns('show_narrative'),
        #   label = 'Show narrative',
        #   value = TRUE)
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
      get_round_text(data_filt())
      # if (isTRUE(input$show_narrative)) get_round_text(data_filt())
    })

    # plot for KPIs
    output$plot_kpis_overall = renderPlotly({
      req(data_filt)

      data_wide = copy(data_filt()$data_wide)
      data_wide[, treatment_name := str_wrap(treatment_name, 20)]

      data_long = copy(data_filt()$data_long)
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      yaxis = list(title = 'Share of students (%)', titlefont = list(size = 20))

      fig = get_barplot_summary(
        data_long, col = 'level_ace', fills = get_fills('ace'),
        by_treatment = input$by_treatment)
      fig_ace = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = yaxis)

      fig = get_barplot_summary(
        data_long, col = 'level_beginner', fills = get_fills('beginner'),
        by_treatment = input$by_treatment)
      fig_beginner = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = yaxis)

      fig = get_barplot_summary(
        data_wide, col = 'level_improved', fills = get_fills('improved'),
        y_lims = c(0, 100), by_treatment = input$by_treatment, bar_width = 0.5)
      fig_improved = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = yaxis)

      if (input$by_treatment) {
        fig_ace = facet_strip_bigger(fig_ace)
        fig_beginner = facet_strip_bigger(fig_beginner)
        fig_improved = facet_strip_bigger(fig_improved)
        y = c(1.055, 0.68, 0.3)
        heights = c(0.31, 0.38, 0.31)
        marj_subplot = 0.065
        marj_layout = list(t = 65)
      } else {
        y = c(1, 0.655, 0.275)
        heights = c(0.31, 0.38, 0.31)
        marj_subplot = 0.035
        marj_layout = list(t = 20)
      }

      annos = list(
        list(x = 0, y = y[1L], text = 'Numeracy: division level'),
        list(x = 0, y = y[2L], text = 'Innumeracy: beginner level'),
        list(x = 0, y = y[3L], text = 'Improved at least one level'))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beginner, fig_improved, nrows = 3L,
        heights = heights, margin = marj_subplot, titleY = TRUE) |>
        layout(annotations = annos, margin = marj_layout)
    }) |>
      bindCache(input$round_ids, input$by_treatment)

    # plot for A/B detailed results
    output$plot_detailed = renderPlotly({
      req(data_filt, !is.null(input$by_treatment))

      y = if (input$by_treatment) 1.12 else 1
      anno = list(x = 0, y = y, text = 'All levels')
      marj = if (input$by_treatment) list(t = 50) else NULL
      lej = list(tracegroupgap = 0)

      fig = get_barplot_detailed(
        data_filt()$data_long, col = 'level_name', fills = get_fills('full'),
        by_treatment = input$by_treatment)

      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj, legend = lej)
    }) |>
      bindCache(input$round_ids, input$by_treatment)
  })
}
