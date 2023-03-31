# TODO

connected_ui = function(id) {
  ns = NS(id)
  ht = 300

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
            ns('plot_kpis'), height = glue('{ht*3}px'), width = '80%'),
        ),
        tabPanel(
          title = 'Detailed Outcomes',
          uiOutput(ns('round_text_detailed')),
          plotlyOutput(
            ns('plot_detailed'), height = glue('{ht + 50}px'), width = '95%')
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
          label = 'Split selected round\'s results by treatment',
          value = TRUE),
        # checkboxInput(
        #   inputId = ns('show_pooled'),
        #   label = 'Show pooled results from all rounds',
        #   value = TRUE)
        # checkboxInput(
        #   inputId = ns('show_narrative'),
        #   label = 'Show narrative',
        #   value = TRUE)
      )
    })

    data_pool = reactive({
      req(data_proc)
      data_pool = get_data_filtered(data_proc())
      treatment_now = 'All Rounds,\nAll Treatments'
      data_pool$data_long[, treatment_wrap := treatment_now]
      data_pool$data_wide[, treatment_wrap := treatment_now]
      data_pool$data_long[, treatment_id := 'zzz']
      data_pool$data_wide[, treatment_id := 'zzz']
      data_pool
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

    output$plot_kpis = renderPlotly({
      req(data_filt)
      data_long = copy(data_filt()$data_long)
      data_wide = copy(data_filt()$data_wide)

      if (isFALSE(input$by_treatment)) {
        data_long[, treatment_id := '0']
        data_wide[, treatment_id := '0']
        round_name = data_long$round_name[1L]
        treatment_now = glue('Round {round_name},\nAny Treatment')
        data_long[, treatment_wrap := treatment_now]
        data_wide[, treatment_wrap := treatment_now]
      }

      data_long = rbind(data_long, data_pool()$data_long)
      data_wide = rbind(data_wide, data_pool()$data_wide)
      get_plot_kpis(data_long, data_wide)
    }) |>
      bindCache(input$round_ids, input$by_treatment)

    # plot for A/B detailed results
    output$plot_detailed = renderPlotly({
      req(data_filt)

      data_long = copy(data_filt()$data_long)
      if (isFALSE(input$by_treatment)) {
        data_long[, treatment_id := '0']
        round_name = data_long$round_name[1L]
        treatment_now = glue('Round {round_name},\nEither Treatment')
        data_long[, treatment_wrap := treatment_now]
      }

      data_long = rbind(data_long, data_pool()$data_long)

      fig = get_barplot_detailed(
        data_long, col = 'level_name', fills = get_fills('full'),
        by_treatment = TRUE)
      fig = ggplotly(fig, tooltip = 'text')

      # if (input$by_treatment) {
        fig = facet_strip_bigger(fig)
        y = 1.2
        marj = list(t = 65)
      # } else {
      #   y = 1
      #   marj = NULL
      # }

      anno = c(list(x = 0, y = y, text = 'All levels'), anno_base)
      lej = list(tracegroupgap = 0)
      layout(fig, annotations = anno, margin = marj, legend = lej)
    }) |>
      bindCache(input$round_ids, input$by_treatment)
  })
}
