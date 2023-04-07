# TODO

connected_ui = function(id) {
  ns = NS(id)
  ht = 300

  sidebarLayout(
    sidebarPanel(
      h5('Display Options by Round'),
      uiOutput(ns('ui_input')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Program Overview',
          h4('All Rounds'),
          wellPanel(
            uiOutput(ns('overview_counts')),
            uiOutput(ns('overview_delta_kpis'))
          ),
          plotlyOutput(ns('overview_plot_kpis'), height = glue('{ht}px')),
          # br(),
        ),
        tabPanel(
          title = 'Key Outcomes by Round',
          uiOutput(ns('round_text_kpis')),
          plotlyOutput(
            ns('plot_kpis'), height = glue('{ht*3}px'), width = '80%'),
        ),
        tabPanel(
          title = 'Detailed Outcomes by Round',
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

    output$overview_counts = renderUI({
      req(data_proc)
      entity_cols = c(
        'student_id', 'facilitator_name', 'round_id',
        'treatment_id', 'region_baseline')
      entity_vals = data_proc()$data_wide[
        , lapply(.SD, \(x) scales::label_comma()(uniqueN(x))),
        .SDcols = entity_cols]
      entity_vals[, school_id := '77'] # placeholder

      sty_n = 'font-size:35px;'
      sty_unit = 'font-size:20px;'
      iccls = 'fa-2x'

      fluidRow(
        column(
          width = 2, align = 'center',
          strong(entity_vals$student_id, style = sty_n), HTML('&nbsp;'),
          icon('child-reaching', iccls), br(), p('Students', style = sty_unit)
        ),
        column(
          width = 2, align = 'center',
          strong(entity_vals$facilitator_name, style = sty_n), HTML('&nbsp;'),
          icon('person-chalkboard', iccls), br(),
          p('Facilitators', style = sty_unit)
        ),
        column(
          width = 2, align = 'center',
          strong(entity_vals$school_id, style = sty_n), HTML('&nbsp;'),
          icon('school', iccls), br(), p('Schools', style = sty_unit)
        ),
        column(
          width = 2, align = 'center',
          strong(entity_vals$region_baseline, style = sty_n), HTML('&nbsp;'),
          icon('map-location', iccls), br(), p('Regions', style = sty_unit)
        ),
        column(
          width = 2, align = 'center',
          strong(entity_vals$treatment_id, style = sty_n), HTML('&nbsp;'),
          icon('book', iccls), br(), p('Treatments', style = sty_unit)
        ),
        column(
          width = 2, align = 'center',
          strong(entity_vals$round_id, style = sty_n), HTML('&nbsp;'),
          icon('vials', iccls), br(), p('Rounds', style = sty_unit)
        )
      )
    })

    output$overview_plot_kpis = renderPlotly({
      req(data_pool)

      fig = get_barplot_summary(
        data_pool()$data_long, col = 'level_ace', fills = get_fills('ace'))
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_pool()$data_long, col = 'level_beginner',
        fills = get_fills('beginner')) +
        theme(axis.title.y = element_blank())
      fig_beginner = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_pool()$data_wide, col = 'level_improved',
        fills = get_fills('improved'), y_lims = c(0, 100)) +
        theme(axis.title.y = element_blank())
      fig_improved = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1, text = 'Numeracy: division level'),
        list(x = 0.405, y = 1, text = 'Innumeracy: beginner level'),
        list(x = 0.775, y = 1, text = 'Improved at least\none level'))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beginner, fig_improved, widths = c(0.37, 0.37, 0.26),
        margin = 0.04, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 55))
    })

    output$overview_delta_kpis = renderUI({
      req(data_proc)

      perc = scales::label_percent(scale = 1, accuracy = 1, suffix = ' %')

      metrics = data_proc()$data_long[, .(
        pct_ace = 100 * sum(level_ace) / .N,
        pct_beginner = -100 * sum(level_beginner) / .N),
        keyby = timepoint][
          , lapply(.SD, \(x) perc(diff(x))),
          .SDcols = c('pct_ace', 'pct_beginner')]
      pct_improved = data_proc()$data_wide[, 100 * sum(level_improved) / .N]
      metrics[, pct_improved := perc(pct_improved)]

      sty_n = 'font-size:35px;'
      sty_unit = 'font-size:20px;'
      iccls = 'fa-2x'

      fluidRow(
        column(
          width = 5, align = 'center', style = 'background-color:#a6cee3;',
          strong(paste0(metrics$pct_ace, '-pts'), style = sty_n), br(),
          p('Increased Numeracy', style = sty_unit)
        ),
        column(
          width = 4, align = 'center', style = 'background-color:#fb9a99;',
          strong(paste0(metrics$pct_beginner, '-pts'), style = sty_n), br(),
          p('Decreased Innumeracy', style = sty_unit)
        ),
        column(
          width = 3, align = 'center', style = 'background-color:#b2df8a;',
          strong(metrics$pct_improved, style = sty_n), br(),
          p('Improved a Level', style = sty_unit)
        )
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
