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
          title = 'Overview',
          h4('All Rounds'),
          uiOutput(ns('overview_banner')),
          plotlyOutput(ns('overview_plot_kpis'), height = glue('{ht}px'))
        ),
        tabPanel(
          title = 'Trends',
          h4('All Rounds'),
          plotlyOutput(
            ns('plot_trends'), height = glue('{ht * 2.5}px'), width = '50%')
        ),
        tabPanel(
          title = 'Key Outcomes by Round',
          uiOutput(ns('round_header_kpis')),
          uiOutput(ns('round_banner_kpis')),
          uiOutput(ns('round_text_kpis')),
          plotlyOutput(
            ns('plot_kpis'), height = glue('{ht * 3}px'), width = '80%'),
        ),
        tabPanel(
          title = 'Detailed Outcomes by Round',
          uiOutput(ns('round_header_detailed')),
          uiOutput(ns('round_banner_detailed')),
          uiOutput(ns('round_text_detailed')),
          plotlyOutput(
            ns('plot_detailed'), height = glue('{ht + 50}px'), width = '95%')
        )
      ),
      width = 9
    )
  )
}

connected_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = data_proc()$connected_rounds$round_id
      names(choices) = data_proc()$connected_rounds$round_label

      tagList(
        pickerInput(
          inputId = ns('round_id'),
          choices = choices,
          selected = tail(choices, n = 1L)
        ),
        checkboxInput(
          inputId = ns('by_treatment'),
          label = 'Split selected round\'s results by treatment',
          value = TRUE),
        em(tags$sup('‡'),
           'All results based on students assessed at baseline and endline.')
      )
    })

    data_pool = reactive({
      req(data_proc)
      tbls = c(
        'connected_assessments_nomissing', 'connected_students_nomissing')
      data_pool = get_data_filtered(data_proc()[tbls])
      treatment_now = 'All Rounds,\nAll Treatments'
      for (tbl in tbls) {
        set(data_pool[[tbl]], j = 'treatment_id', value = 'zzz')
        set(data_pool[[tbl]], j = 'treatment_wrap', value = treatment_now)
      }
      data_pool
    })

    output$overview_banner = renderUI({
      req(data_proc)
      get_overview_banner(data_proc()$connected_students_nomissing, 'connected')
    })

    output$overview_plot_kpis = renderPlotly({
      req(data_pool)

      fig = get_barplot_summary(
        data_pool()$connected_assessments, col = 'level_ace',
        fills = get_fills('ace'))
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_pool()$connected_assessments, col = 'level_beginner',
        fills = get_fills('beginner')) +
        theme(axis.title.y = element_blank())
      fig_beginner = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_pool()$connected_students, col = 'level_improved',
        fills = get_fills('improved'), y_lims = c(0, 100)) +
        theme(axis.title.y = element_blank())
      fig_improved = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1, text = get_title('ace')),
        list(x = 0.405, y = 1, text = get_title('beginner')),
        list(x = 0.775, y = 1, text = get_title('improved')))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beginner, fig_improved, widths = c(0.37, 0.37, 0.26),
        margin = 0.04, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 40))
    })

    data_filt = reactive({
      req(data_proc, input$round_id)
      filt = CJ(round_id = input$round_id)
      get_data_filtered(
        data_proc()[startsWith(names(data_proc()), 'connected')], filt)
    })

    output$plot_trends = renderPlotly({
      req(data_proc)
      get_plot_trends_connected(
        data_proc()$connected_students_nomissing,
        data_proc()$connected_rounds)
    })

    output$round_header_kpis = output$round_header_detailed = renderUI({
      h4(data_filt()$connected_rounds$round_name)
    })

    output$round_banner_kpis = output$round_banner_detailed = renderUI({
      req(data_proc)
      get_overview_banner(data_filt()$connected_students_nomissing, 'connected')
    })

    # narrative text for the selected round
    output$round_text_kpis = output$round_text_detailed = renderUI({
      req(data_filt)
      get_round_text(data_filt())
    })

    output$plot_kpis = renderPlotly({
      req(data_filt)
      long = copy(data_filt()$connected_assessments_nomissing)
      wide = copy(data_filt()$connected_students_nomissing)

      if (isFALSE(input$by_treatment)) {
        round_name = data_filt()$connected_rounds$round_name
        treatment_now = glue('{round_name},\nAny Treatment')
        long[, treatment_id := '0']
        long[, treatment_wrap := treatment_now]
        wide[, treatment_id := '0']
        wide[, treatment_wrap := treatment_now]
      }

      long = rbind(long, data_pool()$connected_assessments_nomissing)
      wide = rbind(wide, data_pool()$connected_students_nomissing)
      get_plot_kpis(long, wide)
    }) |>
      bindCache(input$round_id, input$by_treatment)

    # plot for A/B detailed results
    output$plot_detailed = renderPlotly({
      req(data_filt)

      long = copy(data_filt()$connected_assessments_nomissing)
      if (isFALSE(input$by_treatment)) {
        round_name = data_filt()$connected_rounds$round_name
        treatment_now = glue('{round_name},\nAny Treatment')
        long[, treatment_id := '0']
        long[, treatment_wrap := treatment_now]
      }
      long = rbind(long, data_pool()$connected_assessments_nomissing)

      fig = get_barplot_detailed(
        long, col = 'student_level_str', fills = get_fills('full'),
        by_treatment = TRUE)
      fig = ggplotly(fig, tooltip = 'text')

      fig = facet_strip_bigger(fig)
      y = 1.2
      marj = list(t = 65)

      anno = c(list(x = 0, y = y, text = get_title('full')), anno_base)
      lej = list(tracegroupgap = 0)
      layout(fig, annotations = anno, margin = marj, legend = lej)
    }) |>
      bindCache(input$round_id, input$by_treatment)
  })
}
