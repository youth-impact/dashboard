# TODO

tarllit_ui = function(id) {
  ns = NS(id)

  ht = 300
  height = glue('{ht}px')

  sidebarLayout(
    sidebarPanel(
      h5('Display Options'),
      uiOutput(ns('ui_input')),
      uiOutput(ns('ui_counts')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Overview',
          br(),
          uiOutput(ns('overview_banner')),
          plotlyOutput(ns('plot_kpis'), height = height)
        ),
        tabPanel(
          title = 'Trends',
          br(),
          uiOutput(ns('ui_trends')),
          plotlyOutput(
            ns('plot_trends'), height = glue('{ht * 2.75}px'), width = '40%')
        ),
        tabPanel(
          title = 'Detailed Outcomes',
          br(),
          plotlyOutput(
            ns('plot_detailed_prepost'), height = glue('{ht + 25}px'),
            width = '60%'),
          plotlyOutput(
            ns('plot_detailed_alluvial'), height = glue('{ht * 1.5}px'),
            width = '60%')
        ),
        tabPanel(
          title = 'Outcomes by School',
          br(),
          uiOutput(ns('ui_school')),
          div(dataTableOutput(ns('table_by_school')), style = 'font-size:80%'),
        )
      ),
      width = 9
    )
  )
}

tarllit_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    type = 'literacy'

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      long = data_proc()$tarllit_assessments_nomissing

      cols = c('duration_days', 'region', 'year_term_str')
      choices = lapply(cols, \(col) sort(unique(long[[col]])))
      names(choices) = cols
      choices$baseline_level = levels(long$student_level_str)

      tagList(
        pickerInput(
          inputId = ns('duration_days'),
          choices = choices$duration_days,
          selected = choices$duration_days,
          multiple = TRUE,
          options = get_picker_options('Durations (days)')
        ),
        pickerInput(
          inputId = ns('region'),
          choices = choices$region,
          selected = choices$region,
          multiple = TRUE,
          options = get_picker_options('Regions')
        ),
        pickerInput(
          inputId = ns('year_term_str'),
          choices = choices$year_term_str,
          selected = choices$year_term_str,
          multiple = TRUE,
          options = get_picker_options('Years and terms')
        ),
        pickerInput(
          inputId = ns('baseline_level'),
          choices = choices$baseline_level,
          selected = choices$baseline_level,
          multiple = TRUE,
          options = get_picker_options('Levels at baseline')
        )
      )
    })

    filt = reactive({
      req(input$duration_days, input$region, input$year_term_str)
      CJ(
        duration_days = as.integer(input$duration_days),
        region = input$region,
        year_term_str = input$year_term_str)
    })

    data_filt = reactive({
      req(data_proc, filt, input$baseline_level)
      tbls = c('tarllit_assessments_nomissing', 'tarllit_students_nomissing')
      filt_by_student = data.table(
        timepoint = 'Baseline', student_level_str = input$baseline_level)
      data_filt = get_data_filtered(data_proc()[tbls], filt(), filt_by_student)
      data_filt$tarllit_students_nomissing =
        data_filt$tarllit_students_nomissing[
          student_id %in% data_filt$tarllit_assessments_nomissing$student_id]
      data_filt
    })

    output$overview_banner = renderUI({
      req(data_filt)
      get_overview_banner(data_filt()$tarllit_students_nomissing, 'tarllit')
    })

    output$ui_counts = renderUI({
      req(data_filt)
      counts = data_filt()$tarllit_assessments_nomissing[, .(
        n_students = uniqueN(student_id))]

      n_total = scales::label_comma()(sum(counts$n_students))
      n_total_txt = glue('{n_total} students in total')

      counts[, n_students := scales::label_comma()(n_students)]
      em(tags$sup('‡'),
         'All results based on students assessed at baseline and endline.',
         br(), br(), list(n_total_txt))
    })

    output$plot_kpis = renderPlotly({
      req(data_filt)

      fig = get_barplot_summary(
        data_filt()$tarllit_assessments_nomissing, col = 'level_ace',
        fills = get_fills('ace'))
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$tarllit_assessments_nomissing, col = 'level_beginner',
        fills = get_fills('beginner')) +
        theme(axis.title.y = element_blank())
      fig_beginner = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$tarllit_students_nomissing, col = 'level_improved',
        fills = get_fills('improved'), y_lims = c(0, 100)) +
        theme(axis.title.y = element_blank())
      fig_improved = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1, text = get_title('ace', type)),
        list(x = 0.405, y = 1, text = get_title('beginner', type)),
        list(x = 0.775, y = 1, text = 'Improved a level'))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beginner, fig_improved, widths = c(0.37, 0.37, 0.26),
        margin = 0.04, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 40))
    }) |>
      bindCache(
        input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$ui_trends = renderUI({
      ns = session$ns
      checkboxInput(
        inputId = ns('by_year'),
        label = 'Aggregate by year',
        value = FALSE
      )
    })

    output$plot_trends = renderPlotly({
      req(data_filt, !is.null(input$by_year))
      get_plot_trends_tarl(
        data_filt()$tarllit_students_nomissing, isTRUE(input$by_year), type)
    }) |>
      bindCache(
        input$duration_days, input$region,
        input$year_term_str, input$baseline_level, input$by_year)

    output$plot_detailed_prepost = renderPlotly({
      req(data_filt)
      fig = get_barplot_detailed(
        data_filt()$tarllit_assessments_nomissing, col = 'student_level_str',
        fills = get_fills('full'))
      anno = list(x = 0, y = 1, text = 'All levels')
      lej = list(tracegroupgap = 0)
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), legend = lej)
    }) |>
      bindCache(
        input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_detailed_alluvial = renderPlotly({
      req(data_filt)
      flows = dcast(
        data_filt()$tarllit_assessments_nomissing, student_id ~ timepoint,
        value.var = 'student_level_str')

      # keep as factor
      levs = sort(unique(
        data_filt()$tarllit_assessments_nomissing$student_level_str))
      flows = merge(
        CJ(Baseline = levs, Endline = levs),
        flows[, .N, keyby = .(Baseline, Endline)],
        by = c('Baseline', 'Endline'), all.x = TRUE)
      flows[is.na(N), N := 0L]

      plot_ly(
        type = 'sankey', orientation = 'h',
        node = list(
          label = rev(rep(levels(flows$Baseline), 2L)),
          color = rep(get_fills('full'), 2L),
          pad = 15, thickness = 20, line = list(color = 'black', width = 0.5),
          hoverinfo = 'none'),
        link = list(
          source = rev(as.integer(flows$Baseline) - 1L),
          target = rev(as.integer(flows$Endline) + length(levs) - 1L),
          value =  flows$N),
        valueformat = ',',
        valuesuffix = ' students') |>
        layout(title = list(
          text = 'Progress from Baseline to Endline', x = 0.15))
    }) |>
      bindCache(
        input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$ui_school = renderUI({
      ns = session$ns
      fluidRow(
        column(
          width = 6,
          checkboxInput(
            inputId = ns('school_kpis_by_timepoint'),
            label = 'Show key outcomes for baseline and endline',
            value = FALSE,
            width = '100%')),
        column(
          width = 6, align = 'right',
          downloadButton(ns('download_by_school'), style = 'font-size:80%')
        ),
        br(), br()
      )
    })

    metrics_by_school = reactive({
      req(data_filt, !is.null(input$school_kpis_by_timepoint))
      by_cols = c('region', 'school_name', 'school_id')

      metrics = data_filt()$tarllit_assessments_nomissing[, .(
        pct_ace = 100 * sum(level_ace, na.rm = TRUE) / .N,
        pct_beginner = 100 * sum(level_beginner, na.rm = TRUE) / .N),
        keyby = c(by_cols, 'timepoint')]

      metrics = dcast(
        metrics, formula('... ~ timepoint'),
        value.var = c('pct_ace', 'pct_beginner'))

      metrics_wide = data_filt()$tarllit_students_nomissing[, .(
        n_students = .N,
        n_terms = uniqueN(year_term_str),
        pct_improved = 100 * sum(level_improved, na.rm = TRUE) / .N),
        keyby = by_cols]
      metrics = merge(metrics, metrics_wide, by = by_cols)

      metrics[, pct_ace_diff := pct_ace_Endline - pct_ace_Baseline]
      metrics[
        , pct_beginner_diff := pct_beginner_Baseline - pct_beginner_Endline]
      setorder(metrics, -pct_ace_diff, pct_beginner_diff, -pct_improved)

      cols_old = c(
        'school_name', 'school_id', 'region',
        'pct_ace_diff', 'pct_ace_Baseline', 'pct_ace_Endline',
        'pct_beginner_diff', 'pct_beginner_Baseline', 'pct_beginner_Endline',
        'pct_improved', 'n_students', 'n_terms')
      cols_new = c(
        'School name', 'School ID', 'Region',
        'Increase in literacy (%-points)', 'Baseline literacy (%)',
        'Endline literacy (%)',
        'Decrease in illiteracy (%-points)', 'Baseline illiteracy (%)',
        'Endline illiteracy (%)',
        'Improved a level (%)', 'Number of students', 'Number of terms')
      setnames(metrics, cols_old, cols_new)
      setcolorder(metrics, cols_new)
    })

    output$table_by_school = renderDataTable({
      req(metrics_by_school, !is.null(input$school_kpis_by_timepoint))
      metrics = copy(metrics_by_school())
      cols_num = colnames(metrics)[4:10]

      if (isFALSE(input$school_kpis_by_timepoint)) {
        cols_drop = c(
          'Baseline literacy (%)', 'Endline literacy (%)',
          'Baseline illiteracy (%)', 'Endline illiteracy (%)')
        metrics[, (cols_drop) := NULL]
        cols_num = setdiff(cols_num, cols_drop)
      }

      opts = list(pageLength = 500L, lengthMenu = c(50, 150, 500))
      DT::datatable(metrics, rownames = FALSE, options = opts) |>
        formatStyle(
          columns = 'Increase in literacy (%-points)',
          background = styleColorBar(c(0, 100), get_fills('ace')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatStyle(
          columns = 'Decrease in illiteracy (%-points)',
          background = styleColorBar(c(0, 100), get_fills('beginner')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatStyle(
          columns = 'Improved a level (%)',
          background = styleColorBar(c(0, 100), get_fills('improved')),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatRound(columns = cols_num, digits = 1L) |>
        formatStyle(colnames(metrics), lineHeight = '80%')
    })

    output$download_by_school = downloadHandler(
      filename = \() 'tarl_literacy_outcomes_by_school.xlsx',
      content = function(file) {
        writexl::write_xlsx(metrics_by_school(), file)}
    )
  })
}
