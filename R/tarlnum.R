# TODO

tarlnum_ui = function(id) {
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
          title = 'Key Outcomes',
          br(),
          plotlyOutput(ns('plot_kpis'), height = height)
        ),
        tabPanel(
          title = 'Trends',
          fluidRow(
            column(
              width = 6,
              plotlyOutput(ns('plot_trend_ace'), height = glue('{ht + 50}px'))
            ),
            column(
              width = 6,
              plotlyOutput(ns('plot_trend_beginner'), height = '350px')
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(ns('plot_trend_ace_diff'), height = height)
            ),
            column(
              width = 6,
              plotlyOutput(ns('plot_trend_beginner_diff'), height = height)
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(ns('plot_trend_improved'), height = height)
            ),
            column(
              width = 6,
              plotlyOutput(ns('plot_trend_total'), height = height)
            )
          )
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
        ),
        tabPanel(
          title = 'Outcomes by Delivery Model',
          br(),
          p(em('Based on display options other than delivery model.')),
          plotlyOutput(ns('plot_compare'), height = glue('{ht * 2}px'))
        )
      ),
      width = 9
    )
  )
}

tarlnum_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      long = data_proc()$tarlnum_assessments_nomissing

      cols = c('delivery_model', 'duration_days', 'region', 'year_term_str')
      choices = lapply(cols, \(col) sort(unique(long[[col]])))
      names(choices) = cols
      choices$baseline_level = levels(long$student_level_str)

      tagList(
        pickerInput(
          inputId = ns('delivery_model'),
          choices = choices$delivery_model,
          selected = choices$delivery_model,
          multiple = TRUE,
          options = get_picker_options('Delivery models')
        ),
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
      req(input$delivery_model, input$duration_days,
          input$region, input$year_term_str)
      CJ(
        delivery_model = input$delivery_model,
        duration_days = as.integer(input$duration_days),
        region = input$region,
        year_term_str = input$year_term_str)
    })

    data_filt = reactive({
      req(data_proc, filt, input$baseline_level)
      tbls = c('tarlnum_assessments_nomissing', 'tarlnum_students_nomissing')
      filt_by_student = data.table(
        timepoint = 'Baseline', student_level_str = input$baseline_level)
      data_filt = get_data_filtered(data_proc()[tbls], filt(), filt_by_student)
      data_filt$tarlnum_students_nomissing =
        data_filt$tarlnum_students_nomissing[
          student_id %in% data_filt$tarlnum_assessments_nomissing$student_id]
      data_filt
    })

    output$ui_counts = renderUI({
      req(data_filt)
      counts = data_filt()$tarlnum_assessments_nomissing[, .(
        n_students = uniqueN(student_id)), keyby = delivery_model]

      n_total = scales::label_comma()(sum(counts$n_students))
      n_total_txt = glue('{n_total} students in total')

      counts[, n_students := scales::label_comma()(n_students)]
      txt = lapply(glue(
        '{counts$n_students} students ({counts$delivery_model})'),
        \(x) list(x, br()))
      em(c(unlist(txt, recursive = FALSE), list(n_total_txt)))
    })

    output$plot_kpis = renderPlotly({
      req(data_filt)

      fig = get_barplot_summary(
        data_filt()$tarlnum_assessments_nomissing, col = 'level_ace',
        fills = get_fills('ace'))
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$tarlnum_assessments_nomissing, col = 'level_beginner',
        fills = get_fills('beginner')) +
        theme(axis.title.y = element_blank())
      fig_beginner = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$tarlnum_students_nomissing, col = 'level_improved',
        fills = get_fills('improved'), y_lims = c(0, 100)) +
        theme(axis.title.y = element_blank())
      fig_improved = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1, text = 'Numeracy: division level'),
        list(x = 0.405, y = 1, text = 'Innumeracy: beginner level'),
        list(x = 0.775, y = 1, text = 'Improved a level\n(or more)'))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beginner, fig_improved, widths = c(0.37, 0.37, 0.26),
        margin = 0.04, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 55))
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    data_trend = reactive({
      req(data_filt)

      metrics = get_metrics(
        data_filt()$tarlnum_assessments_nomissing,
        data_filt()$tarlnum_students_nomissing,
        c('year_term_num', 'year_term_str'))

      metrics$long[, tt_ace := get_tooltips(
        n_ace, pct_ace, pre = year_term_str)]
      metrics$long[, tt_beginner := get_tooltips(
        n_beginner, pct_beginner, pre = year_term_str)]

      metrics$wide[, tt_total := get_tooltips(n_total, pre = year_term_str)]
      metrics$wide[, tt_improved := get_tooltips(
        n_improved, pct_improved, pre = year_term_str)]

      metrics$wide[, tt_ace_diff := get_tooltips(
        n_ace_diff, pct_ace_diff, pre = year_term_str)]
      metrics$wide[, tt_beginner_diff := get_tooltips(
        n_beginner_diff, pct_beginner_diff, pre = year_term_str)]

      metrics
    })

    marj = list(t = 30)
    lej = list(
      tracegroupgap = 0, x = 1, y = 1, xanchor = 'right', yanchor = 'bottom')

    output$plot_trend_ace = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$long, x_col = 'year_term_num', y_col = 'pct_ace',
        text_col = 'tt_ace', fill = get_fills('ace'), shape = c(24, 25),
        size = 2.5, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Numeracy: division level')
      ggplotly(fig, tooltip = 'text') |>
        layout(
          annotations = c(anno, anno_base), margin = marj, legend = lej)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_trend_beginner = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$long, x_col = 'year_term_num', y_col = 'pct_beginner',
        text_col = 'tt_beginner', fill = get_fills('beginner'),
        shape = c(24, 25), size = 2.5, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Innumeracy: beginner level')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj, legend = lej)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_trend_ace_diff = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'pct_ace_diff',
        text_col = 'tt_ace_diff', fill = get_fills('ace')[1L], shape = 21,
        size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Increased numeracy')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_trend_beginner_diff = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'pct_beginner_diff',
        text_col = 'tt_beginner_diff', fill = get_fills('beginner')[1L],
        shape = 21, sign = -1, size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Decreased innumeracy')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_trend_improved = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'pct_improved',
        text_col = 'tt_improved', fill = get_fills('improved'), shape = 21,
        size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Improved a level (or more)')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_trend_total = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'n_total',
        text_col = 'tt_total', fill = get_fills('total'), shape = 21,
        percent = FALSE, size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Completed the program')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_detailed_prepost = renderPlotly({
      req(data_filt)
      fig = get_barplot_detailed(
        data_filt()$tarlnum_assessments_nomissing, col = 'student_level_str',
        fills = get_fills('full'))
      anno = list(x = 0, y = 1, text = 'All levels')
      lej = list(tracegroupgap = 0)
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), legend = lej)
    }) |>
      bindCache(
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$plot_detailed_alluvial = renderPlotly({
      req(data_filt)
      flows = dcast(
        data_filt()$tarlnum_assessments_nomissing, student_id ~ timepoint,
        value.var = 'student_level_str')

      # keep as factor
      levs = sort(unique(
        data_filt()$tarlnum_assessments_nomissing$student_level_str))
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
        input$delivery_model, input$duration_days, input$region,
        input$year_term_str, input$baseline_level)

    output$ui_school = renderUI({
      ns = session$ns
      checkboxInput(
        inputId = ns('school_kpis_by_timepoint'),
        label = 'Show key outcomes for baseline and endline',
        value = FALSE,
        width = '100%')
    })

    output$table_by_school = renderDataTable({
      req(data_filt, !is.null(input$school_kpis_by_timepoint))
      by_cols = c('delivery_model', 'region', 'school_name', 'school_id')

      metrics = data_filt()$tarlnum_assessments_nomissing[, .(
        pct_ace = 100 * sum(level_ace, na.rm = TRUE) / .N,
        pct_beginner = 100 * sum(level_beginner, na.rm = TRUE) / .N),
        keyby = c(by_cols, 'timepoint')]

      metrics = dcast(
        metrics, formula('... ~ timepoint'),
        value.var = c('pct_ace', 'pct_beginner'))

      metrics_wide = data_filt()$tarlnum_students_nomissing[, .(
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
        'delivery_model', 'region', 'school_name', 'school_id',
        'pct_ace_diff', 'pct_ace_Baseline', 'pct_ace_Endline',
        'pct_beginner_diff', 'pct_beginner_Baseline', 'pct_beginner_Endline',
        'pct_improved', 'n_students', 'n_terms')
      cols_new = c(
        'Delivery model', 'Region', 'School name', 'School ID',
        'Increased numeracy (%-pts)', 'Baseline numeracy (%)',
        'Endline numeracy (%)',
        'Decreased innumeracy (%-pts)', 'Baseline innumeracy (%)',
        'Endline innumeracy (%)',
        'Improved a level (%)', 'Number of students', 'Number of terms')
      setnames(metrics, cols_old, cols_new)
      setcolorder(metrics, cols_new)
      cols_num = cols_new[5:11]

      if (isFALSE(input$school_kpis_by_timepoint)) {
        cols_drop = c(
          'Baseline numeracy (%)', 'Endline numeracy (%)',
          'Baseline innumeracy (%)', 'Endline innumeracy (%)')
        metrics[, (cols_drop) := NULL]
        cols_num = setdiff(cols_num, cols_drop)
      }

      opts = list(pageLength = 25L)
      DT::datatable(metrics, rownames = FALSE, options = opts) |>
        formatStyle(
          columns = 'Increased numeracy (%-pts)',
          background = styleColorBar(c(0, 100), get_fills('ace')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatStyle(
          columns = 'Decreased innumeracy (%-pts)',
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

    filt_compare = reactive({
      req(filt)
      unique(filt()[, !'delivery_model'])
    })

    data_compare = reactive({
      req(data_proc, filt_compare, input$baseline_level)

      filt_by_student = data.table(
        timepoint = 'Baseline', student_level_str = input$baseline_level)

      tbls = c('tarlnum_assessments_nomissing', 'tarlnum_students_nomissing')
      data_compare = get_data_filtered(
        data_proc()[tbls], filt_compare(), filt_by_student)

      data_compare$tarlnum_students_nomissing =
        data_compare$tarlnum_students_nomissing[
        student_id %in% data_compare$tarlnum_assessments_nomissing$student_id]

      data_compare$tarlnum_assessments_nomissing[
        , treatment_id := delivery_model]
      data_compare$tarlnum_assessments_nomissing[
        , treatment_wrap := delivery_model]
      data_compare$tarlnum_students_nomissing[
        , treatment_id := delivery_model]
      data_compare$tarlnum_students_nomissing[
        , treatment_wrap := delivery_model]
      data_compare
    })

    output$plot_compare = renderPlotly({
      req(data_compare)

      yaxis = list(title = 'Share of students (%)', titlefont = list(size = 20))
      metrics = get_metrics(
        data_compare()$tarlnum_assessments_nomissing,
        data_compare()$tarlnum_students_nomissing,
        c('treatment_id', 'treatment_wrap'))

      metrics$wide[, tt_progress := glue(
        'Mean: {tt_mean}\nSD: {tt_sd}',
        tt_mean = format(mean_progress_per_week, digits = 2L, nsmall = 2L),
        tt_sd = format(sd_progress_per_week, digits = 2L, nsmall = 2L))]

      fig = get_barplot_summary(
        data_compare()$tarlnum_assessments_nomissing, col = 'level_ace',
        fills = get_fills('ace'), by_treatment = TRUE)
      fig_ace = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = yaxis)

      fig = get_barplot_summary(
        data_compare()$tarlnum_assessments_nomissing, col = 'level_beginner',
        fills = get_fills('beginner'), by_treatment = TRUE) +
        theme(axis.title.y = element_blank())
      fig_beginner = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = yaxis)

      fig = get_barplot_summary(
        data_compare()$tarlnum_students_nomissing, col = 'level_improved',
        fills = get_fills('improved'), by_treatment = TRUE, y_lims = c(0, 100))
      fig_improved = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = yaxis)

      fig = ggplot(metrics$wide) +
        facet_wrap(vars(treatment_id), nrow = 1L) +
        geom_pointrange(
          aes(x = timepoint, y = mean_progress_per_week,
              ymin = mean_progress_per_week - sd_progress_per_week,
              ymax = mean_progress_per_week + sd_progress_per_week,
              text = tt_progress),
          size = 2, linewidth = 3) +
        labs(y = 'Number of levels per week') +
        theme(axis.title.x = element_blank())
      fig_progress = ggplotly(fig, tooltip = 'text') |>
        layout(yaxis = list(
          title = 'Number of levels per week', titlefont = list(size = 20)))

      annos = list(
        list(x = 0, y = 1.05, text = 'Numeracy: division level'),
        list(x = 0.56, y = 1.05, text = 'Innumeracy: beginner level'),
        list(x = 0, y = 0.46, text = 'Improved a level (or more)'),
        list(x = 0.56, y = 0.46, text = 'Progress toward numeracy'))
      annos = lapply(annos, \(z) c(z, anno_base))

      sp1 = subplot(fig_ace, fig_beginner, margin = 0.06, titleY = TRUE)
      sp2 = subplot(fig_improved, fig_progress, margin = 0.06, titleY = TRUE)
      subplot(sp1, sp2, nrows = 2L, margin = 0.09, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 50))
    }) |>
      bindCache(
        input$duration_days, input$region, input$year_term_str,
        input$baseline_level)
  })
}
