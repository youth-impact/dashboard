# TODO

tarlnum_ui = function(id) {
  ns = NS(id)

  ht = 300
  height = glue('{ht}px')

  sidebarLayout(
    sidebarPanel(
      h5('Display options'),
      uiOutput(ns('ui_input')),
      uiOutput(ns('ui_counts')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Key Outcomes - Overall',
          br(),
          plotlyOutput(ns('plot_kpis_overall'), height = height)
        ),
        tabPanel(
          title = 'Key Outcomes - Trends',
          fluidRow(
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_trends_ace'), height = glue('{ht + 50}px'))
            ),
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_trends_beginner'), height = '350px')
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_trends_ace_diff'), height = height)
            ),
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_trends_beginner_diff'), height = height)
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_trends_improved'), height = height)
            ),
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_trends_total'), height = height)
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
          div(dataTableOutput(ns('table_by_school')), style = 'font-size:80%'),
        ),
        tabPanel(
          title = 'Outcomes by Direct and Govt. Delivery',
          br(),
          p(em('Based on filtering options other than delivery type.')),
          plotlyOutput(ns('plot_compare'), height = glue('{ht * 2}px'))
        )
      ),
      width = 9
    )
  )
}

tarlnum_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    data_proc = reactive({
      req(data_raw)
      get_data_tarlnum(data_raw())
    })

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns

      delivery_types = sort(unique(data_proc()$data_long$delivery_type))
      delivery_types_options = get_picker_options(
        noneSelectedText = 'Delivery types')

      durations = sort(unique(data_proc()$data_long$duration))
      durations_options = get_picker_options(
        noneSelectedText = 'Durations (days)')

      regions = sort(unique(data_proc()$data_long$region))
      regions_options = get_picker_options(
        noneSelectedText = 'Regions')

      year_terms = sort(unique(data_proc()$data_long$year_term))
      year_terms_options = get_picker_options(
        noneSelectedText = 'Years and terms')

      tagList(
        pickerInput(
          inputId = ns('delivery_types'),
          choices = delivery_types,
          selected = delivery_types,
          multiple = TRUE,
          options = delivery_types_options
        ),
        pickerInput(
          inputId = ns('durations'),
          choices = durations,
          selected = durations,
          multiple = TRUE,
          options = durations_options
        ),
        pickerInput(
          inputId = ns('regions'),
          choices = regions,
          selected = regions,
          multiple = TRUE,
          options = regions_options
        ),
        pickerInput(
          inputId = ns('year_terms'),
          choices = year_terms,
          selected = year_terms,
          multiple = TRUE,
          options = year_terms_options
        )
      )
    })

    filt = reactive({
      req(input$delivery_types, input$durations,
          input$regions, input$year_terms)
      CJ(
        delivery_type = input$delivery_types,
        duration = as.integer(input$durations),
        region = input$regions,
        year_term = input$year_terms)
    })

    data_filt = reactive({
      req(data_proc, filt)
      long = get_data_filtered(data_proc(), filt())$data_long
      wide = get_data_wide(long, c('duration', 'year_term_num', 'year_term'))
      list(long = long, wide = wide)
    })

    output$ui_counts = renderUI({
      req(data_filt)
      # is actually unique student tarl-round combos, e.g.,
      # the same person participating in two years would get counted twice
      counts = data_filt()$long[, .(
        n_students = uniqueN(student_id)), keyby = delivery_type]
      n_total = scales::label_comma()(sum(counts$n_students))
      counts[, n_students := scales::label_comma()(n_students)]
      txt = lapply(glue(
        '{counts$n_students} students ({counts$delivery_type})'),
        \(x) list(x, br()))
      txt = c(unlist(txt, recursive = FALSE),
              list(glue('{n_total} students in total')))
      em(txt)
    })

    output$plot_kpis_overall = renderPlotly({
      req(data_filt)

      fig = get_barplot_summary(
        data_filt()$long, col = 'level_ace', fills = get_fills('ace'))
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$long, col = 'level_beginner',
        fills = get_fills('beginner')) +
        theme(axis.title.y = element_blank())
      fig_beginner = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$wide, col = 'level_improved', fills = get_fills('improved'),
        y_lims = c(0, 100)) +
        theme(axis.title.y = element_blank())
      fig_improved = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1, text = 'Numeracy: division level'),
        list(x = 0.4, y = 1, text = 'Innumeracy: beginner level'),
        list(x = 0.77, y = 1, text = 'Improved at least\none level'))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beginner, fig_improved, widths = c(0.37, 0.37, 0.26),
        margin = 0.04, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 55))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    data_trend = reactive({
      req(data_filt)

      by_cols = c('year_term_num', 'year_term', 'duration')
      data_wide = get_data_wide(data_filt()$long, by_cols)
      metrics = get_metrics(data_filt()$long, data_wide, by_cols[1:2])

      metrics$long[, tt_ace := get_tooltips(n_ace, pct_ace, pre = year_term)]
      metrics$long[, tt_beginner := get_tooltips(
        n_beginner, pct_beginner, pre = year_term)]

      metrics$wide[, tt_total := get_tooltips(n_total, pre = year_term)]
      metrics$wide[, tt_improved := get_tooltips(
        n_improved, pct_improved, pre = year_term)]

      metrics$wide[, tt_ace_diff := get_tooltips(
        n_ace_diff, pct_ace_diff, pre = year_term)]
      metrics$wide[, tt_beginner_diff := get_tooltips(
        n_beginner_diff, pct_beginner_diff, pre = year_term)]

      metrics
    })

    marj = list(t = 30)
    lej = list(
      tracegroupgap = 0, x = 1, y = 1, xanchor = 'right', yanchor = 'bottom')

    output$plot_kpis_trends_ace = renderPlotly({
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
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_beginner = renderPlotly({
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
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_ace_diff = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'pct_ace_diff',
        text_col = 'tt_ace_diff', fill = get_fills('ace')[2L], shape = 21,
        size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Increase in numeracy')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_beginner_diff = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'pct_beginner_diff',
        text_col = 'tt_beginner_diff', fill = get_fills('beginner')[2L],
        shape = 21, sign = -1, size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Decrease in innumeracy')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_improved = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$wide, x_col = 'year_term_num', y_col = 'pct_improved',
        text_col = 'tt_improved', fill = get_fills('improved'), shape = 21,
        size = 2, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Improved at least one level')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = marj)
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_total = renderPlotly({
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
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_detailed_prepost = renderPlotly({
      req(data_filt)
      fig = get_barplot_detailed(
        data_filt()$long, col = 'student_level_fct', fills = get_fills('full'))
      anno = list(x = 0, y = 1, text = 'All levels')
      lej = list(tracegroupgap = 0)
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), legend = lej)
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_detailed_alluvial = renderPlotly({
      req(data_filt)
      flows = dcast(
        data_filt()$long, student_id ~ timepoint,
        value.var = 'student_level_fct')
      flows = flows[, .N, keyby = .(Baseline, Endline)]
      n_levels = uniqueN(flows$Baseline)

      plot_ly(
        type = 'sankey', orientation = 'h',
        node = list(
          label = rev(rep(levels(flows$Baseline), 2L)),
          color = rep(get_fills('full'), 2L),
          pad = 15, thickness = 20, line = list(color = 'black', width = 0.5)),
        link = list(
          source = rev(as.integer(flows$Baseline) - 1L),
          target = rev(as.integer(flows$Endline) + n_levels - 1L),
          value =  flows$N),
        valueformat = ',',
        valuesuffix = ' students') |>
        layout(title = list(
          text = 'Progress from Baseline to Endline', x = 0.15))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$table_by_school = renderDataTable({
      data_by_school = data_filt()$long[, .(
        n_total = .N, pct_ace = 100 * sum(level_ace, na.rm = TRUE) / .N),
        keyby = .(delivery_type, region, school_name, school_id, timepoint)]

      data_by_school = dcast(
        data_by_school, formula('... ~ timepoint'),
        value.var = c('n_total', 'pct_ace'))

      data_by_school[, pct_ace_diff := pct_ace_Endline - pct_ace_Baseline]
      data_by_school[, n_total_Endline := NULL]
      for (col in c('pct_ace_Baseline', 'pct_ace_Endline', 'pct_ace_diff')) {
        set(data_by_school, j = col,
            # value = format(data_by_school[[col]], digits = 2, nsmall = 1))
            value = round(data_by_school[[col]], 1))
      }
      setorder(data_by_school, -pct_ace_diff)

      cols_old = c(
        'delivery_type', 'region', 'school_name', 'school_id',
        'n_total_Baseline', 'pct_ace_Baseline',
        'pct_ace_Endline', 'pct_ace_diff')
      cols_new = c(
        'Delivery type', 'Region', 'School name', 'School ID',
        'Number of students', 'Baseline numeracy (%)',
        'Endline numeracy (%)', 'Change in numeracy (%)')
      setnames(data_by_school, cols_old, cols_new)

      opts = list(pageLength = 25L)
      DT::datatable(data_by_school, rownames = FALSE, options = opts) |>
        formatStyle(
          columns = 'Change in numeracy (%)',
          background = styleColorBar(c(0, 100), get_fills('ace')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatStyle(colnames(data_by_school), lineHeight = '80%')
    })

    filt_compare = reactive({
      req(filt)
      unique(filt()[, !'delivery_type'])
    })

    data_compare = reactive({
      req(data_proc, filt_compare)

      long = get_data_filtered(data_proc(), filt_compare())$data_long
      set(long, j = 'treatment_id', value = long$delivery_type)
      set(long, j = 'treatment_name', value = long$delivery_type)

      by_cols = c('treatment_id', 'treatment_name', 'duration')
      wide = get_data_wide(long, by_cols)
      list(long = long, wide = wide)
    })

    output$plot_compare = renderPlotly({
      req(data_compare)

      by_cols = c('treatment_id', 'treatment_name')
      metrics = get_metrics(data_compare()$long, data_compare()$wide, by_cols)

      metrics$wide[, tt_improvement := glue(
        'Mean: {tt_mean}\nSD: {tt_sd}',
        tt_mean = format(mean_improvement_per_week, digits = 2L, nsmall = 2L),
        tt_sd = format(sd_improvement_per_week, digits = 2L, nsmall = 2L))]

      fig = get_barplot_summary(
        data_compare()$long, col = 'level_ace', fills = get_fills('ace'),
        by_treatment = TRUE)
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_compare()$long, col = 'level_beginner',
        fills = get_fills('beginner'), by_treatment = TRUE) +
        theme(axis.title.y = element_blank())
      fig_beg = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_compare()$wide, col = 'level_improved',
        fills = get_fills('improved'), by_treatment = TRUE, y_lims = c(0, 100))
      fig_imp = ggplotly(fig, tooltip = 'text')

      fig = ggplot(metrics$wide) +
        facet_wrap(vars(treatment_id), nrow = 1L) +
        geom_pointrange(
          aes(x = timepoint, y = mean_improvement_per_week,
              ymin = mean_improvement_per_week - sd_improvement_per_week,
              ymax = mean_improvement_per_week + sd_improvement_per_week,
              text = tt_improvement),
          size = 3, linewidth = 2) +
        labs(y = 'Number of levels per week') +
        theme(axis.title.x = element_blank())
      fig_prog = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1.04, text = 'Numeracy: division level'),
        list(x = 0.55, y = 1.04, text = 'Innumeracy: beginner level'),
        list(x = 0, y = 0.47, text = 'Improved at least one level'),
        list(x = 0.55, y = 0.47, text = 'Progress toward numeracy'))
      annos = lapply(annos, \(z) c(z, anno_base))

      # yaxis_base = list(font = list(size = 20))
      #     yaxis = c(yaxis_base, list(title = 'Share of students (%)')),
      #     yaxis2 = c(yaxis_base, list(title = 'Number of levels improved')))

      sp1 = subplot(fig_ace, fig_beg, margin = 0.05, titleY = TRUE)
      sp2 = subplot(fig_imp, fig_prog, margin = 0.05, titleY = TRUE)
      subplot(sp1, sp2, nrows = 2L, margin = 0.07, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 50))
    }) |>
      bindCache(input$durations, input$regions, input$year_terms)
  })
}
