# TODO

tarlnum_ui = function(id) {
  ns = NS(id)

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
          title = 'Key Outcomes',
          h4('Overall'),
          plotlyOutput(ns('plot_kpis_overall')),
          h4('Trends'),
          plotlyOutput(
            ns('plot_kpis_trends_ace'), width = '70%', height = '350px'),
          plotlyOutput(
            ns('plot_kpis_trends_beg'), width = '70%', height = '350px'),
          plotlyOutput(
            ns('plot_kpis_trends_imp'), width = '56%', height = '350px')
        ),
        tabPanel(
          title = 'Detailed Outcomes',
          br(),
          plotlyOutput(ns('plot_detailed'), width = '60%')
        ),
        tabPanel(
          title = 'Outcomes by School',
          br(),
          p('TBD')
        ),
        tabPanel(
          title = 'Comparing Direct and Govt. Delivery',
          br(),
          p(em('Based on filtering options other than delivery type.')),
          plotlyOutput(ns('plot_comp'), height = '800px')
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
      long = get_data_filtered(data_proc(), filt())$data_long[
        timepoint != 'Midline'][order(timepoint)]

      wide = long[, .(
        level_improved = diff(as.integer(student_level)) < 0),
        by = .(student_id, year_term_num, year_term)] |>
        set(j = 'timepoint', value = 'Baseline to Endline')

      list(long = long[], wide = wide[])
    })

    output$ui_counts = renderUI({
      req(data_filt)
      # is actually unique student tarl-round combos, e.g.,
      # the same person participating in two years would get counted twice
      counts = data_filt()$long[, .(
        n_students = uniqueN(student_id)), keyby = delivery_type]
      counts[, delivery_type := gsub(' Delivery$', '', delivery_type)]
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
      fig_beg = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_filt()$wide, col = 'level_improved', fills = get_fills('improved'),
        y_lims = c(0, 100)) +
        theme(axis.title.y = element_blank())
      fig_imp = ggplotly(fig, tooltip = 'text')

      annos = list(
        list(x = 0, y = 1, text = 'Numeracy: division level'),
        list(x = 0.4, y = 1, text = 'Innumeracy: beginner level'),
        list(x = 0.77, y = 1, text = 'Improved at least\none level'))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_ace, fig_beg, fig_imp, widths = c(0.37, 0.37, 0.26), margin = 0.035,
        titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 55))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    data_trend = reactive({
      req(data_filt)

      long = data_filt()$long[, .(
        n_total = .N, n_ace = sum(level_ace), n_beg = sum(level_beginner)),
        keyby = .(year_term_num, year_term, timepoint)]
      long[, pct_ace := 100 * n_ace / n_total]
      long[, pct_beg := 100 * n_beg / n_total]

      long[, label_ace := glue('{year_term}\n{timepoint}', .envir = .SD)]
      long[, label_beg := glue('{year_term}\n{timepoint}', .envir = .SD)]

      wide = data_filt()$wide[, .(
        n_total = .N, n_imp = sum(level_improved)),
        keyby = .(year_term_num, year_term, timepoint)]
      wide[, pct_imp := 100 * n_imp / n_total]

      wide[, label_imp := glue('{year_term}', .envir = .SD)]

      list(long = long[], wide = wide[])
    })

    output$plot_kpis_trends_ace = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$long, x_col = 'year_term_num', y_col = 'pct_ace',
        text_col = 'label_ace', fills = get_fills('ace'), shapes = c(24, 25),
        size = 3, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Numeracy: division level')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = list(t = 30))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_beg = renderPlotly({
      req(data_trend)
      fig = get_trend_plot(
        data_trend()$long, x_col = 'year_term_num', y_col = 'pct_beg',
        text_col = 'label_beg', fills = get_fills('beginner'),
        shapes = c(24, 25), size = 3, stroke = 0)
      anno = list(x = 0, y = 1, text = 'Innumeracy: beginner level')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = list(t = 30))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_kpis_trends_imp = renderPlotly({
      fig = ggplot(
        data_trend()$wide,
        aes(x = year_term_num, y = pct_imp, text = label_imp)) +
        geom_point(
          size = 2.5, shape = 21, stroke = 0, fill = get_fills('improved')) +
        labs(x = 'Year', y = 'Share of students (%)') +
        scale_x_continuous(minor_breaks = NULL) +
        scale_y_continuous(labels = label_percent_func, limits = c(0, 100))
      anno = list(x = 0, y = 1, text = 'Improved at least one level')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base), margin = list(t = 30))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    output$plot_detailed = renderPlotly({
      req(data_filt)
      fig = get_barplot_detailed(
        data_filt()$long, col = 'student_level', fills = get_fills('full'))
      anno = list(x = 0, y = 1, text = 'All levels')
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = c(anno, anno_base))
    }) |>
      bindCache(
        input$delivery_types, input$durations, input$regions, input$year_terms)

    # TODO: tables for numeracy stats by school

    filt_comp = reactive({
      unique(filt()[, !'delivery_type'])
    })

    data_comp = reactive({
      req(data_proc, filt_comp)
      long = get_data_filtered(data_proc(), filt_comp())$data_long
      long = long[timepoint != 'Midline'][order(timepoint)]
      long[, treatment_id := delivery_type]
      long[, treatment_name := delivery_type]

      # levels of student_level go from Division to Beginner, so diff < 0
      wide = long[, .(
        treatment_id, treatment_name,
        student_level_diff = -diff(as.integer(student_level))),
        by = student_id] |>
        set(j = 'timepoint', value = 'Baseline to Endline')
      wide[, level_improved := student_level_diff > 0]

      list(long = long[], wide = wide[])
    })

    output$plot_comp = renderPlotly({
      req(data_comp)

      data_wide_summary = data_comp()$wide[, .(
        diff_mean = mean(student_level_diff),
        diff_sd = sd(student_level_diff)),
        keyby = .(treatment_id, timepoint)]
      data_wide_summary[, label := glue(
        'Mean: {round(diff_mean, 1)}\nSD: {round(diff_sd, 1)}', .envir = .SD)]

      fig = get_barplot_summary(
        data_comp()$long, col = 'level_ace', fills = get_fills('ace'),
        by_treatment = TRUE)
      fig_ace = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_comp()$long, col = 'level_beginner', fills = get_fills('beginner'),
        by_treatment = TRUE) +
        theme(axis.title.y = element_blank())
      fig_beg = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_comp()$wide, col = 'level_improved', fills = get_fills('improved'),
        by_treatment = TRUE, y_lims = c(0, 100))
      fig_imp = ggplotly(fig, tooltip = 'text')

      fig = ggplot(data_wide_summary) +
        facet_wrap(vars(treatment_id), nrow = 1L) +
        geom_pointrange(
          aes(x = timepoint, y = diff_mean, ymin = diff_mean - diff_sd,
              ymax = diff_mean + diff_sd, text = label),
          size = 3, linewidth = 2) +
        labs(y = 'Number of levels improved') +
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
