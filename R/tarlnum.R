# TODO

tarlnum_ui = function(id) {
  ns = NS(id)

  sidebarLayout(
    sidebarPanel(
      h5('Filtering options'),
      uiOutput(ns('ui_input')),
      uiOutput(ns('ui_counts')),
      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Key Performance Indicators',
          h4('Overall'),
          plotOutput(ns('plot_kpis_overall')),
          h4('Trends')#,
          # plotOutput(ns('plot_kpis_trends'))
        ),
        tabPanel(
          title = 'Detailed Results',
          # h4('Overall'),
          plotOutput(ns('plot_detailed_overall'))
        ),
        tabPanel(
          title = 'By School',
          p('TBD')
        ),
        tabPanel(
          title = 'Comparing Direct and Govt. Delivery',
          br(),
          p(em('Based on filtering options other than delivery type.')),
          # h4('Overall'),
          plotOutput(ns('plot_comp_overall'), height = '800px')
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

      delivery_types = sort(unique(data_proc()$data_long$delivery_type))
      delivery_types_options = get_picker_options(
        noneSelectedText = 'Delivery type(s)')

      regions = sort(unique(data_proc()$data_long$region))
      regions_options = get_picker_options(
        noneSelectedText = 'Region(s)')

      tagList(
        pickerInput(
          inputId = ns('delivery_types'),
          choices = delivery_types,
          selected = delivery_types,
          multiple = TRUE,
          options = delivery_types_options
        ),
        pickerInput(
          inputId = ns('regions'),
          choices = regions,
          selected = regions,
          multiple = TRUE,
          options = regions_options
        )
      )
    })

    data_long_filt = reactive({
      req(data_proc, input$delivery_types, input$regions)
      filt = CJ(
        delivery_type = input$delivery_types,
        region = input$regions)
      get_data_filtered(data_proc(), filt)$data_long[
        timepoint != 'Midline'][order(timepoint)]
    })

    output$ui_counts = renderUI({
      req(data_long_filt)
      # is actually unique student tarl-round combos, e.g.,
      # the same person participating in two years would get counted twice
      counts = data_long_filt()[, .(
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

    output$plot_kpis_overall = renderPlot({
      req(data_long_filt)

      data_long_now = data_long_filt()
      # levels of student_level go from Division to Beginner, so diff < 0
      data_wide = data_long_now[, .(
        level_improved = diff(as.integer(student_level)) < 0),
        by = student_id]
      set(data_wide, j = 'timepoint', value = 'Baseline to Endline')

      p_div = get_summary_barplot(
        data_long_now, col = 'level_division', fills = c('#a6cee3', '#1f78b4'),
        title = 'Numeracy: division level')

      p_beg = get_summary_barplot(
        data_long_now, col = 'level_beginner', fills = c('#fb9a99', '#e31a1c'),
        title = 'Innumeracy: beginner level') +
        theme(axis.title.y = element_blank())

      p_imp = get_summary_barplot(
        data_wide, col = 'level_improved', fills = '#33a02c',
        title = 'Learned a new\noperation', y_lims = c(0, 1)) +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      plot_grid(
        p_div, p_beg, p_imp, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 0.95, 0.7))
    }) |>
      bindCache(input$delivery_types, input$regions)

    output$plot_detailed_overall = renderPlot({
      req(data_long_filt)
      get_detailed_barplot(
        data_long_filt(), col = 'student_level', fills = get_levels_fills(),
        title = 'All levels')
    }) |>
      bindCache(input$delivery_types, input$regions)

    # TODO: tables for numeracy stats by school

    data_long_comp = reactive({
      req(data_proc, input$regions)
      filt = CJ(
        region = input$regions)
      d = get_data_filtered(data_proc(), filt)$data_long
      d = d[timepoint != 'Midline'][order(timepoint)]
      d[, treatment_id := delivery_type]
      d[, treatment_name := delivery_type][]
    })

    output$plot_comp_overall = renderPlot({
      req(data_long_comp)

      data_long_now = data_long_comp()
      # levels of student_level go from Division to Beginner, so diff < 0
      data_wide = data_long_now[, .(
        treatment_id, treatment_name,
        level_improved = diff(as.integer(student_level)) < 0),
        by = student_id]
      set(data_wide, j = 'timepoint', value = 'Baseline\nto Endline')

      p_div = get_summary_barplot(
        data_long_now, col = 'level_division', fills = c('#a6cee3', '#1f78b4'),
        title = 'Numeracy: division level', by_treatment = TRUE)

      p_beg = get_summary_barplot(
        data_long_now, col = 'level_beginner', fills = c('#fb9a99', '#e31a1c'),
        title = 'Innumeracy: beginner level', by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      p_imp = get_summary_barplot(
        data_wide, col = 'level_improved', fills = '#33a02c',
        title = 'Learned a new operation', by_treatment = TRUE)

      # use cowplot::plot_grid() to arrange plots
      p_div_beg = plot_grid(
        p_div, p_beg, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 0.95))

      p_imp_null = plot_grid(
        p_imp, grid::nullGrob(), nrow = 1L, rel_widths = c(1, 1))

      plot_grid(p_div_beg, p_imp_null, ncol = 1L)

      # TODO: plot student_level_diff?
    }) |>
      bindCache(input$regions)

  })
}
