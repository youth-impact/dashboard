# TODO

tarl_detailed_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Single-selection Results',
    br(),
    uiOutput(ns('ui_input')),
    uiOutput(ns('ui_counts')),
    uiOutput(ns('ui_overall')),
    plotOutput(ns('plot_overall')),
    uiOutput(ns('ui_trends'))
  )
}

tarl_detailed_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns

      delivery_types = sort(unique(data_proc()$data_long$delivery_type))
      delivery_types_options = get_picker_options(
        noneSelectedText = 'Delivery type(s)')

      regions = sort(unique(data_proc()$data_long$region))
      regions_options = get_picker_options(noneSelectedText = 'Region(s)')

      fluidRow(
        column(
          3,
          pickerInput(
            inputId = ns('delivery_types'),
            choices = delivery_types,
            selected = delivery_types,
            multiple = TRUE,
            options = delivery_types_options)
        ),
        column(
          3,
          pickerInput(
            inputId = ns('regions'),
            choices = regions,
            selected = regions,
            multiple = TRUE,
            options = regions_options)
        )
      )
    })

    data_long_filt = reactive({
      req(data_proc, input$delivery_types, input$regions)
      filt = CJ(
        delivery_type = input$delivery_types,
        region = input$regions)
      merge(data_proc()$data_long, filt, by = colnames(filt))[
        timepoint != 'Midline'][order(timepoint)]
    })

    output$ui_counts = renderUI({
      req(data_long_filt)
      n_students = get_count_comma(data_long_filt())
      p(em(glue('{n_students} students (ascertained at baseline and endline)')))
    })

    output$ui_overall = renderUI({
      req(data_long_filt)
      h4('Overall')
    })

    output$plot_overall = renderPlot({
      req(data_long_filt)

      data_long_now = data_long_filt()
      # levels of student_level go from Division to Beginner, so diff < 0
      data_wide = data_long_now[, .(
        level_improved = diff(as.integer(student_level)) < 0),
        by = student_id]
      set(data_wide, j = 'timepoint', value = 'Baseline to Endline')

      # https://waldyrious.net/viridis-palette-generator/
      # https://mdigi.tools/lighten-color/

      p_div = get_summary_barplot(
        data_long_now, col = 'level_division', #fills = c('#b2df8a', '#33a02c'),
        fills = c('#fef17c', '#fde725'), title = 'Numeracy:\ndivision level')

      p_beg = get_summary_barplot(
        data_long_now, col = 'level_beginner', #fills = c('#a6cee3', '#1f78b4'),
        fills = c('#66027e', '#440154'), title = 'Innumeracy:\nbeginner level') +
        theme(axis.title.y = element_blank())

      p_imp = get_summary_barplot(
        data_wide, col = 'level_improved', fills = '#56B4E9',
        title = 'Learned a\nnew operation', bar_width = 0.6) +
        theme(axis.title.y = element_blank())

      p_stack = get_detailed_barplot(
        data_long_now, col = 'student_level', title = 'All levels') +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      plot_grid(
        p_div, p_beg, p_imp, p_stack, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(1, 0.9, 0.7, 1.3))
    }) |>
      bindCache(input$delivery_types, input$regions)

    output$ui_trends = renderUI({
      req(data_long_filt)
      h4('Trends')
    })
  })
}
