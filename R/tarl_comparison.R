# TODO

tarl_comparison_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Comparison Results',
    br(),
    uiOutput(ns('ui_input_a')),
    uiOutput(ns('ui_counts_a')),
    uiOutput(ns('ui_input_b')),
    uiOutput(ns('ui_counts_b')),
    uiOutput(ns('ui_overall')),
    plotOutput(ns('plot_overall'), height = '800px'),
    uiOutput(ns('ui_trends'))
  )
}

tarl_comparison_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input_a = renderUI({
      req(data_proc)
      ns = session$ns

      delivery_types = sort(unique(data_proc()$data_long$delivery_type))
      delivery_types_options = get_picker_options(
        noneSelectedText = 'Delivery type(s)')

      regions = sort(unique(data_proc()$data_long$region))
      regions_options = get_picker_options(noneSelectedText = 'Region(s)')

      fluidRow(
        h4('Group A'),
        column(
          3,
          pickerInput(
            inputId = ns('delivery_types_a'),
            choices = delivery_types,
            selected = delivery_types[1L],
            multiple = TRUE,
            options = delivery_types_options)
        ),
        column(
          3,
          pickerInput(
            inputId = ns('regions_a'),
            choices = regions,
            selected = regions,
            multiple = TRUE,
            options = regions_options)
        )
      )
    })

    output$ui_input_b = renderUI({
      req(data_proc)
      ns = session$ns

      delivery_types = sort(unique(data_proc()$data_long$delivery_type))
      delivery_types_options = get_picker_options(
        noneSelectedText = 'Delivery type(s)')

      regions = sort(unique(data_proc()$data_long$region))
      regions_options = get_picker_options(noneSelectedText = 'Region(s)')

      fluidRow(
        h4('Group B'),
        column(
          3,
          pickerInput(
            inputId = ns('delivery_types_b'),
            choices = delivery_types,
            selected = delivery_types[2L],
            multiple = TRUE,
            options = delivery_types_options)
        ),
        column(
          3,
          pickerInput(
            inputId = ns('regions_b'),
            choices = regions,
            selected = regions,
            multiple = TRUE,
            options = regions_options)
        )
      )
    })

    data_long_filt = reactive({
      req(data_proc, input$delivery_types_a, input$regions_a,
          input$delivery_types_b, input$regions_b)
      filt = rbind(
        CJ(treatment_name = 'Group A',
           treatment_id = 'Group A',
           delivery_type = input$delivery_types_a,
           region = input$regions_a),
        CJ(treatment_name = 'Group B',
           treatment_id = 'Group B',
           delivery_type = input$delivery_types_b,
           region = input$regions_b))
      merge(
        data_proc()$data_long, filt, by = colnames(filt)[-1 * (1:2)],
        allow.cartesian = TRUE)[
          timepoint != 'Midline'][order(timepoint)]
    })

    output$ui_counts_a = renderUI({
      req(data_long_filt)
      data_now = data_long_filt()[treatment_id == 'Group A']
      n_students = get_count_comma(data_now)
      p(em(glue('{n_students} students (ascertained at baseline and endline)')))
    })

    output$ui_counts_b = renderUI({
      req(data_long_filt)
      data_now = data_long_filt()[treatment_id == 'Group B']
      n_students = get_count_comma(data_now)
      p(em(glue('{n_students} students (ascertained at baseline and endline)')))
    })

    output$ui_overall = renderUI({
      req(data_long_filt)
      h4('Overall')
    })

    output$plot_overall = renderPlot({
      req(data_long_filt)

      str_wd = 40
      data_long_now = data_long_filt()
      # levels of student_level go from Division to Beginner, so diff < 0
      data_wide = data_long_now[, .(
        level_improved = diff(as.integer(student_level)) < 0),
        by = .(treatment_id, treatment_name, student_id)]
      set(data_wide, j = 'timepoint', value = 'Baseline to Endline')

      # https://waldyrious.net/viridis-palette-generator/
      # https://mdigi.tools/lighten-color/

      p_div = get_summary_barplot(
        data_long_now, col = 'level_division', #fills = c('#b2df8a', '#33a02c'),
        fills = c('#fef17c', '#fde725'), by_treatment = TRUE,
        title = str_wrap('Numeracy: division level', str_wd))

      p_beg = get_summary_barplot(
        data_long_now, col = 'level_beginner', #fills = c('#a6cee3', '#1f78b4'),
        fills = c('#66027e', '#440154'), by_treatment = TRUE,
        title = str_wrap('Innumeracy: beginner level', str_wd)) +
        theme(axis.title.y = element_blank())

      p_imp = get_summary_barplot(
        data_wide, col = 'level_improved', fills = '#56B4E9',
        by_treatment = TRUE,
        title = str_wrap('Learned a new operation', str_wd), bar_width = 0.6)

      p_stack = get_detailed_barplot(
        data_long_now, col = 'student_level', title = 'All levels',
        by_treatment = TRUE) +
        theme(axis.title.y = element_blank())

      # use cowplot::plot_grid() to arrange plots
      p_div_beg = plot_grid(p_div, p_beg, nrow = 1L, align = 'h', axis = 'tb')

      p_imp_stack = plot_grid(
        p_imp, p_stack, nrow = 1L, align = 'h', axis = 'tb',
        rel_widths = c(0.8, 1.2))

      plot_grid(p_div_beg, p_imp_stack, ncol = 1L)
    }) |>
      bindCache(
        input$delivery_types_a, input$regions_a,
        input$delivery_types_b, input$regions_b)

    output$ui_trends = renderUI({
      req(data_long_filt)
      h4('Trends')
    })
  })
}
