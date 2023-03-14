# connected_pooled module creates and displays ConnectEd pooled results

connected_pooled_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Overall Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')), # output$ui_input
        width = 2),

      mainPanel(
        br(),
        plotlyOutput(ns('plot_numeracy'), width = '80%'),
        br(),
        plotlyOutput(ns('plot_innumeracy'), width = '80%'),
        br(),
        plotlyOutput(ns('plot_improved'), width = '65%'),
        # plotOutput(ns('plot_all'), width = '80%'), # output$plot_all
        # tableOutput(ns('round_students')), # output$round_students
        width = 10)
    )
  )
}

connected_pooled_server = function(id, data_proc, keep_missing) {
  moduleServer(id, function(input, output, session) {

    # pooled results can include one or more rounds
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$rounds)

      checkboxGroupInput(
        inputId = ns('round_ids'),
        label = strong('Round'),
        choices = choices,
        selected = choices)
    })

    data_overall = reactive({
      req(data_proc, input$round_ids)
      get_data_connected_overall(data_proc(), input$round_ids)
    })

    # https://meyerweb.com/eric/tools/color-blend/#:::hex
    sz = 4
    wd = 0.7
    shp = c(24, 25)

    output$plot_numeracy = renderPlotly({
      req(data_overall)
      p = ggplot(
        data_overall()$long,
        aes(x = factor(round_name), group = arm_id, text = label)) +
        geom_linerange(
          aes(ymin = pct_div_Baseline, ymax = pct_div_Endline),
          position = position_dodge(width = wd), color = '#73C05B',
          linetype = 'dashed', data = data_overall()$wide) +
        geom_point(
          aes(y = pct_div, fill = Timepoint, shape = Timepoint),
          position = position_dodge(width = wd), size = sz, stroke = 0) +
        labs(x = 'Round', y = 'Percentage of students',
             title = 'Numeracy: can add, subtract, multiply, and divide') +
        scale_y_continuous(labels = scales::label_percent()) +
        scale_fill_manual(values = c('#b2df8a', '#33a02c')) +
        scale_shape_manual(values = shp)
      ggplotly(p)
    }) |>
      bindCache(input$round_ids, keep_missing())

    output$plot_innumeracy = renderPlotly({
      req(data_overall)
      p = ggplot(
        data_overall()$long,
        aes(x = factor(round_name), group = arm_id, text = label)) +
        geom_linerange(
          aes(ymin = pct_noadd_Baseline, ymax = pct_noadd_Endline),
          position = position_dodge(width = wd), color = '#63A3CC',
          linetype = 'dashed', data = data_overall()$wide) +
        geom_point(
          aes(y = pct_noadd, fill = Timepoint, shape = Timepoint),
          position = position_dodge(width = wd), size = sz, stroke = 0) +
        labs(x = 'Round', y = 'Percentage of students',
             title = 'Innumeracy: cannot add') +
        scale_y_continuous(labels = scales::label_percent()) +
        scale_fill_manual(values = c('#a6cee3', '#1f78b4')) +
        scale_shape_manual(values = shp)
      ggplotly(p)
    }) |>
      bindCache(input$round_ids, keep_missing())

    output$plot_improved = renderPlotly({
      req(data_overall)
      p = ggplot(
        data_overall()$wide,
        aes(x = factor(round_name), group = arm_id, text = label)) +
        geom_point(
          aes(y = pct_improved), position = position_dodge(width = wd),
          color = '#fdbf6f', size = sz) +
        labs(x = 'Round', y = 'Percentage of students',
             title = 'Improved: learned a new operation') +
        scale_y_continuous(labels = scales::label_percent())
      ggplotly(p)
    }) |>
      bindCache(input$round_ids, keep_missing())

    # combined plots for pooled results
    # output$plot_all = renderPlot({
    #   req(data_proc, input$round_ids)
    #
    #   data_wide = data_proc()$data_wide[round_id %in% input$round_ids]
    #   data_long = data_proc()$data_long[round_id %in% input$round_ids]
    #
    #   p_imp = get_summary_barplot(
    #     data_wide, col = 'improved', fill_vals = '#fdbf6f', bar_width = 0.6,
    #     title = str_wrap('Improved: learned a new operation', 18)) +
    #     theme(axis.title.y = element_blank())
    #
    #   p_div = get_summary_barplot(
    #     data_long, col = 'can_divide', fill_vals = c('#b2df8a', '#33a02c'),
    #     title = str_wrap(
    #       'Numeracy: can add, subtract, multiply, and divide', 30))
    #
    #   p_add = get_summary_barplot(
    #     data_long, col = 'cannot_add', fill_vals = c('#a6cee3', '#1f78b4'),
    #     title = str_wrap('Innumeracy: cannot add', 30)) +
    #     theme(axis.title.y = element_blank())
    #
    #   # use cowplot::plot_grid() to arrange plots
    #   plot_grid(
    #     p_div, p_add, p_imp, nrow = 1L, align = 'h', axis = 'tb',
    #     rel_widths = c(1, 0.9, 0.7))
    # }) |>
    #   bindCache(input$round_ids, keep_missing())

    # output$round_students = renderTable({
    #   req(data_proc, input$round_ids)
    #   get_counts_by_round(data_proc()$data_wide[round_id %in% input$round_ids])
    # }, align = 'r')
  })
}
