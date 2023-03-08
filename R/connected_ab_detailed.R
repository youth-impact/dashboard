# connected_ab_detailed module creates and displays ConnectEd A/B detailed results

connected_ab_detailed_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'A/B Detailed Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')), # output$ui_input
        width = 3),

      mainPanel(
        uiOutput(ns('round_text')), # output$round_text
        tableOutput(ns('treatment_students')), # output$treatment_students
        br(),
        plotOutput(ns('plot_all'), width = '90%'), # output$plot_all
        width = 9)
    )
  )
}

connected_ab_detailed_server = function(id, data_proc, keep_missing) {
  moduleServer(id, function(input, output, session) {

    # A/B detailed results for a single round
    # option to show stacked barplot as percentages or counts
    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$data)

      tagList(
        radioButtons(
          inputId = ns('round_ids'),
          label = strong('Round'),
          choices = choices,
          selected = tail(choices, n = 1L)),
        radioButtons(
          inputId = ns('y_display'),
          label = strong('Display as'),
          choices = c('percentage', 'count'))
      )
    })

    # narrative text for the selected round
    output$round_text = renderUI({
      req(data_proc, input$round_ids)
      get_round_text(
        data_proc()$rounds, data_proc()$arms, data_proc()$treatments,
        input$round_ids)
    }) |>
      bindCache(input$round_ids)

    # plot for A/B detailed results
    output$plot_all = renderPlot({
      req(data_proc, input$round_ids, input$y_display)

      data_long = data_proc()$data_long[round_id %in% input$round_ids]
      data_long[, treatment_name := str_wrap(treatment_name, 20)]

      get_detailed_barplot(
        data_long, col = 'level_name', by_treatment = TRUE,
        percent = startsWith(input$y_display, 'percent'))
    }) |>
      bindCache(input$round_ids, input$y_display, keep_missing())

    output$treatment_students = renderTable({
      req(data_proc, input$round_ids)
      get_counts_by_treatment(data_proc()$data[round_id %in% input$round_ids])
    }, align = 'lr')
  })
}
