connected_ab_detailed_ui = function(id) {
  ns = NS(id)

  tabPanel(
    'A/B Detailed Results',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')),
        width = 2),

      mainPanel(
        h6(textOutput(ns('round_text'))),
        br(),
        plotOutput(ns('plot_all'), width = '70%'),
        width = 10)
    )
  )
}

connected_ab_detailed_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      choices = get_choices(data_proc()$data)
      tagList(
        radioButtons(
          inputId = ns('round_ids'),
          label = 'Round',
          choices = choices,
          selected = tail(choices, n = 1L)),
        radioButtons(
          inputId = ns('y_display'),
          label = 'Display as',
          choices = c('percentage', 'count'))
      )
    })

    output$round_text = renderText({
      req(input$round_ids, data_proc)
      rounds_now = data_proc()$rounds[round_id == input$round_ids]
      glue('Round {rounds_now$round_name}: {rounds_now$round_purpose}')
    }) |>
      bindCache(input$round_ids)

    output$plot_all = renderPlot({
      req(input$round_ids, input$y_display, data_proc)

      data_long = copy(data_proc()$data_long)

      get_detailed_barplot(
        data_long, input$round_ids, col = 'level_name', by_arm = TRUE,
        percent = startsWith(input$y_display, 'percent'))
    }) |>
      bindCache(input$round_ids, input$y_display)
  })
}
