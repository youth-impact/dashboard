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

connected_ab_detailed_server = function(id, connected) {
  moduleServer(id, function(input, output, session) {

    d_long = copy(connected$data_long)
    d_long[, time := factor(
      time, c('Sensitization', 'Endline'), c('Sens.', 'Endline'))]
    rounds_avail = sort(unique(connected$data$round_id))

    output$ui_input = renderUI({
      ns = session$ns
      tagList(
        radioButtons(
          inputId = ns('round_ids'),
          label = 'Round',
          choices = rounds_avail,
          selected = max(rounds_avail)),
        radioButtons(
          inputId = ns('y_display'),
          label = 'Display as',
          choices = c('percentage', 'count'))
      )
    })

    output$round_text = renderText({
      req(input$round_ids)
      rounds_now = connected$rounds[round_id == input$round_ids]
      glue('Round {rounds_now$round_id}: {rounds_now$round_desc}')
    }) |>
      bindCache(input$round_ids)

    output$plot_all = renderPlot({
      req(input$round_ids, input$y_display)
      get_detailed_barplot(
        d_long, input$round_ids, col = 'level_name', by_arm = TRUE,
        percent = startsWith(input$y_display, 'percent'))
    }) |>
      bindCache(input$round_ids, input$y_display)
  })
}
