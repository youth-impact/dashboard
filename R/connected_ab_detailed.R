connected_ab_detailed_ui = function(id, conn) {
  ns = NS(id)
  round_ids = sort(unique(conn$data$round_id))

  tabPanel(
    'A/B Detailed Results',
    sidebarLayout(

      sidebarPanel(
        radioButtons(
          inputId = ns('round_ids'),
          label = 'Round',
          choices = round_ids,
          selected = max(round_ids)),
        radioButtons(
          inputId = ns('y_display'),
          label = 'Display as',
          choices = c('percentage', 'count')),
        width = 2),

      mainPanel(
        h6(textOutput(ns('round_text'))),
        br(),
        plotOutput(ns('plot1'), width = '70%'),
        width = 10)
    )
  )
}

connected_ab_detailed_server = function(id, conn) {

  moduleServer(
    id,
    function(input, output, session) {

      d_long = copy(conn$data_long)
      d_long[, time := factor(
        time, c('Sensitization', 'Endline'), c('Sens.', 'Endline'))]

      output$round_text = renderText({
        r = conn$rounds[round_id == input$round_ids]
        glue('Round {r$round_id}: {r$round_desc}')
      })

      output$plot1 = renderPlot({
        p = get_detailed_barplot(
          d_long, input$round_ids, col = 'level_name', by_arm = TRUE,
          percent = startsWith(input$y_display, 'percent'))
        p
      })
    }
  )
}
