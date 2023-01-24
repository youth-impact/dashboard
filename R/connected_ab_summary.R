connected_ab_summary_ui = function(id, conn) {
  ns = NS(id)
  round_ids = sort(unique(conn$data$round_id))

  tabPanel(
    'A/B Summary Results',
    sidebarLayout(

      sidebarPanel(
        radioButtons(
          inputId = ns('round_ids'),
          label = 'Round',
          choices = round_ids,
          selected = max(round_ids)),
        width = 2),

      mainPanel(
        h6(textOutput(ns('round_text'))),
        br(),
        plotOutput(ns('plot1')),
        br(),
        plotOutput(ns('plot2')),
        width = 10)
    )
  )
}

connected_ab_summary_server = function(id, conn) {

  moduleServer(
    id,
    function(input, output, session) {

      d = copy(conn$data)[, time := 'Sens. to\nEndline']
      d_long = copy(conn$data_long)
      d_long[, time := factor(
        time, c('Sensitization', 'Endline'), c('Sens.', 'Endline'))]

      output$round_text = renderText({
        r = conn$rounds[round_id == input$round_ids]
        glue('Round {r$round_id}: {r$round_desc}')
      })

      output$plot1 = renderPlot({
        p1 = get_summary_barplot(
          d_long, input$round_ids, col = 'can_add', col_val = FALSE,
          title = 'Innumeracy', nudge_y = 0.01,
          fill_vals = c('#a6cee3', '#1f78b4'), by_arm = TRUE)

        p2 = get_summary_barplot(
          d_long, input$round_ids, col = 'can_divide', col_val = TRUE,
          title = 'Numeracy', nudge_y = 0.02,
          fill_vals = c('#b2df8a', '#33a02c'), by_arm = TRUE)

        plot_grid(p1, p2, nrow = 1L, align = 'h', axis = 'tblr')
      })

      output$plot2 = renderPlot({
        p1 = get_summary_barplot(
          d, input$round_ids, col = 'improved', col_val = TRUE,
          title = 'Improved', nudge_y = 0.04, fill_vals = '#fdbf6f',
          by_arm = TRUE)

        p2 = get_summary_barplot(
          d_long, input$round_ids, col = 'present', col_val = TRUE,
          title = 'Totals', nudge_y = 0, fill_vals = c('#cab2d6', '#6a3d9a'),
          by_arm = TRUE, percent = FALSE)

        plot_grid(
          p1, grid::nullGrob(), p2, nrow = 1L,
          align = 'h', axis = 'tb', rel_widths = c(0.8, 0.23, 0.97))
      })
    }
  )
}
