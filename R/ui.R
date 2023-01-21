ui = fluidPage(
  titlePanel('ConnectEd'),

  tabsetPanel(

    tabPanel(
      'Pooled Summary Results',
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            inputId = 'rounds',
            label = 'Round(s)',
            choices = sort(unique(conn$data$round_id)),
            selected = unique(conn$data$round_id)),
          width = 2),
        mainPanel(
          plotOutput('plot1'),
          plotOutput('plot3', width = '36.4%'),
          width = 10)
      )
    ),

    tabPanel(
      'A/B Summary Results',
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = 'round',
            label = 'Round',
            choices = sort(unique(conn$data$round_id)),
            selected = max(conn$data$round_id)),
          width = 2),
        mainPanel(
          plotOutput('plot2'),
          plotOutput('plot4', width = '36.4%'),
          width = 10)
      )
    )
  )
)
