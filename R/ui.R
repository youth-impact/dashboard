ui = fluidPage(
  titlePanel('ConnectEd'),

  tabsetPanel(

    tabPanel(
      'Pooled Summary Results',
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            inputId = 'round_ids',
            label = 'Round(s)',
            choices = sort(unique(conn$data$round_id)),
            selected = unique(conn$data$round_id)),
          width = 2),
        mainPanel(
          plotOutput('plot1'),
          br(),
          plotOutput('plot2'),
          width = 10)
      )
    ),

    tabPanel(
      'A/B Summary Results',
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = 'round_id_summ',
            label = 'Round',
            choices = sort(unique(conn$data$round_id)),
            selected = max(conn$data$round_id)),
          width = 2),
        mainPanel(
          plotOutput('plot3'),
          br(),
          plotOutput('plot4'),
          width = 10)
      )
    ),

    tabPanel(
      'A/B Detailed Results',
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = 'round_id_detl',
            label = 'Round',
            choices = sort(unique(conn$data$round_id)),
            selected = max(conn$data$round_id)),
          radioButtons(
            inputId = 'y_detl',
            label = 'Display as',
            choices = c('percentages', 'counts')),
          width = 2),
        mainPanel(
          plotOutput('plot5', width = '70%'),
          width = 10)
      )
    )
  )
)
