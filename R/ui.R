ui = fluidPage(
  titlePanel('ConnectEd Results'),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = 'rounds',
        label = 'Round(s) included',
        choices = sort(unique(results$round_id)),
        selected = unique(results$round_id)
      ),
      width = 2
    ),

    mainPanel(
      plotOutput('plot1'),
      width = 10
    )
  )
)
