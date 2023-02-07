ui = navbarPage(
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  'Youth Impact',

  tabPanel(
    'Reach',
    'reach stuff'
  ),

  tabPanel(
    'ConnectEd',
    tabsetPanel(
      connected_pooled_ui('connected_pooled'),
      connected_ab_summary_ui('connected_ab_summary'),
      connected_ab_detailed_ui('connected_ab_detailed')
    )
  ),

  tabPanel(
    'TaRL',
    'tarl stuff'
  ),

  tabPanel(
    'Zones',
    'zones stuff'
  )
)

server = function(input, output, session) {
  connected_pooled_server('connected_pooled', connected)
  connected_ab_summary_server('connected_ab_summary', connected)
  connected_ab_detailed_server('connected_ab_detailed', connected)
}

shiny::shinyApp(ui = ui, server = server)
