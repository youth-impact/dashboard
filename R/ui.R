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
      connected_pooled_ui('connected_pooled', conn),
      connected_ab_summary_ui('connected_ab_summary', conn),
      connected_ab_detailed_ui('connected_ab_detailed', conn)
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
