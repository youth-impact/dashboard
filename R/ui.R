ui = navbarPage(
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  'Youth Impact',

  tabPanel(
    'Reach',
    'reach stuff'
  ),

  tabPanel(
    'ConnectEd',
    connected_ui('connected', conn),
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
