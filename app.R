# create the ui object for the shiny app
ui = navbarPage(
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  title = 'Youth Impact',
  selected = 'ConnectEd', # temporary

  tabPanel(
    title = 'Reach',
    reach_ui('reach')
  ),

  # tabPanel(
  #   title = 'Zones',
  #   'zones stuff' # placeholder
  # ),

  tabPanel(
    title = 'ConnectEd',
    connected_ui('connected')
  ),

  tabPanel(
    title = 'TaRL Numeracy',
    tarlnum_ui('tarlnum')
  ),

  tabPanel(
    title = 'TaRL Literacy',
    tarllit_ui('tarllit')
  ),

  tabPanel(
    title = 'Data Validation',
    get_data_ui('get_data')
  )
)

# create the server object for the shiny app
server = function(input, output, session) {
  # load data
  data_proc = get_data_server('get_data', params$folder_url)

  # Reach
  reach_server('reach', data_proc)

  # ConnectEd
  connected_server('connected', data_proc)

  # TaRL numeracy
  tarlnum_server('tarlnum', data_proc)

  # TaRL literacy
  tarllit_server('tarllit', data_proc)
}

# create the shiny app object
shiny::shinyApp(ui = ui, server = server)
