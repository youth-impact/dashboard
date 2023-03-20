# create the ui object for the shiny app

ui = navbarPage(
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  title = 'Youth Impact',
  selected = 'TaRL Numeracy', # temporary

  tabPanel(
    title = 'Reach',
    'reach stuff' # placeholder
  ),

  tabPanel(
    title = 'Zones',
    'zones stuff' # placeholder
  ),

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
    'tarl literacy stuff' # placeholder
  ),

  tabPanel(
    'Status',
    data_status_ui('data_status')
  )
)

# create the server object for the shiny app
server = function(input, output, session) {
  # load raw data from the Google Drive folder
  data_raw = get_data_raw_server('get_data_raw', params$folder_url)

  # create display elements for status of raw data files
  data_status_server('data_status', data_raw)

  # ConnectEd
  connected_server('connected', data_raw)

  # TaRL numeracy
  tarlnum_server('tarlnum', data_raw)
}

# create the shiny app object
shiny::shinyApp(ui = ui, server = server)
