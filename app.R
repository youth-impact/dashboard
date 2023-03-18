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
    tabsetPanel(
      connected_kpis_ui('connected_kpis'),
      connected_detailed_ui('connected_detailed')
    )
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
  data_conn = get_data_connected_server('get_data_connected', data_raw)
  connected_kpis_server('connected_kpis', data_conn)
  connected_detailed_server('connected_detailed', data_conn)

  # TaRL numeracy
  data_tarlnum = get_data_tarlnum_server('get_data_tarlnum', data_raw)
  tarlnum_server('tarlnum', data_tarlnum)
}

# create the shiny app object
shiny::shinyApp(ui = ui, server = server)
