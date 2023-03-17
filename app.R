# create the ui object for the shiny app.
# separate tabPanels for Reach, ConnectEd, TaRL, Zones, and Status.
# each program then has its own set of tabPanels.

ui = navbarPage(
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  title = 'Youth Impact',

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
    # connected_ui('connected')
    # tabsetPanel(
      # connected_kpis_ui('connected_pooled'),
      # connected_ab_summary_ui('connected_ab_summary'),
      # connected_detailed_ui('connected_detailed'),
      # connected_advanced_ui('connected_advanced')
    # )
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

  # set advanced options for displaying ConnectEd results
  # keep_missing_conn = connected_advanced_server('connected_advanced')

  # process raw data for visualization
  data_conn = get_data_connected_server('get_data_connected', data_raw)
  # connected_server('connected', data_conn)

  # create display elements for ConnectEd pooled results
  # connected_pooled_server('connected_pooled', data_conn, keep_missing_conn)

  # create display elements for ConnectEd A/B Summary results
  # connected_ab_summary_server(
  #   'connected_ab_summary', data_conn, keep_missing_conn)

  # create display elements for ConnectEd A/B Detailed results
  # connected_ab_detailed_server(
  #   'connected_ab_detailed', data_conn, keep_missing_conn)

  data_tarlnum = get_data_tarlnum_server('get_data_tarlnum', data_raw)
  tarlnum_server('tarlnum', data_tarlnum)
}

# create the shiny app object
shiny::shinyApp(ui = ui, server = server)
