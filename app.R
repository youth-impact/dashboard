# create the ui object for the shiny app
# separated into tabPanels for Reach, ConnectEd, TaRL, Zones, and Status
# a given program's tabPanel consists of a set of tabPanels for the plots
ui = navbarPage(
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  title = 'Youth Impact',

  tabPanel(
    title = 'Reach',
    'reach stuff' # placeholder
  ),

  tabPanel(
    title = 'ConnectEd',
    tabsetPanel(
      connected_pooled_ui('connected_pooled'),
      connected_ab_summary_ui('connected_ab_summary'),
      connected_ab_detailed_ui('connected_ab_detailed')
    )
  ),

  tabPanel(
    title = 'TaRL',
    'tarl stuff' # placeholder
  ),

  tabPanel(
    title = 'Zones',
    'zones stuff' # placeholder
  ),

  tabPanel(
    'Status',
    data_status_ui('data_status')
  )
)

# create the server object for the shiny app
server = function(input, output, session) {
  data_raw = get_data_raw_server('get_data_raw', params$folder_url)
  data_proc = get_data_proc_server('get_data_proc', data_raw)
  data_status_server('data_status', data_raw)

  connected_pooled_server('connected_pooled', data_proc)
  connected_ab_summary_server('connected_ab_summary', data_proc)
  connected_ab_detailed_server('connected_ab_detailed', data_proc)
}

# create the shiny app object
shiny::shinyApp(ui = ui, server = server)
