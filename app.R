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
  ),

  tabPanel(
    'Status',
    data_status_ui('data_status')
  )
)

server = function(input, output, session) {
  data_raw = get_data_raw_server('get_data_raw', params$folder_url)
  data_proc = get_data_proc_server('get_data_proc', data_raw)
  data_status_server('data_status', data_raw)

  connected_pooled_server('connected_pooled', data_proc)
  connected_ab_summary_server('connected_ab_summary', data_proc)
  connected_ab_detailed_server('connected_ab_detailed', data_proc)
}

shiny::shinyApp(ui = ui, server = server)
