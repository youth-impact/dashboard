get_data_ui = function(id) {
  ns = NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(ns('ui_issue')),
      width = 3
    ),
    mainPanel(
      verbatimTextOutput(ns('validation_checks')),
      width = 9
    )
  )
}

get_data_server = function(id, folder_url) {
  moduleServer(id, function(input, output, session) {

    # reactive data source makes sure app has latest data
    data_drive = reactivePoll(
      intervalMillis = 1000 * 60 * 60, # 1 hour
      session = session,

      checkFunc = \() {
        metadata = get_metadata_drive(folder_url)
        paste(metadata$name, metadata$modified_time, collapse = ' __ ')
      },

      valueFunc = \() get_data_drive(folder_url)
    )

    output$validation_checks = renderPrint({
      req(data_drive)
      get_data_validation(data_drive())
    })

    output$ui_issue = renderUI({
      # req(data_drive)
      # ns = session$ns
      get_issue_button()
    })

    data_proc = reactive({
      req(data_drive)
      get_data_proc(data_drive())
    })
  })
}
