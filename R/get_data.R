get_data_ui = function(id) {
  ns = NS(id)

  verbatimTextOutput(ns('validation_checks'))
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

    data_proc = reactive({
      req(data_drive)
      get_data_proc(data_drive())
    })
  })
}
