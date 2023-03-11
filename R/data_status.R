# data_status module shows metadata for files loaded from Google Drive

data_status_ui = function(id) {
  ns = NS(id)

  tagList(
    tableOutput(ns('raw_file_metadata')) # output$raw_file_metadata
  )
}

data_status_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    output$raw_file_metadata = renderTable({
      req(data_raw)
      data_raw()$`_file_metadata`
    })
  })
}
