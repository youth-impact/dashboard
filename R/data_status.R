data_status_ui = function(id) {
  ns = NS(id)

  tagList(
    tableOutput(ns('raw_file_metadata'))
  )
}

data_status_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    output$raw_file_metadata = renderTable({
      data_raw()$`_file_metadata`
    })
  })
}
