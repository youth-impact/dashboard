# load raw data from the Google Drive folder
get_data_raw_server = function(id, folder_url) {
  moduleServer(id, function(input, output, session) {

    # reactive data source makes sure app has latest data from Google Drive
    data_raw = reactivePoll(
      intervalMillis = 1000 * 60 * 60, # 1 hour
      session = session,

      # use file names and modification times to
      # determine if underlying data have changed
      checkFunc = \() {
        files = get_file_metadata(folder_url)
        paste(files$name, files$modified_time, collapse = ' __ ')
      },

      # download and read files in the given folder
      valueFunc = \() get_data_raw(folder_url)
    )
  })
}

# process raw ConnectEd data
get_data_connected_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    data_proc = reactive({
      req(data_raw)
      get_data_connected(data_raw())
    })
  })
}

# process raw TaRL data
get_data_tarlnum_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    data_proc = reactive({
      req(data_raw)
      get_data_tarlnum(data_raw())
    })
  })
}
