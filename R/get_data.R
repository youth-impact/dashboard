get_data_raw_server = function(id, folder_url) {
  moduleServer(id, function(input, output, session) {

    data_raw = reactivePoll(
      intervalMillis = 1000 * 60 * 60, # 1 hour
      session = session,

      checkFunc = \() {
        files = get_file_metadata(folder_url)
        paste(files$name, files$modified_time, collapse = ' __ ')
      },

      valueFunc = \() {
        files = get_file_metadata(folder_url)

        data_list = lapply(files$id, \(id) {
          local_file = withr::local_tempfile()
          drive_download(id, local_file)
          fread(local_file)
        })

        names(data_list) = gsub('\\.csv$', '', files$name)
        data_list$`_file_metadata` = files[, !'drive_resource']
        data_list
      }
    )
  })
}

get_data_proc_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    data_proc = reactive({
      req(data_raw)

      data = data_raw()$connected_data
      rounds = data_raw()$connected_rounds
      arms = data_raw()$connected_arms
      levs = data_raw()$connected_levels

      arms[, arm_name := factor(arm_name, arm_name)]
      levs[, level_name := factor(level_name, level_name)]

      data = merge(data, arms, by = c('round_id', 'arm_id'))
      data[, improved := end_level > start_level]

      data_long = melt(
        data, measure.vars = c('start_level', 'end_level'),
        variable.name = 'time', value.name = 'level_id')
      data_long[, time := factor(
        time, c('start_level', 'end_level'), c('Sensitization', 'Endline'))]

      data_long = merge(data_long, levs, by = 'level_id', sort = FALSE)
      data_long[, can_add := level_id > 0]
      data_long[, can_divide := level_id == 4]
      data_long[, present := TRUE]

      list(data = data, data_long = data_long,
           rounds = rounds, arms = arms, levs = levs)
    })
  })
}
