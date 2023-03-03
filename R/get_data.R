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

        data_raw = lapply(seq_len(nrow(files)), \(i) {
          local_file = withr::local_tempfile()
          drive_download(files$id[i], local_file)
          if (endsWith(files$name[i], '.csv')) {
            fread(local_file)
          } else if (endsWith(files$name[i], '.dta')) {
            setDT(haven::read_dta(local_file))
          } else {
            NULL
          }
        })

        names(data_raw) = tools::file_path_sans_ext(files$name)
        data_raw$`_file_metadata` = files[, !'drive_resource']
        data_raw
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

      arms[, treatment_name := forcats::fct_reorder(
        treatment_name, treatment_id, .fun = \(x) x[1L])]
      levs[, level_name := factor(level_name, level_name)]

      data = get_clean_connected_data(data) |>
        merge(arms, by = c('round', 'treatment'))

      setnames(data, 'round', 'round_name')
      data[, treatment := NULL]
      arms[, c('round', 'treatment') := NULL]

      data[, student_level_diff := student_level_endline - student_level_baseline]
      data[, improved := student_level_diff > 0]

      meas_vars = c('student_level_baseline', 'student_level_endline')
      data_long = melt(
        data, measure.vars = meas_vars,
        variable.name = 'time', value.name = 'level_id') |>
        merge(levs, by = 'level_id', all.x = TRUE, sort = FALSE)

      data_long[, time := factor(time, meas_vars, c('Baseline', 'Endline'))]
      data_long[, can_add := level_id > 0]
      data_long[, can_divide := level_id == 4]
      data_long[, present := !is.na(level_id)]

      list(data = data, data_long = data_long,
           rounds = rounds, arms = arms, levs = levs)
    })
  })
}
