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

      # download and read csv and dta files in the given folder
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
        data_raw # list of data.tables
      }
    )
  })
}

# process raw data for visualization
get_data_proc_server = function(id, data_raw) {
  moduleServer(id, function(input, output, session) {

    data_proc = reactive({
      req(data_raw)

      data = data_raw()$connected_data
      rounds = data_raw()$connected_rounds
      arms = data_raw()$connected_arms
      levs = data_raw()$connected_levels

      # use factors to ensure proper ordering in plots
      arms[, treatment_name := forcats::fct_reorder(
        treatment_name, treatment_id, .fun = \(x) x[1L])]
      levs[, level_name := factor(level_name, level_name)]

      # basic renaming and selecting particular columns
      data = get_clean_connected_data(data) |>
        merge(arms, by = c('round', 'treatment'))

      # remove unused columns
      setnames(data, 'round', 'round_name')
      data[, treatment := NULL]
      arms[, c('round', 'treatment') := NULL]

      data[, student_level_diff := student_level_endline - student_level_baseline]
      data[, improved := student_level_diff > 0] # moved up at least one level

      # convert to long format for some plots
      meas_vars = c('student_level_baseline', 'student_level_endline')
      data_long = melt(
        data, measure.vars = meas_vars,
        variable.name = 'time', value.name = 'level_id') |>
        merge(levs, by = 'level_id', all.x = TRUE, sort = FALSE)

      data_long[, time := factor(time, meas_vars, c('Baseline', 'Endline'))]
      data_long[, cannot_add := level_id == 0] # tarl innumeracy
      data_long[, can_divide := level_id == 4] # tarl numeracy
      data_long[, present := !is.na(level_id)] # non-missing

      # add time column for plotting
      data[, time := 'Baseline\nto Endline']

      list(data = data, data_long = data_long,
           rounds = rounds, arms = arms, levs = levs)
    })
  })
}
