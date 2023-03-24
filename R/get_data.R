get_data_raw = function(folder_url) {
  files = get_file_metadata(folder_url)

  # round trip to simplify all.equal
  local_file = withr::local_tempfile()
  fwrite(files[, !'drive_resource'], local_file)
  files = fread(local_file)
  cache_dir = '_cache'

  metadata_path = '_file_metadata.csv'
  files_old = if (file.exists(metadata_path)) {
    fread(metadata_path)
  } else {
    data.table()
  }

  cols = c('name', 'modified_time', 'id')
  files_equal = all.equal(files[, ..cols], files_old[, ..cols])
  paths_exist = (length(files_old$path) > 0) && all(file.exists(files_old$path))

  if (isTRUE(files_equal) && paths_exist) {
    files[, path := files_old$path]
  } else {
    if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
    dir.create(cache_dir)
    local_paths = sapply(seq_len(nrow(files)), \(i) {
      local_path = file.path(cache_dir, files$name[i])
      drive_download(as_id(files$id[i]), local_path, overwrite = TRUE)
      local_path
    })
    files[, path := local_paths]
    fwrite(files, metadata_path)
  }

  data_raw = lapply(seq_len(nrow(files)), \(i) {
    if (endsWith(files$name[i], '.csv')) {
      fread(files$path[i])
    } else if (endsWith(files$name[i], '.dta')) {
      setDT(haven::read_dta(files$path[i]))
    } else {
      NULL
    }
  })

  names(data_raw) = tools::file_path_sans_ext(files$name)
  data_raw$`_file_metadata` = files
  data_raw # list of data.tables
}

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

get_data_connected = function(data_raw, keep_missing = c()) {
  data_wide = copy(data_raw$connected_data)
  rounds = copy(data_raw$connected_rounds)
  arms = copy(data_raw$connected_arms)
  treatments = copy(data_raw$connected_treatments)
  numeracy_levels = copy(data_raw$numeracy_levels)

  rounds[, label := glue(
    '{round_name} ({year}, Term {term})', .envir = .SD)]

  # use factors to ensure proper ordering in plots
  arms[, treatment_name := forcats::fct_reorder(
    treatment_name, treatment_id, .fun = \(x) x[1L])]
  numeracy_levels[, level_name := factor(level_name, rev(level_name))]

  # basic renaming and selecting particular columns
  data_wide = data_wide[, .(
    round = as.integer(round),
    treatment,
    facilitator_name = as.character(facilitator_i),
    student_id = sprintf('P%08d', seq_len(.N)),
    student_level_baseline = as.integer(stud_level_bl),
    student_level_endline = as.integer(stud_level),
    student_sex_baseline = as.character(forcats::as_factor(stud_sex_bl)),
    region_baseline = forcats::as_factor(region_bl)
  )]
  data_wide[round == 5L & treatment == '', treatment := 'Caregiver Choice']

  # filter missing data
  if (!('baseline' %in% keep_missing)) {
    data_wide = data_wide[!is.na(student_level_baseline)]
  }
  if (!('endline' %in% keep_missing)) {
    data_wide = data_wide[!is.na(student_level_endline)]
  }

  # add and remove columns
  data_wide = merge(data_wide, arms, by = c('round', 'treatment'))
  setnames(data_wide, 'round', 'round_name')
  data_wide[, treatment := NULL]
  arms[, c('round', 'treatment') := NULL]

  # convert to long format for some plots
  meas_vars = c('student_level_baseline', 'student_level_endline')
  data_long = melt(
    data_wide, measure.vars = meas_vars,
    variable.name = 'timepoint', value.name = 'level_id') |>
    merge(numeracy_levels, by = 'level_id', all.x = TRUE, sort = FALSE)

  data_long[, timepoint := factor(
    timepoint, meas_vars, c('Baseline', 'Endline'))]
  data_long[, level_beginner := level_id == 0]
  data_long[, level_ace := level_id == 4]

  # add other columns for plotting
  data_wide[, timepoint := 'Baseline\nto Endline']
  data_wide[, student_level_diff :=
              student_level_endline - student_level_baseline]
  data_wide[, level_improved := student_level_diff > 0]

  list(data_wide = data_wide, data_long = data_long, rounds = rounds,
       treatments = treatments, arms = arms, numeracy_levels = numeracy_levels)
}

get_data_tarlnum = function(data_raw, keep_missing = 'Midline') {
  data_long = copy(data_raw$tarl_data)
  numeracy_levels = copy(data_raw$numeracy_levels)

  data_long = unique(data_long)[uid_s != '']
  numeracy_levels[, level_name := factor(level_name, rev(level_name))]

  setnames(data_long, 'round', 'timepoint')
  setnames(data_long, \(x) str_replace(x, '^stu_', 'student_'))
  setcolorder(data_long, 'timepoint', before = 'student_level')

  data_long[, term := as.integer(str_extract(term, '[0-9]+$'))]
  data_long[, delivery_type := fifelse(
    delivery_type == 'Model School', 'Direct', 'Government')]

  data_long[, duration := as.integer(str_extract(imp_length, '^[0-9]+'))]
  data_long[, imp_length := NULL]

  data_long[, timepoint := factor(
    timepoint, c('Baseline', 'Midline', 'Endline'))]
  data_long[, student_level_fct := factor(
    student_level, levels(numeracy_levels$level_name))]
  data_long[, student_id := paste(
    year, term, delivery_type, duration, region, school_name, uid_s, sep = '|')]
  data_long[, student_level := NULL]
  data_long[, student_level_int := as.integer(student_level_fct)]

  # filter rows missing duration
  data_long = data_long[!is.na(duration)]

  # filter data missing certain timepoints
  timepoints_reqd = setdiff(levels(data_long$timepoint), keep_missing)
  data_long = data_long[
    , if (all(timepoints_reqd %in% timepoint[!is.na(student_level_int)])) .SD,
    by = student_id]

  # filter midline data
  data_long = data_long[timepoint != 'Midline']

  data_long[, year_term := paste0(year, ' T', term)]
  data_long[, year_term_num := round(year + (term - 1) / 3, 2)]
  data_long[, level_beginner := student_level_fct == 'Beginner']
  data_long[, level_ace := student_level_fct == 'Division']

  data_wide = 0

  setkey(data_long) # important for diff student_level
  list(data_long = data_long[], numeracy_levels = numeracy_levels)
}

get_data_filtered = function(x, filt) {
  y = lapply(x, \(d) {
    if (any(colnames(filt) %in% colnames(d))) {
      by_cols = intersect(colnames(filt), colnames(d))
      merge(d, filt, by = by_cols, sort = FALSE, allow.cartesian = TRUE)
    } else {
      copy(d)
    }
  })
  names(y) = names(x)
  y
}
