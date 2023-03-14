get_data_raw = function(folder_url) {
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

get_data_connected = function(data_raw, keep_missing = c()) {
  data_wide = copy(data_raw$connected_data)
  rounds = copy(data_raw$connected_rounds)
  arms = copy(data_raw$connected_arms)
  treatments = copy(data_raw$connected_treatments)
  levs = copy(data_raw$connected_levels)

  rounds[, label := glue(
    '{round_name} ({year}, Term {term})', .envir = .SD)]

  # use factors to ensure proper ordering in plots
  arms[, treatment_name := forcats::fct_reorder(
    treatment_name, treatment_id, .fun = \(x) x[1L])]
  levs[, level_name := factor(level_name, level_name)]

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
    merge(levs, by = 'level_id', all.x = TRUE, sort = FALSE)

  data_long[, timepoint := factor(
    timepoint, meas_vars, c('Baseline', 'Endline'))]
  data_long[, cannot_add := level_id == 0] # tarl innumeracy
  data_long[, can_divide := level_id == 4] # tarl numeracy

  # add other columns for plotting
  data_wide[, timepoint := 'Baseline\nto Endline']
  data_wide[, student_level_diff :=
              student_level_endline - student_level_baseline]
  data_wide[, improved := student_level_diff > 0] # moved up

  list(data_wide = data_wide, data_long = data_long, rounds = rounds,
       treatments = treatments, arms = arms, levs = levs)
}

get_data_tarl = function(data_raw, keep_missing = c()) {
  data_long = copy(data_raw$tarl_data)

  levs = c('Beginner', 'Addition', 'Subtraction', 'Multiplication', 'Division')
  data_long = unique(data_long)[uid_s != '']

  setnames(data_long, 'round', 'timepoint')
  setnames(data_long, \(x) str_replace(x, '^stu_', 'student_'))
  setcolorder(data_long, 'timepoint', before = 'student_level')

  data_long[, term := as.integer(str_extract(term, '[0-9]+$'))]
  data_long[, delivery_type := paste(fifelse(
    delivery_type == 'Model', 'Direct', 'Government'), 'Delivery')]
  data_long[, imp_length := paste(
    str_extract(imp_length, '^[0-9]+'), 'days')]
  data_long[, timepoint := factor(
    timepoint, c('Baseline', 'Midline', 'Endline'))]
  data_long[, student_level := factor(student_level, levs)]
  data_long[, student_id := paste(
    year, term, delivery_type, uid_s, sep = '|')]
  setkey(data_long)

  # TODO: use keep_missing
  list(data_wide = data_wide, data_long = data_long)
}


get_data_connected_overall = function(data_proc, round_ids) {
  d1 = copy(data_proc$data_long)[
    round_id %in% round_ids,
    .(n_noadd = sum(cannot_add),
      n_div = sum(can_divide),
      n_total = .N),
    keyby = .(round_id, round_name, arm_id, treatment_name, timepoint)]

  setnames(d1, 'timepoint', 'Timepoint')
  d1[, pct_noadd := n_noadd / n_total]
  d1[, pct_div := n_div / n_total]
  d1[, label := paste('Intervention:', treatment_name)]

  d2 = dcast(
    d1, round_name + arm_id + label ~ Timepoint,
    value.var = c('pct_div', 'pct_noadd'))

  d2 = copy(data_proc$data_wide)[
    round_id %in% round_ids,
    .(pct_improved = sum(improved, na.rm = TRUE) / sum(!is.na(improved))),
    keyby = .(round_id, round_name, arm_id, treatment_name)] |>
    merge(d2)

  list(long = d1, wide = d2)
}
