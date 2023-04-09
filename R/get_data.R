get_data_raw = function(project, dataset) {
  con = dbConnect(bigrquery::bigquery(), project = project, dataset = dataset)
  file_metadata = setDT(dbReadTable(con, '_file_metadata'))

  cache_ok = FALSE
  file_metadata_path = '_cache_file_metadata.qs'
  data_raw_path = '_cache_data_raw.qs'
  if (file.exists(file_metadata_path) && file.exists(data_raw_path)) {
    file_metadata_old = qs::qread(file_metadata_path)
    if (isTRUE(all.equal(file_metadata, file_metadata_old))) {
      cache_ok = TRUE
    }
  }

  if (cache_ok) {
    data_raw = qs::qread(data_raw_path)
  } else {
    table_names = dbListTables(con)
    data_raw = lapply(table_names, \(x) setDT(dbReadTable(con, x)))
    names(data_raw) = table_names
    qs::qsave(data_raw, data_raw_path)
    qs::qsave(file_metadata, file_metadata_path)
  }

  for (i in seq_len(length(data_raw))) {
    data_raw[[i]][, c('_load_uuid', '_load_emitted_at') := NULL]
  }
  data_raw # list of data.tables
}

get_data_proc_connected = function(data_raw) {
  # use factors to ensure proper ordering in plots
  rounds = copy(data_raw$connected_rounds)
  rounds[, label := glue('{round_name} ({year}, Term {term})', .envir = .SD)]

  arms = merge(
    data_raw$connected_arms, data_raw$connected_treatments, by = 'treatment_id')
  arms[, treatment_name := forcats::fct_reorder(
    treatment_name, treatment_id, .fun = \(x) x[1L])]

  assessments = data_raw$connected_assessments |>
    merge(data_raw$connected_students, by = 'student_id') |>
    merge(arms[, !'treatment_description'], by = 'arm_id')

  # keep only students ascertained at baseline and endline
  assessments = assessments[
    (timepoint %in% c('Baseline', 'Endline')) & !is.na(student_level_int),
    if (.N == 2L) .SD, by = .(student_id, arm_id)]

  student_levels = unique(
    assessments[, .(student_level_int, student_level_str)])
  setkey(student_levels)

  assessments[, student_level_str := factor(
    student_level_str, student_levels$student_level_str)]
  assessments[, timepoint := factor(timepoint, c('Baseline', 'Endline'))]
  assessments[, treatment_wrap := str_wrap(treatment_name, 20L)]
  setkey(assessments, student_id, timepoint)

  by_cols = setdiff(
    colnames(assessments),
    c('timepoint', 'student_level_int', 'student_level_str'))

  assessments_wide = assessments[, .(
    student_level_baseline = student_level_str[timepoint == 'Baseline'],
    student_level_endline = student_level_str[timepoint == 'Endline'],
    student_level_diff = diff(student_level_int)), # ordered by timepoint
    by = by_cols]

  assessments[, level_beginner := student_level_str == 'Beginner']
  assessments[, level_ace := student_level_str == 'Division']
  assessments_wide[, level_improved := student_level_diff > 0]
  assessments_wide[, timepoint := 'Baseline to Endline']
  setkey(assessments_wide, student_id)

  data_proc_connected = list(
    connected_rounds = rounds, connected_arms = arms,
    connected_long = assessments, connected_wide = assessments_wide)
}

get_data_proc_tarlnum = function(data_raw) {
  assessments = data_raw$tarlnum_assessments |>
    merge(data_raw$tarlnum_students, by = 'student_id') |>
    merge(data_raw$tarlnum_schools, by = c('school_id', 'school_name')) |>
    merge(data_raw$tarlnum_implementations, by = 'impl_id')

  assessments = assessments[
    (timepoint %in% c('Baseline', 'Endline')) & !is.na(student_level_int),
    if (.N == 2L) .SD, by = student_id]

  student_levels = unique(
    assessments[, .(student_level_int, student_level_str)])
  setkey(student_levels)

  assessments[, student_level_str := factor(
    student_level_str, student_levels$student_level_str)]
  assessments[, timepoint := factor(timepoint, c('Baseline', 'Endline'))]
  assessments[, year_term := paste0(year, ' T', term)]
  assessments[, year_term_num := round(year + (term - 1) / 3, 2)]
  setkey(assessments, student_id, timepoint)

  by_cols = setdiff(
    colnames(assessments),
    c('timepoint', 'student_level_int', 'student_level_str'))

  assessments_wide = assessments[, .(
    student_level_baseline = student_level_str[timepoint == 'Baseline'],
    student_level_endline = student_level_str[timepoint == 'Endline'],
    student_level_diff = diff(student_level_int)), # ordered by timepoint
    by = by_cols]

  assessments[, level_beginner := student_level_str == 'Beginner']
  assessments[, level_ace := student_level_str == 'Division']
  assessments_wide[, level_improved := student_level_diff > 0]
  assessments_wide[, timepoint := 'Baseline to Endline']
  setkey(assessments_wide, student_id)

  data_proc_tarlnum = list(
    tarlnum_long = assessments, tarlnum_wide = assessments_wide)
}

get_data_proc_reach = function(data_raw) {
  tarlnum_students = data_raw$tarlnum_students |>
    merge(data_raw$tarlnum_implementations, by = 'impl_id')
  tarlnum_students[, program := 'TaRL Numeracy']

  connected_students = data_raw$connected_students |>
    merge(data_raw$connected_arms, by = 'arm_id') |>
    merge(data_raw$connected_rounds[, !c('purpose', 'conclusion')],
          by = 'round_id')
  connected_students[, delivery_model := 'Direct']
  connected_students[, program := 'ConnectEd']

  students = rbind(connected_students, tarlnum_students, fill = TRUE)
  students[, year_term := paste0(year, ' T', term)]
  students[, year_term_num := round(year + (term - 1) / 3, 2)]
  students[is.na(student_gender), student_gender := 'Unknown']

  data_proc_reach = list(reach_students = students)
}

get_data_proc = function(data_raw) {
  data_connected = get_data_proc_connected(data_raw)
  data_tarlnum = get_data_proc_tarlnum(data_raw)
  data_reach = get_data_proc_reach(data_raw)
  data_proc = c(data_connected, data_tarlnum, data_reach)
}

get_data_server = function(id, project, dataset) {
  moduleServer(id, function(input, output, session) {

    # reactive data source makes sure app has latest data
    data_raw = reactivePoll(
      intervalMillis = 1000 * 60 * 60, # 1 hour
      session = session,

      checkFunc = \() {
        con = dbConnect(
          bigrquery::bigquery(), project = project, dataset = dataset)
        dbReadTable(con, '_file_metadata')$`_load_emitted_at`[1L]
      },

      valueFunc = \() get_data_raw(project, dataset)
    )

    data_proc = reactive({
      req(data_raw)
      get_data_proc(data_raw())
    })
  })
}
