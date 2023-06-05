get_metadata_drive = function(folder_url) {
  metadata = setDT(drive_ls(folder_url))
  setkey(metadata, name)
  metadata[, modified_time := sapply(drive_resource, \(f) f$modifiedTime)]
  metadata[, mime_type := sapply(drive_resource, \(f) f$mimeType)]
  metadata[]
}

get_data_drive = function(folder_url) {
  metadata = get_metadata_drive(folder_url)
  metadata = metadata[!startsWith(name, '_')]
  metadata[, drive_resource := NULL]

  metadata_path = '_cache_metadata.qs'
  data_path = '_cache_data.qs'
  cache_ok = FALSE
  if (file.exists(metadata_path) && file.exists(data_path)) {
    metadata_old = qs::qread(metadata_path)
    if (isTRUE(all.equal(metadata, metadata_old))) {
      cache_ok = TRUE
    }
  }

  if (cache_ok) {
    data_drive = qs::qread(data_path)
  } else {
    data_drive = lapply(seq_len(nrow(metadata)), \(i) {
      local_path = tempfile()
      m = metadata[i]
      if (endsWith(m$name, '.csv') && m$mime_type == 'text/csv') {
        drive_download(m$id, local_path, overwrite = TRUE)
        fread(local_path)
      } else if (
        endsWith(m$name, '.dta') && m$mime_type == 'application/octet-stream') {
        drive_download(m$id, local_path, overwrite = TRUE)
        setDT(as_factor(haven::read_dta(local_path)))
      } else if (m$mime_type == 'application/vnd.google-apps.spreadsheet') {
        gsheet_meta = gs4_get(m$id)
        wsheet_names = gsheet_meta$sheets$name[
          !startsWith(gsheet_meta$sheets$name, '_')]
        wsheets = lapply(wsheet_names, \(x) setDT(read_sheet(m$id, x)))
        names(wsheets) = wsheet_names
        wsheets
      } else {
        NULL
      }
    })

    names(data_drive) = tools::file_path_sans_ext(metadata$name)
    data_drive = purrr::list_flatten(data_drive)
    names(data_drive) = gsub('_metadata_', '_', names(data_drive))

    data_drive$`_file_metadata` = metadata
    qs::qsave(data_drive, data_path)
    qs::qsave(metadata, metadata_path)
  }

  data_drive # list of data.tables
}

get_data_validation = function(data_drive) {
  get_counts = function(x) {
    c(uniqueN(x), sum(is.na(x)), sum(x == '', na.rm = TRUE))
  }
  n_types = c('unique', 'missing', 'empty string')

  # ConnectEd
  cols = c(
    'hh_id', 'stud_age_bl', 'stud_std_bl', 'stud_level_bl', 'school_id_bl',
    'school_name_bl', 'facilitator_id_i', 'facilitator_i', 'stud_level',
    'treatment', 'round', 'stud_sex_bl', 'region_bl')
  connected_1 = data_drive$connected_students[
    , lapply(.SD, get_counts), .SDcols = cols]
  connected_1[, n_type := n_types]
  setcolorder(connected_1, 'n_type')

  connected_2 = data_drive$connected_students[
    , .(n_facilitator_i = uniqueN(facilitator_i)),
    keyby = facilitator_id_i][n_facilitator_i > 1L]

  connected_3 = data_drive$connected_students[
    , .(n_school_name_bl = uniqueN(school_name_bl)),
    keyby = school_id_bl][n_school_name_bl > 1L]

  cat('### ConnectEd counts of unique, missing, and empty string values\n')
  print(connected_1)
  cat('\n### ConnectEd facilitator_id_i linked to >1 facilitator_i\n')
  print(connected_2)
  cat('\n### ConnectEd school_id_bl linked to >1 school_name_bl\n')
  print(connected_3)

  # TaRL Numeracy
  cols = c(
    'year', 'term', 'phase', 'delivery_type', 'imp_length', 'round',
    'region', 'school_name', 'school_id', 'uid_s', 'stu_gender', 'stu_std',
    'stu_age', 'stu_class', 'stu_level')
  tarlnum_1 = data_drive$tarlnum_assessments[
    , lapply(.SD, get_counts), .SDcols = cols]
  tarlnum_1[, n_type := n_types]
  setcolorder(tarlnum_1, 'n_type')

  tarlnum_2 = data_drive$tarlnum_assessments[
    , .(n_school_name = uniqueN(school_name)), keyby = school_id][
      n_school_name > 1L]

  cat(paste(
    '\n### TaRL Numeracy counts of unique,',
    'missing, and empty string values\n'))
  print(tarlnum_1)
  cat('\n### TaRL Numeracy school_id linked to >1 school_name\n')
  print(tarlnum_2)

  # TaRL Literacy
  cols = c(
    'imp_year', 'imp_term', 'imp_type', 'imp_duration',
    'region', 'school_name', 'school_id', 'stu_uid', 'stu_std','stu_class',
    'stu_gender', 'stu_age', 'stu_level_bsl', 'stu_level_end')
  tarllit_1 = data_drive$tarllit_students[
    , lapply(.SD, get_counts), .SDcols = cols]
  tarllit_1[, n_type := n_types]
  setcolorder(tarllit_1, 'n_type')

  tarllit_2 = data_drive$tarllit_students[
    , .(n_school_name = uniqueN(school_name)), keyby = school_id][
      n_school_name > 1L]

  cat(paste(
    '\n### TaRL Literacy counts of unique,',
    'missing, and empty string values\n'))
  print(tarllit_1)
  cat('\n### TaRL Literacy school_id linked to >1 school_name\n')
  print(tarllit_2)

  # Zones
  zones_assessments = merge(
    rbindlist(
      data_drive[startsWith(names(data_drive), 'zones_assessments_')],
      fill = TRUE, idcol = 'file_name'),
    data_drive$zones_files, by = 'file_name')

  cols = c(
    'year', 'term', 'round', 'student_gender', 'school_name',
    'fac_id', 'k_hiv_least_10to19', 'k_hiv_riskiest_older')
  zones_1 = zones_assessments[, lapply(.SD, get_counts), .SDcols = cols]
  zones_1[, n_type := n_types]
  setcolorder(zones_1, 'n_type')

  cat('\n### Zones counts of unique, missing, and empty string values\n')
  print(zones_1)

  zones_2 = zones_assessments[, .(
    n_total = .N,
    n_missing_older = sum(is.na(k_hiv_riskiest_older)),
    n_missing_10to19 = sum(is.na(k_hiv_least_10to19))),
    keyby = .(file_name, round)]

  cat('\n### Zones counts of total and missing values per file per timepiont\n')
  print(zones_2)

  invisible()
}

get_data_proc = function(data_drive) {
  assert_names(
    names(data_drive),
    must.include = c(
      'connected_arms', 'connected_rounds', 'connected_students',
      'connected_treatments', 'numeracy_levels', 'tarlnum_assessments',
      'literacy_levels', 'tarllit_students', 'zones_files'))

  dp = lapply(data_drive, copy)

  ### numeracy_levels
  setkeyv(dp$numeracy_levels, 'level_int')
  numer_levs = dp$numeracy_levels$level_str
  dp$numeracy_levels[, level_str := factor(level_str, numer_levs)]

  ### literacy_levels
  setkeyv(dp$literacy_levels, 'level_int')
  liter_levs = dp$literacy_levels$level_str
  dp$literacy_levels[, level_str := factor(level_str, liter_levs)]

  ### connected_treatments
  # ok as is

  ### connected_rounds
  dp$connected_rounds[, `:=`(
    round_label = glue('{round_name} ({year} T{term})', .envir = .SD),
    year_term_str = glue('{year} T{term}', .envir = .SD),
    year_term_num = year + (term - 1) / 3)]
  setkeyv(dp$connected_rounds, 'round_id')

  ### connected_students
  arms_tmp = dp$connected_arms |>
    merge(dp$connected_treatments, by = 'treatment_id') |>
    merge(dp$connected_rounds, by = 'round_id')

  students_tmp = dp$connected_students |>
    merge(arms_tmp, by = c('round', 'treatment'))

  dp$connected_students = students_tmp[, .(
    year,
    term,
    year_term_str,
    year_term_num,
    round_id,
    arm_id,
    treatment_id,
    treatment_name,
    treatment_wrap = str_wrap(treatment_name, 20L),
    region = as.character(region_bl),
    school_id = school_id_bl,
    school_name = fifelse(school_name_bl == '', NA, school_name_bl),
    facilitator_id_impl = fifelse(facilitator_id_i == '', NA, facilitator_id_i),
    facilitator_name_impl = fifelse(facilitator_i == '', NA, facilitator_i),
    student_id = hh_id, # one student per household
    student_gender = fifelse(
      is.na(stud_sex_bl), 'Unknown', as.character(stud_sex_bl)),
    student_age = stud_age_bl,
    student_standard = stud_std_bl,
    student_level_str_baseline = factor(stud_level_bl, numer_levs),
    student_level_str_endline = factor(stud_level, numer_levs))]

  dp$connected_students[, `:=`(
    student_level_num_baseline = as.integer(student_level_str_baseline),
    student_level_num_endline = as.integer(student_level_str_endline),
    level_beginner_baseline = student_level_str_baseline == 'Beginner',
    level_beginner_endline = student_level_str_endline == 'Beginner',
    level_ace_baseline = student_level_str_baseline == 'Division',
    level_ace_endline = student_level_str_endline == 'Division')]

  dp$connected_students[, `:=`(
    timepoint = 'Baseline to Endline',
    level_progress = student_level_num_endline - student_level_num_baseline,
    level_improved = student_level_num_endline > student_level_num_baseline)]
  setkeyv(dp$connected_students, 'student_id')

  ### connected_assessments
  dp$connected_assessments = melt(
    dp$connected_students[, !'timepoint'],
    measure.vars = patterns(
      '^student_level_str_', '^student_level_num_',
      '^level_beginner_', '^level_ace_'),
    variable.name = 'timepoint',
    value.name = c(
      'student_level_str', 'student_level_num', 'level_beginner', 'level_ace'),
    value.factor = TRUE)

  dp$connected_assessments[
    , timepoint := factor(timepoint, labels = c('Baseline', 'Endline'))]
  setkeyv(dp$connected_assessments, c('student_id', 'timepoint'))

  ### connected_arms
  dp$connected_arms = arms_tmp[, .(
    round_id,
    year,
    term,
    arm_id,
    treatment_id,
    treatment_name,
    treatment_description)]
  setkeyv(dp$connected_arms, 'arm_id')

  ### tarlnum_assessments
  dp$tarlnum_assessments = unique(dp$tarlnum_assessments[
    uid_s != '', .(
      year,
      term = as.integer(str_extract(term, '[0-9]+$')),
      delivery_model = fifelse(
        delivery_type == 'Model School', 'Direct', 'Government'),
      duration_days = fifelse(# inferred from data for school_id 96
        imp_length == '', 30L, as.integer(str_extract(imp_length, '^[0-9]+'))),
      phase,
      region,
      school_name,
      school_id,
      facilitator_id_impl = NA,
      facilitator_name_impl = NA,
      student_id_orig = uid_s,
      student_gender = fcase(
        stu_gender == 'F', 'Female',
        stu_gender == 'M', 'Male',
        default = 'Unknown'),
      student_age = stu_age,
      student_standard = stu_std,
      student_class = fifelse(stu_class == '', 'Default', stu_class),
      timepoint = factor(round, c('Baseline', 'Midline', 'Endline')),
      student_level_str = factor(stu_level, numer_levs))])

  dp$tarlnum_assessments[, `:=`(
    year_term_str = glue('{year} T{term}', .envir = .SD),
    year_term_num = year + (term - 1) / 3,
    student_level_num = as.integer(student_level_str),
    student_id = paste(
      year, term, delivery_model, duration_days, region,
      school_name, school_id, student_id_orig, student_gender,
      student_age, student_standard, student_class, sep = '|'),
    level_beginner = student_level_str == 'Beginner',
    level_ace = student_level_str == 'Division')]
  setkeyv(dp$tarlnum_assessments, c('student_id', 'timepoint'))

  ### tarlnum_students
  dp$tarlnum_students = dcast(
    dp$tarlnum_assessments, formula('... ~ timepoint'),
    value.var = c(
      'student_level_str', 'student_level_num', 'level_beginner', 'level_ace'))
  setnames(dp$tarlnum_students, tolower)
  dp$tarlnum_students[, `:=`(
    timepoint = 'Baseline to Endline',
    level_progress = student_level_num_endline - student_level_num_baseline,
    level_improved = student_level_num_endline > student_level_num_baseline)]
  setkeyv(dp$tarlnum_students, 'student_id')

  ### tarllit_students
  dp$tarllit_students = dp$tarllit_students[, .(
    year = imp_year,
    term = imp_term,
    delivery_model = fifelse(
      imp_type == 'Direct Delivery', 'Direct', 'Government'),
    duration_days = as.integer(str_extract(imp_duration, '^[0-9]+')),
    region,
    school_name,
    school_id,
    facilitator_id_impl = NA,
    facilitator_name_impl = NA,
    student_id_orig = stu_uid,
    student_gender = stu_gender,
    student_age = stu_age,
    student_standard = stu_std,
    student_class = stu_class,
    student_level_str_baseline = factor(stu_level_bsl, liter_levs),
    student_level_str_endline = factor(stu_level_end, liter_levs))]

  dp$tarllit_students[, `:=`(
    year_term_str = glue('{year} T{term}', .envir = .SD),
    year_term_num = year + (term - 1) / 3,
    student_id = paste(
      year, term, delivery_model, duration_days, region,
      school_name, school_id, student_id_orig, student_gender,
      student_age, student_standard, student_class, sep = '|'),
    student_level_num_baseline = as.integer(student_level_str_baseline),
    student_level_num_endline = as.integer(student_level_str_endline),
    level_beginner_baseline = student_level_str_baseline == 'Beginner',
    level_beginner_endline = student_level_str_endline == 'Beginner',
    level_ace_baseline = student_level_str_baseline == 'Story',
    level_ace_endline = student_level_str_endline == 'Story')]

  dp$tarllit_students[, `:=`(
    timepoint = 'Baseline to Endline',
    level_progress = student_level_num_endline - student_level_num_baseline,
    level_improved = student_level_num_endline > student_level_num_baseline)]
  setkeyv(dp$tarllit_students, 'student_id')

  ### tarllit_assessments
  dp$tarllit_assessments = melt(
    dp$tarllit_students[, !'timepoint'],
    measure.vars = patterns(
      '^student_level_str_', '^student_level_num_',
      '^level_beginner_', '^level_ace_'),
    variable.name = 'timepoint',
    value.name = c(
      'student_level_str', 'student_level_num', 'level_beginner', 'level_ace'),
    value.factor = TRUE)

  dp$tarllit_assessments[
    , timepoint := factor(timepoint, labels = c('Baseline', 'Endline'))]
  setkeyv(dp$tarllit_assessments, c('student_id', 'timepoint'))

  ### zones_assessments
  zones_assessments = merge(
    rbindlist(
      data_drive[startsWith(names(data_drive), 'zones_assessments_')],
      fill = TRUE, idcol = 'file_name'),
    data_drive$zones_files, by = 'file_name')

  dp$zones_assessments = zones_assessments[, .(
    year,
    term,
    year_term_str = glue('{year} T{term}', .envir = .SD),
    year_term_num = year + (term - 1) / 3,
    # TODO: remove placeholders for region, school_id, and school_name
    region = rep_len(rep_len(c('Kgatleng', 'South East'), 10L), .N),
    school_id = rep_len(paste0('S0', 0:9), .N),
    school_name = rep_len(paste0('PS 0', 0:9), .N),
    facilitator_id_impl = fac_id,
    facilitator_name_impl = NA,
    student_gender,
    student_age = NA,
    student_standard = NA,
    timepoint = factor(
      round, c('baseline', 'endline'), c('Baseline', 'Endline')),
    know_hiv_least_10to19 = fcase(
      k_hiv_least_10to19 == 'Yes', TRUE, k_hiv_least_10to19 == 'No', FALSE),
    know_hiv_riskiest_older = fcase(
      k_hiv_riskiest_older == 'Yes', TRUE, k_hiv_riskiest_older == 'No', FALSE))]

  dp$zones_assessments[, student_id := paste(
    year, term, student_gender, timepoint, 1:.N, sep = '|')]

  dp[startsWith(names(dp), 'zones_assessments_')] = NULL
  dp$zones_metadata = NULL

  ### nomissing
  dp$connected_students_nomissing = dp$connected_students[
    !is.na(student_level_str_baseline) & !is.na(student_level_str_endline)]
  setkeyv(dp$connected_students_nomissing, 'student_id')

  dp$tarlnum_students_nomissing = dp$tarlnum_students[
    !is.na(student_level_str_baseline) & !is.na(student_level_str_endline)]
  setkeyv(dp$tarlnum_students_nomissing, 'student_id')

  dp$tarllit_students_nomissing = dp$tarllit_students[
    !is.na(student_level_str_baseline) & !is.na(student_level_str_endline)]
  setkeyv(dp$tarllit_students_nomissing, 'student_id')

  dp$connected_assessments_nomissing = dp$connected_assessments[
    timepoint %in% c('Baseline', 'Endline') & !is.na(student_level_str),
    if (.N == 2L) .SD, by = student_id]
  setkeyv(dp$connected_assessments_nomissing, c('student_id', 'timepoint'))

  dp$tarlnum_assessments_nomissing = dp$tarlnum_assessments[
    timepoint %in% c('Baseline', 'Endline') & !is.na(student_level_str),
    if (.N == 2L) .SD, by = student_id]
  dp$tarlnum_assessments_nomissing[
    , timepoint := factor(timepoint, c('Baseline', 'Endline'))]
  setkeyv(dp$tarlnum_assessments_nomissing, c('student_id', 'timepoint'))

  dp$tarllit_assessments_nomissing = dp$tarllit_assessments[
    timepoint %in% c('Baseline', 'Endline') & !is.na(student_level_str),
    if (.N == 2L) .SD, by = student_id]
  setkeyv(dp$tarllit_assessments_nomissing, c('student_id', 'timepoint'))

  ### reach_students
  dp$reach_students = rbind(
    dp$connected_students[, .(
      program = 'ConnectEd', delivery_model = 'Direct',
      year, term, year_term_str, year_term_num,
      region, school_id, school_name,
      facilitator_id_impl, facilitator_name_impl,
      student_id, student_gender, student_age, student_standard)],
    dp$tarlnum_students[, .(
      program = 'TaRL Numeracy', delivery_model,
      year, term, year_term_str, year_term_num,
      region, school_id, school_name,
      facilitator_id_impl, facilitator_name_impl,
      student_id, student_gender, student_age, student_standard)],
    dp$tarllit_students[, .(
      program = 'TaRL Literacy', delivery_model,
      year, term, year_term_str, year_term_num,
      region, school_id, school_name,
      facilitator_id_impl, facilitator_name_impl,
      student_id, student_gender, student_age, student_standard)],
    dp$zones_assessments[timepoint == 'Baseline', .(
      program = 'Zones', delivery_model = 'Direct',
      year, term, year_term_str, year_term_num,
      region, school_id, school_name,
      facilitator_id_impl, facilitator_name_impl,
      student_id, student_gender, student_age, student_standard)],
    fill = TRUE)
  setkeyv(
    dp$reach_students,
    c('program', 'delivery_model', 'region', 'student_gender', 'student_id'))

  dp$`_file_metadata` = NULL
  dp
}
