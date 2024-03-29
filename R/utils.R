library('checkmate')
library('DBI')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
library('forcats')
library('ggplot2')
library('glue')
library('googledrive')
library('googlesheets4')
library('plotly')
library('shiny')
library('shinyWidgets')
library('stringr')
library('DT') # load after shiny for dataTableOutput and renderDataTable

# set global ggplot theme
theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 15),
      axis.text = element_text(color = 'black'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')))

# set ggplot styling to match CSS for shiny app
thematic::thematic_shiny()

label_percent_func = scales::label_percent(scale = 1, accuracy = 1)

anno_base = list(
  font = list(size = 18), showarrow = FALSE, align = 'left',
  xref = 'paper', yref = 'paper', xanchor = 'left', yanchor = 'bottom')

# load parameters
params = yaml::read_yaml('params.yaml')

token_path = if (Sys.getenv('GOOGLE_TOKEN') == '') {
  file.path('secrets', params$token)
} else {
  Sys.getenv('GOOGLE_TOKEN')
}
drive_auth(path = token_path)
gs4_auth(token = drive_token())

########################################

get_issue_button = function(inputId = 'issue_button') {
  url = 'https://github.com/youth-impact/dashboard/issues/new/choose'
  onclick = glue("window.open('{url}', '_blank')")
  actionBttn(
    inputId = inputId, label = 'Report issue', onclick = onclick,
    style = 'fill', size = 'sm')
}

get_data_filtered = function(x, filt = data.table(), filt_by_student = NULL) {
  y = lapply(x, \(d) {
    d_new = if (any(colnames(filt) %in% colnames(d))) {
      by_cols = intersect(colnames(filt), colnames(d))
      merge(d, filt, by = by_cols, sort = FALSE, allow.cartesian = TRUE)
    } else {
      copy(d)
    }
    if (!is.null(filt_by_student) &&
        all(colnames(filt_by_student) %in% colnames(d_new))) {
      ok = d_new[filt_by_student, .(student_id), on = .NATURAL, nomatch = NULL]
      d_new = d_new[unique(ok), on = .NATURAL, nomatch = NULL]
    } else {
      d_new
    }
  })
  names(y) = names(x)
  y
}

get_picker_options = function(noneSelectedText, ...) {
  pickerOptions(
    actionsBox = TRUE, selectedTextFormat = 'static',
    noneSelectedText = noneSelectedText, ...)
}

get_fills = function(type, palette = 'Blues') {
  switch(
    type,
    total = 'black',
    beginner = c('#fb9a99', '#e31a1c'),
    ace = c('#a6cee3', '#1f78b4'),
    improved = '#b2df8a', #'#33a02c',
    full = rev(c(
      '#EF5A5B', RColorBrewer::brewer.pal(n = 5L, name = palette)[-1L])))
}

get_title = function(metric, type = 'numeracy') {
  if (type == 'numeracy') {
    switch(
      metric,
      beginner = 'Innumeracy: Beginner Level',
      ace  = 'Numeracy: Division Level',
      improved = 'Improved a Level',
      beginner_diff = 'Decrease in Innumeracy',
      ace_diff = 'Increase in Numeracy',
      progress = 'Progress toward Numeracy',
      full = 'All Levels')
  } else {
    switch(
      metric,
      beginner = 'Illiteracy: Beginner Level',
      ace  = 'Literacy: Story Level',
      improved = 'Improved a Level',
      beginner_diff = 'Decrease in Illiteracy',
      ace_diff = 'Increase in Literacy',
      progress = 'Progress toward Literacy',
      full = 'All Levels')
  }
}

get_y_title = function(percent = TRUE, points = FALSE) {
  y_title = if (percent) {
    if (points) 'Share of Students\n(%-points)' else 'Share of Students (%)'
  } else {
    'Number of Students'
  }
}

get_overview_banner = function(students, program = 'connected') {
  if (program == 'zones') {
    n_unique = students[timepoint == 'Baseline', .(
      students = uniqueN(.SD, by = 'student_id'),
      facilitators = uniqueN(.SD, by = 'facilitator_id_impl'),
      schools = uniqueN(.SD, by = c('school_id', 'school_name')),
      regions = uniqueN(.SD, by = 'region'))]

    metrics = students[student_gender == 'Female', .(
      pct_younger = 100 * sum(know_hiv_least_10to19, na.rm = TRUE) /
        sum(!is.na(know_hiv_least_10to19)),
      pct_older = 100 * sum(know_hiv_riskiest_older, na.rm = TRUE) /
        sum(!is.na(know_hiv_riskiest_older))),
      keyby = timepoint][, lapply(.SD, diff), .SDcols = !'timepoint']

  } else {
    n_unique = students[, .(
      students = uniqueN(.SD, by = 'student_id'),
      facilitators = uniqueN(.SD, by = 'facilitator_id_impl'),
      schools = uniqueN(.SD, by = c('school_id', 'school_name')),
      regions = uniqueN(.SD, by = 'region'))]

    metrics = students[, .(
      pct_beginner =
        100 * sum(level_beginner_baseline - level_beginner_endline) / .N,
      pct_ace = 100 * sum(level_ace_endline - level_ace_baseline) / .N,
      pct_improved = 100 * sum(level_improved) / .N)]
  }

  n_unique = n_unique[, lapply(.SD, scales::label_comma())]
  metrics = metrics[, lapply(.SD, scales::label_number(accuracy = 1))]

  align = 'center'
  sty_n = 'font-size:26px;'
  sty_unit = 'font-size:20px;'
  icls = 'fa-2x'
  sp = HTML('&nbsp;')
  wds = if (program %in% c('connected', 'zones')) {
    list(students = 3, facilitators = 3, schools = 3, regions = 3)
  } else {
    list(students = 5, schools = 4, regions = 3, facilitators = 1)
  }

  ui_cols = list()
  ui_cols$students = column(
    width = wds$students, align = align,
    strong(n_unique$students, style = sty_n),
    a(tags$sup('‡'), style = sty_unit),
    icon('child-reaching', icls), br(), p('Students', style = sty_unit))

  ui_cols$facilitators = column(
    width = wds$facilitators, align = align,
    strong(n_unique$facilitators, style = sty_n), sp,
    icon('person-chalkboard', icls), br(), p('Facilitators', style = sty_unit))

  ui_cols$schools = column(
    width = wds$schools, align = align,
    strong(n_unique$schools, style = sty_n), sp,
    icon('school', icls), br(), p('Schools', style = sty_unit))

  ui_cols$regions = column(
    width = wds$regions, align = align,
    strong(n_unique$regions, style = sty_n), sp,
    icon('map-location', icls), br(), p('Regions', style = sty_unit))

  ui_row_counts = if (program %in% c('connected', 'zones')) {
    fluidRow(
      ui_cols$students, ui_cols$facilitators, ui_cols$schools, ui_cols$regions)
  } else {
    fluidRow(ui_cols$students, ui_cols$schools, ui_cols$regions)
  }

  ui_row_kpis = if (program == 'zones') {
    title_younger = paste(
      'Increase in females who know 10-to-19-year-olds',
      'have lowest prevalence of HIV')
    title_older = paste(
      'Increase in females who know older partners',
      'have highest risk of transmitting HIV')

    fluidRow(
      column(
        width = 6, style = 'background-color:#a6cee3;',
        p(strong(metrics$pct_younger, style = sty_n),
          a(' %-points', br(), title_younger, style = sty_unit))
      ),
      column(
        width = 6, style = 'background-color:#fb9a99;',
        p(strong(metrics$pct_older, style = sty_n),
          a(' %-points', br(), title_older, style = sty_unit))
      )
    )

  } else {
    title_ace = paste(
      'Increase in', if (program == 'tarllit') 'Literacy' else 'Numeracy')
    title_beginner = paste(
      'Decrease in', if (program == 'tarllit') 'Illiteracy' else 'Innumeracy')

    fluidRow(
      column(
        width = 5, align = align, style = 'background-color:#a6cee3;',
        p(strong(metrics$pct_ace, style = sty_n),
          a(' %-points', br(), title_ace, style = sty_unit))
      ),
      column(
        width = 4, align = align, style = 'background-color:#fb9a99;',
        p(strong(metrics$pct_beginner, style = sty_n),
          a(' %-points', br(), title_beginner, style = sty_unit))
      ),
      column(
        width = 3, align = align, style = 'background-color:#b2df8a;',
        p(strong(metrics$pct_improved, style = sty_n),
          a(' %', br(), get_title('improved'), style = sty_unit))
      )
    )
  }

  wellPanel(ui_row_counts, ui_row_kpis)
}

#' Get narrative text describing a round of ConnectEd
#'
#' @param rounds `data.table` containing metadata for rounds.
#' @param arms `data.table` containing metadata for arms.
#' @param treatments `data.table` containing metadata for treatments.
#' @param data `data.table` containing individual-level data.
#' @param round_id_now single value indicating current round.
#'
#' @return HTML tags.
get_round_text = function(data_filt) {
  rd_now = data_filt$connected_rounds

  overview_text = p(
    br(), strong('Dates:'), # using en-dash
    paste(rd_now$baseline_start_month, rd_now$endline_end_month, sep = '–'),
    ' ', rd_now$year, br(),
    strong('Theme: '), rd_now$theme, br(),
    strong('Question: '), rd_now$question, br(),
    strong('Conclusion: '), rd_now$conclusion)

  data_now = merge(
    data_filt$connected_students_nomissing[, .N, keyby = arm_id],
    data_filt$connected_arms, by = 'arm_id')
  n_students = sum(data_now$N)

  treatment_text = lapply(seq_len(nrow(data_now)), \(i) {
    list(strong(data_now[i]$treatment_name), '(',
         em(glue('{data_now[i]$N} students'), .noWS = 'outside'),
         paste('):', data_now[i]$treatment_description), br())
  })

  round_text = tagList(
    overview_text, h5('Treatments'), unlist(treatment_text, recursive = FALSE),
    h5('Results'))
}

get_tooltips = function(
    n = NULL, pct = NULL, pre = NULL, suf = NULL, entity = 'students') {
  if (is.null(n)) {
    ans = label_percent_func(pct)
  } else {
    n_label = scales::label_comma()(n)
    ans = glue('{n_label} {entity}')
    if (!is.null(pct)) {
      pct_label = label_percent_func(pct)
      ans = glue('{ans} ({pct_label})')
    }
  }
  if (!is.null(pre)) ans = glue('{pre}\n{ans}')
  if (!is.null(suf)) ans = glue('{ans}\n{suf}')
  ans
}

get_barplot_data = function(data, x_col, col, by_treatment, percent) {
  by1 = c(x_col, col)
  if (by_treatment) by1 = c('treatment_id', 'treatment_wrap', by1)
  by2 = by1[-length(by1)]

  data_now = data[, .N, keyby = by1]
  treatment_levels = unique(data_now$treatment_wrap) # trust keyby treatment_id
  if (by_treatment) {
    data_now[, treatment_wrap := factor(treatment_wrap, treatment_levels)]
  }
  data_now[, n_students := N, by = by2]
  data_now[, pct_students := 100 * N / sum(n_students), by = by2]
  quant_col = if (percent) 'pct_students' else 'n_students'
  set(data_now, j = 'quant', value = data_now[[quant_col]])

  data_now[, label := get_tooltips(n_students, pct_students)]
}

#' Get summary barplot for outcomes data
#'
#' This function creates a barplot, optionally faceted, to summarize counts or
#' percentages based on individual-level data.
#'
#' @param data `data.table` of individual-level data, e.g., from ConnectEd.
#' @param col string indicating column in `data` that contains logical values to
#'   use for calculating counts or percentages.
#' @param fills character vector indicating fill colors for bars.
#' @param title string indicating title of plot.
#' @param x_col string indicating column in `data` to use for x-axis.
#' @param by_treatment logical indicating whether to facet by treatment.
#' @param percent logical indicating whether to plot percentages or counts.
#' @param bar_width numeric used by [ggplot2::geom_col()] for width of bars.
#' @param text_size numeric used by [ggplot2::geom_text()] for text above bars.
#'
#' @return `ggplot` object.
get_barplot_summary = function(
    data, col, fills, title = waiver(), x_col = 'timepoint',
    by_treatment = FALSE, percent = TRUE, bar_width = 0.7, text_size = 5,
    y_lims = NULL, ...) {

  stopifnot(is_logical(by_treatment))
  stopifnot(is_logical(percent))

  data_now = get_barplot_data(data, x_col, col, by_treatment, percent)
  data_now = data_now[z == TRUE, env = list(z = col)]
  y_scale = if (percent) label_percent_func else waiver()

  p = ggplot(data_now, aes(x = .data[[x_col]], y = quant)) +
    geom_col(aes(fill = .data[[x_col]], text = label), width = bar_width) +
    labs(y = get_y_title(percent), title = title) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(labels = y_scale, limits = y_lims) +
    scale_fill_manual(drop = FALSE, values = fills) +
    theme(axis.title.x = element_blank(), legend.position = 'none')

  if (by_treatment) p = p + facet_wrap(vars(treatment_wrap), ...)
  p
}

#' Get detailed barplot for outcomes data
#'
#' This function creates a stacked barplot, optionally faceted, of counts or
#' percentages based on individual-level data.
#'
#' @param data `data.table` of individual-level data, e.g., from ConnectEd.
#' @param col string indicating column in `data` to use for calculating counts
#'   or percentages.
#' @param x_col string indicating column in `data` to use for x-axis.
#' @param by_treatment logical indicating whether to facet by treatment.
#' @param percent logical indicating whether to plot percentages or counts.
#' @param bar_width numeric used by [ggplot2::geom_col()] for width of bars.
#'
#' @return `ggplot` object.
get_barplot_detailed = function(
    data, col, fills, title = waiver(), x_col = 'timepoint',
    by_treatment = FALSE, percent = TRUE, bar_width = 0.7) {

  stopifnot(is_logical(by_treatment))
  stopifnot(is_logical(percent))

  data_now = get_barplot_data(data, x_col, col, by_treatment, percent)
  y_scale = if (percent) label_percent_func else waiver()

  p = ggplot(data_now, aes(x = .data[[x_col]], y = quant)) +
    geom_col(
      aes(fill = forcats::fct_rev(.data[[col]]), text = label),
      width = bar_width) +
    labs(y = get_y_title(percent), fill = NULL, title = title) +
    scale_y_continuous(labels = y_scale) +
    scale_fill_manual(values = fills) +
    theme(axis.title.x = element_blank())
  if (by_treatment) p = p + facet_wrap(vars(treatment_wrap))
  p
}

# https://stackoverflow.com/questions/61122868/
facet_strip_bigger = function(p, size = 45) {
  n_facets = c(1:length(p[['x']][['layout']][['shapes']]))
  for (i in n_facets) {
    if (n_facets[i] %% 2 == 0) {
      p[['x']][['layout']][['shapes']][[i]][['y0']] = size
      p[['x']][['layout']][['shapes']][[i]][['y1']] = 0
    }
  }
  p
}

get_plot_trends_connected = function(students, rounds) {
  metrics = students[, .(
    n_beginner = sum(level_beginner_baseline - level_beginner_endline),
    pct_beginner =
      100 * sum(level_beginner_baseline - level_beginner_endline) / .N,
    n_ace = sum(level_ace_endline - level_ace_baseline),
    pct_ace = 100 * sum(level_ace_endline - level_ace_baseline) / .N,
    n_improved = sum(level_improved),
    pct_improved = 100 * sum(level_improved) / .N),
    by = round_id] |>
    merge(rounds[, .(round_id, round_name)], by = 'round_id')

  metrics[, tt_beginner := get_tooltips(n_beginner, pct_beginner)]
  metrics[, tt_ace := get_tooltips(n_ace, pct_ace)]
  metrics[, tt_improved := get_tooltips(n_improved, pct_improved)]
  metrics[, round_short := str_extract(round_name, '[0-9]+$')]

  marj = list(t = 30)
  anno = c(list(x = 0, y = 1), anno_base)

  fig = ggplot(metrics, aes(x = round_id, y = pct_ace, text = tt_ace)) +
    geom_col(fill = get_fills('ace')[1L]) +
    labs(y = get_y_title(points = TRUE)) +
    scale_x_discrete(labels = metrics$round_short) +
    scale_y_continuous(labels = label_percent_func, limits = c(0, NA)) +
    theme(axis.title.x = element_blank())
  fig_ace = ggplotly(fig, tooltip = 'text') |>
    layout(annotations = c(text = get_title('ace_diff'), anno), margin = marj)

  fig = ggplot(
    metrics, aes(x = round_id, y = pct_beginner, text = tt_beginner)) +
    geom_col(fill = get_fills('beginner')[1L]) +
    labs(y = get_y_title(points = TRUE)) +
    scale_x_discrete(labels = metrics$round_short) +
    scale_y_continuous(labels = label_percent_func, limits = c(0, NA)) +
    theme(axis.title.x = element_blank())
  fig_beginner = ggplotly(fig, tooltip = 'text') |>
    layout(
      annotations = c(text = get_title('beginner_diff'), anno), margin = marj)

  fig = ggplot(
    metrics, aes(x = round_id, y = pct_improved, text = tt_improved)) +
    geom_col(fill = get_fills('improved')[1L]) +
    labs(x = 'Round', y = get_y_title()) +
    scale_x_discrete(labels = metrics$round_short) +
    scale_y_continuous(labels = label_percent_func, limits = c(0, 100))
  fig_improved = ggplotly(fig, tooltip = 'text') |>
    layout(annotations = c(text = get_title('improved'), anno), margin = marj)

  subplot(
    fig_ace, fig_beginner, fig_improved, nrows = 3L,
    heights = c(0.33, 0.35, 0.32), margin = 0.04, titleX = TRUE, titleY = TRUE)
}

get_plot_trends_tarl = function(students, by_year, type = 'numeracy') {
  by_cols = if (by_year) 'year' else c('year_term_num', 'year_term_str')
  x_col = if (by_year) 'year' else 'year_term_num'
  pre_col = if (by_year) 'year' else 'year_term_str'

  metrics = students[, .(
    n_beginner = sum(level_beginner_baseline - level_beginner_endline),
    pct_beginner =
      100 * sum(level_beginner_baseline - level_beginner_endline) / .N,
    n_ace = sum(level_ace_endline - level_ace_baseline),
    pct_ace = 100 * sum(level_ace_endline - level_ace_baseline) / .N,
    n_improved = sum(level_improved),
    pct_improved = 100 * sum(level_improved) / .N),
    by = by_cols]

  metrics[, tt_beginner := get_tooltips(
    n_beginner, pct_beginner, pre = pre_col), env = list(pre_col = pre_col)]
  metrics[, tt_ace := get_tooltips(
    n_ace, pct_ace, pre = pre_col), env = list(pre_col = pre_col)]
  metrics[, tt_improved := get_tooltips(
    n_improved, pct_improved, pre = pre_col), env = list(pre_col = pre_col)]

  breaks = sort(unique(round(metrics[[x_col]])))
  marj = list(t = 30)
  anno = c(list(x = 0, y = 1), anno_base)

  fig = ggplot(metrics, aes(x = .data[[x_col]], y = pct_ace, text = tt_ace)) +
    geom_col(fill = get_fills('ace')[1L]) +
    labs(y = get_y_title(points = TRUE)) +
    scale_x_continuous(breaks = breaks, minor_breaks = NULL) +
    scale_y_continuous(labels = label_percent_func, limits = c(0, NA)) +
    theme(axis.title.x = element_blank())
  fig_ace = ggplotly(fig, tooltip = 'text') |>
    layout(
      annotations = c(text = get_title('ace_diff', type), anno), margin = marj)

  fig = ggplot(
    metrics, aes(x = .data[[x_col]], y = pct_beginner, text = tt_beginner)) +
    geom_col(fill = get_fills('beginner')[1L]) +
    labs(y = get_y_title(points = TRUE)) +
    scale_x_continuous(breaks = breaks, minor_breaks = NULL) +
    scale_y_continuous(labels = label_percent_func, limits = c(0, NA)) +
    theme(axis.title.x = element_blank())
  fig_beginner = ggplotly(fig, tooltip = 'text') |>
    layout(
      annotations = c(text = get_title('beginner_diff', type), anno),
      margin = marj)

  fig = ggplot(
    metrics, aes(x = .data[[x_col]], y = pct_improved, text = tt_improved)) +
    geom_col(fill = get_fills('improved')[1L]) +
    labs(x = 'Year', y = get_y_title()) +
    scale_x_continuous(breaks = breaks, minor_breaks = NULL) +
    scale_y_continuous(labels = label_percent_func, limits = c(0, 100))
  fig_improved = ggplotly(fig, tooltip = 'text') |>
    layout(
      annotations = c(text = get_title('improved', type), anno), margin = marj)

  subplot(
    fig_ace, fig_beginner, fig_improved, nrows = 3L,
    heights = c(0.32, 0.36, 0.32), margin = 0.045, titleX = TRUE, titleY = TRUE)
}

get_plot_kpis = function(data_long, data_wide, type = 'numeracy') {
  yaxis = list(title = get_y_title(), titlefont = list(size = 20))

  fig = get_barplot_summary(
    data_long, col = 'level_ace', fills = get_fills('ace'), by_treatment = TRUE)
  fig_ace = ggplotly(fig, tooltip = 'text') |>
    layout(yaxis = yaxis)

  fig = get_barplot_summary(
    data_long, col = 'level_beginner', fills = get_fills('beginner'),
    by_treatment = TRUE)
  fig_beginner = ggplotly(fig, tooltip = 'text') |>
    layout(yaxis = yaxis)

  fig = get_barplot_summary(
    data_wide, col = 'level_improved', fills = get_fills('improved'),
    y_lims = c(0, 100), bar_width = 0.5, by_treatment = TRUE)
  fig_improved = ggplotly(fig, tooltip = 'text') |>
    layout(yaxis = yaxis)

  fig_ace = facet_strip_bigger(fig_ace)
  fig_beginner = facet_strip_bigger(fig_beginner)
  fig_improved = facet_strip_bigger(fig_improved)

  y = c(1.055, 0.68, 0.3)
  heights = c(0.31, 0.38, 0.31)
  marj_subplot = 0.065
  marj_layout = list(t = 65)

  annos = list(
    list(x = 0, y = y[1L], text = get_title('ace', type)),
    list(x = 0, y = y[2L], text = get_title('beginner', type)),
    list(x = 0, y = y[3L], text = get_title('improved', type)))
  annos = lapply(annos, \(z) c(z, anno_base))

  subplot(
    fig_ace, fig_beginner, fig_improved, nrows = 3L,
    heights = heights, margin = marj_subplot, titleY = TRUE) |>
    layout(annotations = annos, margin = marj_layout)
}

get_metrics = function(data_long, data_wide, by_cols, time_col = 'timepoint') {
  # assuming timepoints are Baseline and Endline and they're properly ordered

  # metrics per timepoint
  metric_cols = c('level_beginner', 'level_ace')
  a1 = data_long[
    , c(.(n_total = .N), lapply(.SD, \(x) sum(x, na.rm = TRUE))),
    keyby = c(by_cols, time_col), .SDcols = metric_cols]

  n_cols = gsub('^level', 'n', metric_cols)
  pct_cols = gsub('^level', 'pct', metric_cols)
  setnames(a1, metric_cols, n_cols)

  for (i in seq_len(length(n_cols))) {
    set(a1, j = pct_cols[i], value = 100 * a1[[n_cols[i]]] / a1$n_total)
  }

  a2 = a1[, c(
    n_total = n_total[1L], lapply(.SD, diff)),
    keyby = by_cols, .SDcols = c(n_cols, pct_cols)]
  setnames(a2, c(n_cols, pct_cols), paste0(c(n_cols, pct_cols), '_diff'))

  # metrics between timepoints
  days_per_week = 5 # assumes duration_days is a column in data_wide

  a3 = data_wide[, .(
    mean_progress = mean(level_progress, na.rm = TRUE),
    sd_progress = sd(level_progress, na.rm = TRUE),
    mean_progress_per_week =
      mean(level_progress / duration_days, na.rm = TRUE) * days_per_week,
    sd_progress_per_week =
      sd(level_progress / duration_days, na.rm = TRUE) * days_per_week,
    n_improved = sum(level_progress > 0, na.rm = TRUE),
    pct_improved = 100 * sum(level_progress > 0, na.rm = TRUE) / .N),
    keyby = c(by_cols, time_col)]

  a2 = merge(a2, a3, by = by_cols)
  list(long = a1, wide = a2)
}

get_metrics_zones = function(data, q_cols, by_cols) {
  by_cols_2 = c(by_cols, 'student_gender', 'timepoint')

  metrics = data[
    , lapply(.SD, \(x) 100 * sum(x, na.rm = TRUE) / sum(!is.na(x))),
    keyby = by_cols_2, .SDcols = q_cols]

  form = formula(glue(
    '{y} ~ student_gender + timepoint', y = paste(by_cols, collapse = '+')))
  metrics = dcast(metrics, form, value.var = q_cols)
  setnames(metrics, tolower)

  counts = data[, .(
    # n_terms = uniqueN(year_term_str),
    n_females = sum(timepoint == 'Baseline' & student_gender == 'Female'),
    n_males = sum(timepoint == 'Baseline' & student_gender == 'Male')),
    keyby = by_cols]
  metrics = merge(metrics, counts, by = by_cols)

  for (q in q_cols) {
    for (g in c('female', 'male')) {
      col_pre = glue('{q}_{g}_')
      cols_now = paste0(col_pre, c('baseline', 'endline'))
      if (all(is.na(metrics[, ..cols_now]))) {
        metrics[, (cols_now) := NULL]
      } else {
        set(metrics, j = paste0(col_pre, 'diff'),
            value = metrics[[cols_now[2L]]] - metrics[[cols_now[1L]]])
      }
    }
  }

  cols_diff = colnames(metrics)[endsWith(colnames(metrics), '_diff')]
  setorderv(metrics, cols_diff, order = -1L)
  setcolorder(metrics, cols_diff, after = length(by_cols))
}
