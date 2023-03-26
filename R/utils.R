library('checkmate')
library('cowplot')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
library('ggplot2')
library('glue')
library('googledrive')
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
      # axis.title.y = element_text(size = 18),
      # plot.title = element_text(size = 18),
      # legend.title = element_text(size = 18),
      # strip.text = element_text(size = 14),
      axis.text = element_text(color = 'black'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')))

# set ggplot styling to match CSS for shiny app
thematic::thematic_shiny()

label_percent_func = scales::label_percent(scale = 1, accuracy = 1)

anno_base = list(
  font = list(size = 18), showarrow = FALSE, align = 'left',
  xref = 'paper', yref = 'paper', xanchor = 'left', yanchor = 'bottom')

########################################

# load parameters
params = yaml::read_yaml('params.yaml')

# authorize googledrive to access files
if (Sys.getenv('GOOGLE_TOKEN') == '') { # locally
  drive_auth(email = params$email)
} else { # GitHub Actions
  drive_auth(path = Sys.getenv('GOOGLE_TOKEN'))
}

########################################

#' Get metadata for files in a Google Drive folder
#'
#' This is a thin wrapper around [googledrive::drive_ls()].
#'
#' @param folder_url String of folder url.
#'
#' @return `data.table` containing one row per file, excluding files that start
#'   with "_".
get_file_metadata = function(folder_url) {
  assert_string(folder_url)
  files = setDT(drive_ls(folder_url))
  files = files[!startsWith(name, '_')]
  setorder(files, name)
  files[, modified_time := sapply(files$drive_resource, \(f) f$modifiedTime)]
  files[]
}

########################################

get_picker_options = function(...) {
  pickerOptions(actionsBox = TRUE, selectedTextFormat = 'static', ...)
}

#' Get choices for UI input
#'
#' This function finds unique rows of a `data.table` to use as choices for UI
#' input, such as [shiny::radioButtons()] or [shiny::checkboxGroupInput()].
#'
#' @param d `data.table` that has columns corresponding to `name_col` and
#'   `val_col`.
#' @param name_col String indicating column to use for names.
#' @param val_col String indicating column to use for values.
#'
#' @return Named list, where names will be displayed to the user and values will
#'   be used within the app.
get_choices = function(d, name_col = 'label', val_col = 'round_id') {
  assert_data_table(d)
  assert_subset(c(name_col, val_col), colnames(d))
  d_unique = unique(d[, c(..name_col, ..val_col)])
  setorderv(d_unique, val_col)
  choices = as.list(d_unique[[val_col]])
  names(choices) = paste('Round', d_unique[[name_col]])
  choices
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
get_round_text = function(data_proc) {
  data_now = data_proc$data_wide[, .N, keyby = treatment_id]

  overview_text = p(
    strong('Purpose: '), data_proc$rounds$purpose, br(),
    strong('Conclusion: '), data_proc$rounds$conclusion)

  treatments_now = merge(
    data_proc$treatments, data_proc$arms,
    by = c('treatment_id', 'treatment_name')) |>
    merge(data_now, by = 'treatment_id')
  setorder(treatments_now, arm_id)
  n_students = sum(treatments_now$N)

  treatment_text = lapply(seq_len(nrow(treatments_now)), \(i) {
    r = treatments_now[i]
    list(
      strong(r$treatment_name), '(',
      em(glue('{r$N} students'), .noWS = 'outside'),
      paste('):', r$treatment_description), br())
  })

  round_text = tagList(
    br(), overview_text, h5('Treatments'),
    unlist(treatment_text, recursive = FALSE),
    em(glue('{n_students} students in total ',
            '(ascertained at baseline and endline).')), br())
}

########################################

get_fills = function(type, palette = 'Blues') {
  switch(
    type, total = 'black', beginner = c('#fb9a99', '#e31a1c'),
    ace = c('#a6cee3', '#1f78b4'), improved = '#33a02c',
    full = rev(c(
      '#EF5A5B', RColorBrewer::brewer.pal(n = 5L, name = palette)[-1L])))
}

get_tooltips = function(
    n, pct = NULL, pre = NULL, suf = NULL, entity = 'students') {
  n_label = scales::label_comma()(n)
  ans = glue('{n_label} {entity}')
  if (!is.null(pct)) {
    pct_label = label_percent_func(pct)
    ans = glue('{ans} ({pct_label})')
  }
  if (!is.null(pre)) ans = glue('{pre}\n{ans}')
  if (!is.null(suf)) ans = glue('{ans}\n{suf}')
  ans
}

get_barplot_data = function(data, x_col, col, by_treatment, percent) {
  by1 = c(x_col, col)
  if (by_treatment) by1 = c('treatment_id', 'treatment_name', by1)
  by2 = by1[-length(by1)]

  data_now = data[, .N, keyby = by1]
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
#' @param by_treatment logical indicating whether to facet by `treatment_name`.
#' @param percent logical indicating whether to plot percentages or counts.
#' @param bar_width numeric used by [ggplot2::geom_col()] for width of bars.
#' @param text_size numeric used by [ggplot2::geom_text()] for text above bars.
#'
#' @return `ggplot` object.
get_barplot_summary = function(
    data, col, fills, title = waiver(), x_col = 'timepoint',
    by_treatment = FALSE, percent = TRUE, bar_width = 0.7, text_size = 5,
    y_lims = NULL) {

  stopifnot(is_logical(by_treatment))
  stopifnot(is_logical(percent))

  data_now = get_barplot_data(data, x_col, col, by_treatment, percent)
  data_now = data_now[z == TRUE, env = list(z = col)]
  y_lab = if (percent) 'Share of students (%)' else 'Number of students'
  y_scale = if (percent) label_percent_func else waiver()

  p = ggplot(data_now, aes(x = .data[[x_col]], y = quant)) +
    geom_col(aes(fill = .data[[x_col]], text = label), width = bar_width) +
    labs(y = y_lab, title = title) +
    scale_y_continuous(labels = y_scale, limits = y_lims) +
    scale_fill_manual(values = fills) +
    theme(axis.title.x = element_blank(), legend.position = 'none')

  if (by_treatment) p = p + facet_wrap(vars(treatment_name))
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
#' @param by_treatment logical indicating whether to facet by `treatment_name`.
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
  y_lab = if (percent) 'Share of students (%)' else 'Number of students'
  y_scale = if (percent) label_percent_func else waiver()

  p = ggplot(data_now, aes(x = .data[[x_col]], y = quant)) +
    geom_col(
      aes(fill = forcats::fct_rev(.data[[col]]), text = label),
      width = bar_width) +
    labs(y = y_lab, fill = NULL, title = title) +
    scale_y_continuous(labels = y_scale) +
    scale_fill_manual(values = fills) +
    theme(axis.title.x = element_blank())
  if (by_treatment) p = p + facet_wrap(vars(treatment_name))
  p
}

get_trend_plot = function(
    data, x_col, y_col, text_col, fill = NULL, shape = NULL, y_lims = NULL,
    percent = TRUE, sign = 1, ...) {

  p = ggplot(
    data, aes(
      x = .data[[x_col]], y = sign * .data[[y_col]], text = .data[[text_col]]))

  p = if (length(fill) == 1L) {
    p + geom_point(fill = fill, shape = shape, ...)
  } else {
    p + geom_point(aes(fill = timepoint, shape = timepoint), ...) +
      scale_fill_manual(values = fill) +
      scale_shape_manual(values = shape)
  }

  if (percent) {
    y_lab = 'Share of students (%)'
    y_labs = label_percent_func
  } else {
    y_lab = 'Number of students'
    y_labs = waiver()
  }

  p = p +
    labs(x = 'Year', y = y_lab, fill = NULL, shape = NULL) +
    scale_x_continuous(
      breaks = scales::breaks_extended(), minor_breaks = NULL) +
    scale_y_continuous(labels = y_labs, limits = y_lims)
  p
}

########################################

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
  days_per_week = 5 # assumes duration is a column in data_wide

  a3 = data_wide[, .(
    mean_improvement = mean(student_level_diff, na.rm = TRUE),
    sd_improvement = sd(student_level_diff, na.rm = TRUE),
    mean_improvement_per_week =
      mean(student_level_diff / duration, na.rm = TRUE) * days_per_week,
    sd_improvement_per_week =
      sd(student_level_diff / duration, na.rm = TRUE) * days_per_week,
    n_improved = sum(student_level_diff > 0, na.rm = TRUE),
    pct_improved = 100 * sum(student_level_diff > 0, na.rm = TRUE) / .N),
    keyby = c(by_cols, time_col)]

  a2 = merge(a2, a3, by = by_cols)
  list(long = a1, wide = a2)
}

########################################

# https://stackoverflow.com/questions/61122868/long-facet-wrap-labels-in-ggplotly-plotly-overlap-facets-strip-background
# facet_strip_bigger = function(p, size = 50) {
#   n_facets = c(1:length(p[['x']][['layout']][['shapes']]))
#   for (i in n_facets){
#     if (n_facets[i] %% 2 == 0) {
#       p[['x']][['layout']][['shapes']][[i]][['y0']] = size
#       p[['x']][['layout']][['shapes']][[i]][['y1']] = 0
#     }
#   }
#   p
# }
