library('checkmate')
library('cowplot')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
library('ggplot2')
library('glue')
library('googledrive')
library('shiny')
library('stringr')

# set global ggplot theme
theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 20),
      axis.title.y = element_text(size = 18),
      plot.title = element_text(size = 18),
      axis.text = element_text(color = 'black'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')))

# set ggplot styling to match CSS for shiny app
thematic::thematic_shiny()

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

#' Get clean ConnectEd data
#'
#' This function largely renames and reformats selected columns.
#'
#' @param data_old `data.table` of ConnectEd data derived from the Stata file.
#' @param inc_miss Character vector of time-points for which to exclude missing
#'   data.
#'
#' @return `data.table` of clean ConnectEd data.
get_clean_connected_data = function(data_old, keep_missing) {
  assert_data_table(data_old)
  data = data_old[, .(
    round = as.integer(round),
    treatment = treatment,
    facilitator_name = as.character(facilitator_i),
    student_id = sprintf('P%08d', seq_len(.N)),
    student_level_baseline = as.integer(stud_level_bl),
    student_level_endline = as.integer(stud_level),
    student_sex_baseline = as.character(forcats::as_factor(stud_sex_bl)),
    region_baseline = forcats::as_factor(region_bl)
  )]
  data[round == 5L & treatment == '', treatment := 'Caregiver Choice']

  if (!('baseline' %in% keep_missing)) {
    data = data[!is.na(student_level_baseline)]
  }
  if (!('endline' %in% keep_missing)) {
    data = data[!is.na(student_level_endline)]
  }
  data[]
}

########################################

#' Get choices for UI input
#'
#' This function finds unique rows of a `data.table` to use as choices for UI
#' input, such as [shiny::radioButtons()] or [shiny::checkboxGroupInput()].
#'
#' @param data `data.table` that has columns corresponding to `name_col` and
#'   `val_col`.
#' @param name_col String indicating column to use for names.
#' @param val_col String indicating column to use for values.
#'
#' @return Named list, where names will be displayed to the user and values will
#'   be used within the app.
get_choices = function(data, name_col = 'round_name', val_col = 'round_id') {
  assert_data_table(data)
  assert_subset(c(name_col, val_col), colnames(data))
  data_unique = unique(data[, c(..name_col, ..val_col)])
  setorderv(data_unique, val_col)
  choices = as.list(data_unique[[val_col]])
  names(choices) = data_unique[[name_col]]
  choices
}

#' Get header for a round of ConnectEd
#'
#' @param rounds `data.table` containing metadata for rounds.
#' @param round_id_now single value indicating current round.
#'
#' @return HTML tags.
get_round_header = function(rounds, round_id_now) {
  round_now = rounds[round_id == round_id_now]
  round_header = h4(paste('Round', round_now$round_name))
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
get_round_text = function(rounds, arms, treatments, data, round_id_now) {
  round_now = rounds[round_id == round_id_now]
  data_now = data[round_id == round_id_now, .N, keyby = treatment_id]

  overview_text = p(
    strong('Purpose: '), round_now$round_purpose, br(),
    strong('Conclusion: '), round_now$round_conclusion)

  treatments_now = merge(
    treatments, arms[round_id == round_id_now],
    by = c('treatment_id', 'treatment_name')) |>
    merge(data_now, by = 'treatment_id')
  setorder(treatments_now, arm_id)

  treatment_text = lapply(seq_len(nrow(treatments_now)), \(i) {
    r = treatments_now[i]
    list(
      strong(r$treatment_name), '(',
      em(glue('{r$N} students'), .noWS = 'outside'),
      paste('):', r$treatment_description),
      br())
  })

  round_text = tagList(
    overview_text, h5('Treatments'),
    unlist(treatment_text, recursive = FALSE), br())
}

#' Get number of students per round
#'
#' @param data `data.table` of individual-level data with columns `round_id` and
#'   `round_name`.
#'
#' @return `data.table`
get_counts_by_round = function(data) {
  counts = data[, .N, keyby = .(round_id, round_name)]
  counts = rbind(
    counts[, !'round_id'], data.table(round_name = 'Total', N = sum(counts$N)))
  counts[, N := scales::label_comma()(N)]
  setnames(counts, c('round_name', 'N'), c('Round', 'Students'))
}

#' Get number of students per treatment
#'
#' @param data `data.table` of individual-level data with columns `arm_id` and
#'   `treatment_name`.
#'
#' @return `data.table`
# get_counts_by_treatment = function(data) {
#   counts = data[, .N, keyby = .(arm_id, treatment_name)]
#   counts[, N := scales::label_comma()(N)]
#   counts[, .(Treatment = treatment_name, `Number of students` = N)]
# }

########################################

#' Get summary barplot for outcomes data
#'
#' This function creates a barplot, optionally faceted, to summarize counts or
#' percentages based on individual-level data.
#'
#' @param data `data.table` of individual-level data, e.g., from ConnectEd.
#' @param col string indicating column in `data` that contains logical values to
#'   use for calculating counts or percentages.
#' @param fill_vals character vector indicating fill colors for bars.
#' @param title string indicating title of plot.
#' @param x_col string indicating column in `data` to use for x-axis.
#' @param by_treatment logical indicating whether to facet by `treatment_name`.
#' @param percent logical indicating whether to plot percentages or counts.
#' @param bar_width numeric used by [ggplot2::geom_col()] for width of bars.
#' @param text_size numeric used by [ggplot2::geom_text()] for text above bars.
#'
#' @return `ggplot` object.
get_summary_barplot = function(
    data, col, fill_vals, title, x_col = 'time', by_treatment = FALSE,
    percent = TRUE, bar_width = 0.7, text_size = 5.5) {

  stopifnot(is_logical(by_treatment))
  stopifnot(is_logical(percent))

  by1 = c(x_col, col)
  if (by_treatment) by1 = c('round_id', 'treatment_id', 'treatment_name', by1)
  by2 = by1[-length(by1)]

  data_now = data[, .N, keyby = by1]
  if (percent) {
    data_now[, quant_students := N / sum(N), by = by2]
    data_now[, label := paste0(round(100 * quant_students), '%')]
  } else {
    data_now[, quant_students := sum(N), by = by2]
  }

  data_now = data_now[z == TRUE, env = list(z = col)]

  y_lab = paste(if (percent) 'Percentage' else 'Number', 'of students')
  y_scale = if (percent) scales::label_percent() else waiver()

  p = ggplot(data_now, aes(x = .data[[x_col]], y = quant_students)) +
    geom_col(aes(fill = .data[[x_col]]), width = bar_width) +
    labs(x = '', y = y_lab, title = title) +
    scale_y_continuous(labels = y_scale) +
    scale_fill_manual(values = fill_vals) +
    theme(legend.position = 'none')

  if (by_treatment) p = p + facet_wrap(vars(treatment_name))
  if (percent) {
    nudge_y = max(data_now$quant_students) * 0.04
    p = p + geom_text(aes(label = label), size = text_size, nudge_y = nudge_y)
  }
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
get_detailed_barplot = function(
    data, col, x_col = 'time', by_treatment = FALSE, percent = TRUE,
    bar_width = 0.7) {

  stopifnot(is_logical(by_treatment))
  stopifnot(is_logical(percent))

  y_lab = paste(if (percent) 'Percentage' else 'Number', 'of students')
  y_scale = if (percent) scales::label_percent() else waiver()
  position = if (percent) 'fill' else 'stack'

  p = ggplot(data) +
    geom_bar(
      aes(x = .data[[x_col]], fill = .data[[col]]),
      width = bar_width, position = position) +
    labs(x = '', y = y_lab, fill = 'Level') +
    scale_y_continuous(labels = y_scale) +
    scale_fill_viridis_d(na.value = 'gray')
  if (by_treatment) p = p + facet_wrap(vars(treatment_name))
  p
}
