library('cowplot')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
library('ggplot2')
library('glue')
library('googledrive')
library('shiny')

theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 20),
      axis.text = element_text(color = 'black'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')))

thematic::thematic_shiny()

########################################

# so shiny::runApp() and source('R/global.R') can load the params file
input_dir = if (shiny::isRunning()) '.' else 'R'
params = yaml::read_yaml(file.path(input_dir, 'params.yaml'))

if (Sys.getenv('GOOGLE_TOKEN') == '') {
  drive_auth(email = params$email)
} else {
  drive_auth(path = Sys.getenv('GOOGLE_TOKEN'))
}

########################################

get_file_metadata = function(folder_url) {
  files = setDT(drive_ls(folder_url))
  files = files[!startsWith(name, '_') & endsWith(name, '.csv')]
  setorder(files, name)
  files[, modified_time := sapply(files$drive_resource, \(f) f$modifiedTime)]
  files[]
}

get_summary_barplot = function(
    data, round_ids, col, col_val, title, nudge_y, fill_vals, x_col = 'time',
    by_arm = FALSE, percent = TRUE, bar_width = 0.7, text_size = 5.5) {

  stopifnot(is_logical(by_arm))
  stopifnot(is_logical(percent))

  by1 = c(x_col, col)
  if (by_arm) by1 = c('round_id', 'arm_id', 'arm_name', by1)
  by2 = by1[-length(by1)]

  data_now = data[round_id %in% round_ids, .N, keyby = by1]
  if (percent) {
    data_now[, quant_students := N / sum(N), by = by2]
    data_now[, label := paste0(round(100 * quant_students), '%')]
  } else {
    data_now[, quant_students := sum(N), by = by2]
  }

  data_now = data_now[z == col_val, env = list(z = col)]

  y_lab = paste(if (percent) 'Percentage' else 'Number', 'of students')
  y_scale = if (percent) scales::label_percent() else waiver()
  upper_lim = if (uniqueN(data_now[[x_col]]) == 1L) 1 else NA

  p = ggplot(data_now, aes(x = .data[[x_col]], y = quant_students)) +
    geom_col(aes(fill = .data[[x_col]]), width = bar_width) +
    labs(x = '', y = y_lab, title = title) +
    scale_y_continuous(labels = y_scale, limits = c(0, upper_lim)) +
    scale_fill_manual(values = fill_vals) +
    theme(legend.position = 'none')

  if (by_arm) p = p + facet_wrap(vars(arm_name))
  if (percent) {
    p = p + geom_text(aes(label = label), size = text_size, nudge_y = nudge_y)
  }
  p
}

get_detailed_barplot = function(
    data, round_ids, col, x_col = 'time', by_arm = FALSE, percent = TRUE,
    bar_width = 0.7) {

  stopifnot(is_logical(by_arm))
  stopifnot(is_logical(percent))

  y_lab = paste(if (percent) 'Percentage' else 'Number', 'of students')
  y_scale = if (percent) scales::label_percent() else waiver()
  position = if (percent) 'fill' else 'stack'
  data_now = data[round_id %in% round_ids]

  p = ggplot(data_now) +
    geom_bar(
      aes(x = .data[[x_col]], fill = .data[[col]]),
      width = bar_width, position = position) +
    labs(x = '', y = y_lab, fill = 'Level') +
    scale_y_continuous(labels = y_scale) +
    scale_fill_viridis_d()
  if (by_arm) p = p + facet_wrap(vars(arm_name))
  p
}
