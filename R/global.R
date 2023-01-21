library('cowplot')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
# library('ggokabeito')
library('ggplot2')
library('plotly')
library('shiny')
library('zeallot')

########################################

theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 20),
      plot.title = element_text(face = 'bold'),
      axis.text = element_text(color = 'black'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')))

thematic::thematic_shiny()

########################################

get_connected_results = function(input_dir = '.') {
  d = fread(file.path(input_dir, 'connected_data.csv'))
  a = fread(file.path(input_dir, 'connected_arms.csv'))
  v = fread(file.path(input_dir, 'connected_levels.csv'))

  d = merge(d, a, by = c('round_id', 'arm_id'))
  d[, improved := end_level > start_level]

  d_long = melt(d, measure.vars = c('start_level', 'end_level'))
  d_long[, time := factor(
    variable, c('start_level', 'end_level'), c('Sensitization', 'Endline'))]
  d_long[, can_add := value > 0]
  d_long[, can_divide := value == 4]

  r = list(data = d, data_long = d_long, arms = a, levs = v)
}

input_dir = if (shiny::isRunning()) '.' else 'R'
conn = get_connected_results(input_dir)

########################################

get_summary_barplot = function(
    d, rounds, col, col_val, title, nudge_y, fill_vals, x_col = 'time',
    by_arm = FALSE, percent = TRUE, bar_width = 0.7, text_size = 5.5) {

  stopifnot(is_logical(by_arm))
  stopifnot(is_logical(percent))

  by1 = c(x_col, col)
  if (by_arm) by1 = c('round_id', 'arm_id', 'arm_name', by1)
  by2 = by1[-length(by1)]

  # TODO: account for all rounds, even if zero counts
  r = d[round_id %in% rounds, .N, keyby = by1]
  if (percent) {
    r[, quant_students := N / sum(N), by = by2]
    r[, label := paste0(round(100 * quant_students), '%')]
  } else {
    r[, quant_students := sum(N), by = by2]
  }

  r_sub = r[z == col_val, env = list(z = col)]

  y_lab = if (percent) 'Percentage' else 'Number'
  y_scale = if (percent) scales::label_percent() else waiver()
  up = if (uniqueN(r_sub[[x_col]]) == 1L) 1 else NA

  p = ggplot(r_sub, aes(x = .data[[x_col]], y = quant_students)) +
    labs(x = 'Timepoint', y = paste(y_lab, 'of students'), title = title) +
    geom_col(aes(fill = .data[[x_col]]), width = bar_width) +
    scale_y_continuous(labels = y_scale, limits = c(0, up)) +
    scale_fill_manual(values = fill_vals) +
    theme(legend.position = 'none', axis.title.x = element_blank())

  if (by_arm) p = p + facet_wrap(vars(arm_name))
  if (percent) {
    p = p + geom_text(aes(label = label), size = text_size, nudge_y = nudge_y)
  }
  p
}
