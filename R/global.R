library('cowplot')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
library('ggokabeito')
library('ggplot2')
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

get_results = function() {
  results = fread('sim_data.csv')
  results[, improved := end_level > start_level]

  results_long = melt(
    results, measure.vars = c('start_level', 'end_level'))

  results_long[, time := factor(
    variable, c('start_level', 'end_level'), c('Sensitization', 'Endline'))]
  results_long[, can_add := value > 0]
  results_long[, can_divide := value == 4]

  r = list(results, results_long)
}

c(results, results_long) %<-% get_results()

########################################

get_pooled_barplot = function(
    d, rounds, col, col_val, title, nudge_y, fill_vals, by_time = TRUE,
    bar_width = 0.7, text_size = 5.5) {

  env = list(v = col)

  if (isTRUE(by_time)) {
    r = d[round_id %in% rounds, .N, keyby = .(time, v), env = env]
    r[, frac_students := N / sum(N), keyby = time]
  } else {
    r = d[round_id %in% rounds, .N, keyby = v, env = env]
    r[, frac_students := N / sum(N)]
    r[, time := 'Sensitization →\nEndline']
  }

  r_sub = r[v == col_val, env = env]
  r_sub[, perc_label := paste0(round(100 * frac_students), '%')]

  p = ggplot(r_sub, aes(x = time, y = frac_students))

  p = if (isTRUE(by_time)) {
    p +
      geom_col(aes(fill = time), width = bar_width) +
      geom_text(aes(label = perc_label), size = text_size, nudge_y = nudge_y) +
      labs(x = 'Timepoint', y = 'Percentage of students', title = title) +
      scale_y_continuous(labels = scales::label_percent()) +
      scale_fill_manual(values = fill_vals) +
      theme(legend.position = 'none', axis.title.x = element_blank())
  } else {
    p +
      geom_col(width = bar_width, fill = fill_vals[1L]) +
      geom_text(aes(label = perc_label), size = text_size, nudge_y = nudge_y) +
      labs(x = 'Timepoint', y = 'Percentage of students', title = title) +
      scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
      theme(axis.title.x = element_blank())
  }
}
