library('cowplot')
library('rlang') # load before data.table to avoid masking :=
library('data.table')
library('ggokabeito')
library('ggplot2')
library('shiny')

########################################

# dashboard_app = function(...) {
#   set_default_theme()
#   thematic::thematic_shiny()
#   shiny::shinyApp(ui, server, ...)
# }

# set_default_theme = function() {
theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 20),
      plot.title = element_text(face = 'bold'),
      axis.text = element_text(color = 'black'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')))
# thematic::thematic_shiny()
# }

# get_results = function() {
results = fread('sim_data.csv')

results[, improved := end_level > start_level]

results_long = melt(
  results, measure.vars = c('start_level', 'end_level'))

results_long[, time := factor(
  variable, c('start_level', 'end_level'), c('Sensitization', 'Endline'))]

results_long[, can_add := value > 0]
results_long[, can_divide := value == 4]
  # r = list(results, results_long)
# }
