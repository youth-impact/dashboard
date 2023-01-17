if (Sys.getenv('SHINY_APPS_IO') == '') {
  param_file = file.path('secrets', 'shinyappsio.yaml')
} else {
  param_file = withr::local_tempfile()
  writeLines(Sys.getenv('SHINY_APPS_IO'), param_file)
}
p = yaml::read_yaml(param_file)

rsconnect::setAccountInfo(p$name, p$token, p$secret)
rsconnect::deployApp(
  'R', appName = Sys.getenv('APP_NAME'), forceUpdate = TRUE)
