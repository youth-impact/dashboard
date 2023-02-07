# done: created GitHub Secret for GOOGLE_TOKEN containing the json
# done: added the corresponding email address as viewer on the drive folder

if (Sys.getenv('GITHUB_ACTIONS') == 'true') {
  # if (!dir.exists('secrets')) dir.create('secrets')
  path = 'google_token.json'
  # on GitHub Actions, Sys.getenv('GOOGLE_TOKEN') should be a json string
  cat(Sys.getenv('GOOGLE_TOKEN'), file = path) # write the json string to a file
  # the file should be sent to shinyapps.io, but something's not working
  cat(glue::glue('GOOGLE_TOKEN={path}\n'), file = '.Renviron', append = TRUE)
}

shiny_cred = if (Sys.getenv('SHINY_APPS_IO') == '') {
  yaml::read_yaml(file = file.path('secrets', 'shinyappsio.yaml'))
} else {
  yaml::read_yaml(text = Sys.getenv('SHINY_APPS_IO'))
}

do.call(rsconnect::setAccountInfo, shiny_cred)
rsconnect::deployApp(appName = Sys.getenv('APP_NAME'), forceUpdate = TRUE)
