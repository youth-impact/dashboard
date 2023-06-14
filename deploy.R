# done: created GitHub Secret for SHINY_APPS_IO
# done: created GitHub Secret for GOOGLE_TOKEN
# done: made each secret an env var in the GitHub Actions yaml files
# done: added the corresponding email address as viewer on the drive folder

if (Sys.getenv('GITHUB_ACTIONS') == 'true') {
  # on GitHub Actions, Sys.getenv('GOOGLE_TOKEN') should be a json string
  # so we write the multi-line json string to a file
  # and write the path to the file to .Renviron
  # now the json file and .Renviron should be sent to shinyapps.io

  path = 'google_token.json'
  cat(Sys.getenv('GOOGLE_TOKEN'), '\n', file = path)
  cat(paste0('GOOGLE_TOKEN=', path), '\n', file = '.Renviron', append = TRUE)

  shiny_cred = yaml::read_yaml(text = Sys.getenv('SHINY_APPS_IO'))
  do.call(rsconnect::setAccountInfo, shiny_cred)
  rsconnect::deployApp(appName = Sys.getenv('APP_NAME'), forceUpdate = TRUE)
}
