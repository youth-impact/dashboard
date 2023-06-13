# Shiny Dashboard

## Overview

The Shiny app comprises several modules, with largely one module per script. Most of the modules include a ui and a server, and correspond to one panel of the app.

## Inventory

- .github/: GitHub Actions workflows that deploy the app to shinyapps.io.
  - main.yaml: runs on push to main, deploys the app to the main app.
  - pr.yaml: runs on pull request, deploys the app to the devel app.
- .Rprofile: runs renv/activate.R for new R sessions.
- app.R: defines the two high-level elements of the app, namely ui and server.
- deploy.R: script used by GitHub Actions workflows to deploy the app to shinyapps.io, ensuring that the app can access files on Google Drive.
- params.yaml: parameters used by the app.
- R/: scripts defining the app, thus used by app.R.
  - connected.R: module for ConnectEd.
  - reach.R: module for Reach.
  - tarllit.R: module for TaRL Literacy.
  - tarlnum.R: module for TaRL Numeracy.
  - zones.R: module for Zones.
  - get_data.R: module to get data from Google Drive and to process the data for use by other modules.
  - get_data_utils.R: defines functions used by the get_data module.
  - utils.R: loads packages and parameters, defines functions used by various modules.
- renv/activate.R: script to activate renv for the project.
- renv.lock: lockfile describing state of project library.

## Running the app locally

1. Update the repository using `git pull` or similar.

1. Switch to the desired git branch, if not main.

1. Update the project's private R library using `renv::restore()`.

1. Run the Shiny application using `shiny::runApp()`.
