# Dashboard

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
  - connected_ab_detailed.R: module to create and display detailed plots for ConnectEd A/B tests.
  - connected_ab_summary.R: module to create and display summary plots for ConnectEd A/B tests.
  - connected_pooled.R: module to create and display plots for pooled ConnectEd data.
  - data_status.R: module to show which files are being fetched from Google Drive (intended for development, not necessarily for production).
  - get_data.R: modules to get the raw data from Google Drive and to process the raw data for use by other modules.
  - global.R: loads packages and parameters, defines functions used by various modules.
- renv/activate.R: script to activate renv for the project.
- renv.lock: lockfile describing state of project library.

## Running the app locally

1. Update the repository using `git pull` or similar.

1. Switch to the desired git branch, if not main.

1. Update the project's private R library using `renv::restore()`.

1. Run the Shiny application using `shiny::runApp()`.
