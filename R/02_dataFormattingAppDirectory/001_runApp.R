# app/run_app.R
library(shiny)

# Specify the path to the app directory
app_dir <- here::here("R", "02_dataFormattingAppDirectory")

# Run the Shiny app
shiny::runApp(app_dir)