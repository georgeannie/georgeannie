# FEMA Code Challenge Package Instructions

## App Installation Workflow

After having extracted zip folder contents to file:

1. Navigate to Rshiny_FEMA folder
2. Validate that the file called "Attachment 6 mock_data.csv" is in the folder Rshiny_FEMA/data
3. Open Rshiny_FEMA project by clicking on Rshiny_FEMA.Rproj file
4. Once open in RStudio, run install.packages("shiny") in RStudio Console
	Ignore the prompt to run renv::restore()
5. Once shiny package is installed and up-to-date, open app.R
6. Once open, select Run App

## Project Contents

Contents of the project are divided between four folders:

1. markdown folder
  - Includes three Rmd files exploration_modeling, simulation_test, and generate_plots
  - These files include all code needed to explore mock data and model simulation, test simulation, and generate plots, respectively
2. Rshiny_FEMA
  - Includes all contents needed to run the app
  - Instruction are provided in the App Workflow section above
  - Files in this folder use the same code provided in Rmd files
3. simulation
  - Includes deployment_simulation R script of all functions needed to generate stimulation data
  - This script is used to generate data that appears in plots of Decision Tool tab in the app
  - Also includes readme file that describes all inputs and defaults for functions
4. tests
  - Includes test_simulation script for additional testing
