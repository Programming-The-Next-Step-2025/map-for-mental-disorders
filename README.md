# 🧠 Mental Health DALYs in Europe

This R package contains a Shiny web application that visualizes the burden of mental health disorders across Europe using 2021 DALY (Disability-Adjusted Life Years) data.

##  Installation

Clone this repository and open it in RStudio:

git clone https://github.com/Programming-The-Next-Step-2025/map-for-mental-disorders.git

Then in R:
devtools::load_all()
secondassignment::startApp()

Features
Interactive Map: Click on a country or use the dropdown menu to select it.

Cause-specific Bar Chart: View the percentage contribution of various mental health disorders.

Live Filtering: Hover over map regions and explore national DALY breakdowns.

📊 Data Source
The data comes from the Global Burden of Disease project and includes:

DALYs per 100,000 people

Disaggregated by age, sex, cause, and country (2021 data)

Built With
shiny + shinydashboard – web UI

ggplot2 – visualizations

leaflet – interactive map

sf, dplyr, rnaturalearth – spatial data and processing
