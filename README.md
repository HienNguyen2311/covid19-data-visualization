# COVID-19 Data Visualization Dashboard
This project is an interactive R Shiny application that provides a visualization of COVID-19 data worldwide. It offers insights into case numbers, vaccination rates, and various factors influencing the pandemic's spread across different countries and continents.
## Features
* Interactive world maps showing COVID-19 cases and vaccination rates
* Animated bubble charts displaying daily new cases, tests, and vaccinations
* Factor analysis chart to explore correlations between various demographic, health, and economic factors with COVID-19 metrics
* Time slider for temporal analysis
* Continent and country filters for detailed regional exploration
## Installation
1. Clone this repository
```
git clone https://github.com/HienNguyen2311/covid19-data-visualization.git
```
2. Install R and RStudio.
3. Open the project in RStudio.
4. Install the required packages:
```
install.packages(c("shiny", "leaflet", "dplyr", "ggplot2", "RColorBrewer", "rgdal", "shinythemes", "shinyWidgets"))
devtools::install_github("jcheng5/googleCharts")
```
## Usage
1. Open the app.R file in RStudio.
2. Run the whole session.
3. The app will open in your default web browser.
4. Use the navigation tabs to switch between different visualizations.
5. Interact with the maps, charts, and filters to explore the data.
## Data Sources
* Our World in Data: https://covid.ourworldindata.org/data/owid-covid-data.csv
* Worldometers: https://www.worldometers.info/coronavirus/
* PubMed Central (PMC): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7920465/
## Technologies and Libraries Used
R, Shiny, leaflet, dplyr, ggplot2, RColorBrewer, rgdal, shinythemes, shinyWidgets, googleCharts
## Screenshots
COVID-19 Map

![covid-19 map](https://github.com/user-attachments/assets/b99e39b1-2d63-4f46-ab9c-cd170c8d65a4)

Vaccinations Map

![vaccinations map](https://github.com/user-attachments/assets/77df3b7e-d3c0-41aa-829e-f24899406cc7)

COVID-19 Animated Chart

![covid-19 chart](https://github.com/user-attachments/assets/5131534c-1282-493e-8711-522d5a4a08e8)

Vaccinations Animated Chart

![vaccinations chart](https://github.com/user-attachments/assets/bb6f9cbe-b902-41b8-9c17-e21f392e6043)

Factors Chart

![factors chart](https://github.com/user-attachments/assets/441e12be-7d86-48c8-a439-1d3cd7378b75)
## License
This project is licensed under the MIT License.
## Project Report
A detailed project report is available in the file `report.pdf`. This report includes:

* Introduction and motivation for the project
* Overview of the data sources used
* Detailed explanation of the app's features and visualizations
* Insights gained from the development process
* Conclusions and potential future improvements

The report provides in-depth information about the project's background, methodology, and findings. It's recommended to read the report for a comprehensive understanding of the COVID-19 Data Visualization Dashboard.
