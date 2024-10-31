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

![covid-19 map](https://github.com/user-attachments/assets/ddd6fe4e-29c9-44dd-902a-7be2a223955b)

Vaccinations Map

![vaccinations map](https://github.com/user-attachments/assets/6a3b9288-5e8b-47d5-8202-5e71596310f6)

COVID-19 Animated Chart

![covid-19 chart](https://github.com/user-attachments/assets/8d7d3849-cd6c-4eb8-b5a1-00195f87956b)

Vaccinations Animated Chart

![vaccinations chart](https://github.com/user-attachments/assets/dcb0500f-978e-4100-9c4d-351d063d1d24)

Factors Chart

![factors chart](https://github.com/user-attachments/assets/7f955d2c-9a46-4790-9525-0e6f6506342b)
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
