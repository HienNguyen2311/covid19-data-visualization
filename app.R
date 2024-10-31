#------------------------------------------------------------------------------
#--------------------IMPORT DATA-----------------------------------------------
#------------------------------------------------------------------------------

library(rvest)
library(tidyverse)

# Import Worldometers data
url1 <- read_html('https://www.worldometers.info/coronavirus/')

# Read the HTML table
tables <- url1 %>% html_table()
table1 <- tables[[1]]
table1 <- data.frame(table1)

# Import OWID data
owid_data <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')

# Import continents and countries from continents.csv
continents <- read.csv("continents.csv", na.strings = "")

#------------------------------------------------------------------------------
#--------------------DATA PREPARATION------------------------------------------
#------------------------------------------------------------------------------

# For Worldometers - HTML table, change data from str to numeric type

table1$TotalCases <- gsub(',','',table1$TotalCases)
table1$TotalCases <- as.numeric(table1$TotalCases)

table1$NewCases <- gsub('+','',table1$NewCases)
table1$NewCases <- gsub(',','',table1$NewCases)
table1$NewCases <- as.numeric(table1$NewCases)

table1$ActiveCases <- gsub(',','',table1$ActiveCases)
table1$ActiveCases <- as.numeric(table1$ActiveCases)

table1$TotalDeaths <- gsub(',','',table1$TotalDeaths)
table1$TotalDeaths <- as.numeric(table1$TotalDeaths)

table1$Tot.Cases.1M.pop <- gsub(',','',table1$Tot.Cases.1M.pop)
table1$Tot.Cases.1M.pop <- as.numeric(table1$Tot.Cases.1M.pop)

# Change the country names in Worldometers table to match with the country names in polygons table

table1[which(table1$Country.Other=="USA"),2] <- "United States"
table1[which(table1$Country.Other=="UK"),2] <- "United Kingdom"
table1[which(table1$Country.Other=="Libya"),2] <- "Libyan Arab Jamahiriya"
table1[which(table1$Country.Other=="Ivory Coast"),2] <- "Cote d'Ivoire"
table1[which(table1$Country.Other=="Congo"),2] <- "Democratic Republic of the Congo"
table1[which(table1$Country.Other=="Tanzania"),2] <- "United Republic of Tanzania"
table1[which(table1$Country.Other=="Syria"),2] <- "Syrian Arab Republic"
table1[which(table1$Country.Other=="Czechia"),2] <- "Czech Republic"
table1[which(table1$Country.Other=="Moldova"),2] <- "Republic of Moldova"
table1[which(table1$Country.Other=="Iran"),2] <- "Iran (Islamic Republic of)"
table1[which(table1$Country.Other=="Myanmar"),2] <- "Burma"
table1[which(table1$Country.Other=="Laos"),2] <- "Lao People's Democratic Republic"
table1[which(table1$Country.Other=="Vietnam"),2] <- "Viet Nam"
table1[which(table1$Country.Other=="S. Korea"),2] <- "Korea, Republic of"
table1[which(table1$Country.Other=="Cabo Verde"),2] <- "Cape Verde"
table1[which(table1$Country.Other=="Falkland Islands"),2] <- "Falkland Islands (Malvinas)"
table1[which(table1$Country.Other=="Eswatini"),2] <- "Swaziland"
table1[which(table1$Country.Other=="Vatican City"),2] <- "Holy See (Vatican City)"
table1[which(table1$Country.Other=="Turks and Caicos"),2] <- "Turks and Caicos Islands"
table1[which(table1$Country.Other=="Brunei"),2] <- "Brunei Darussalam"
table1[which(table1$Country.Other=="Micronesia"),2] <- "Micronesia, Federated States of"
table1[which(table1$Country.Other=="North Macedonia"),2] <- "The former Yugoslav Republic of Macedonia"
table1[which(table1$Country.Other=="Réunion"),2] <- "Reunion"
table1[which(table1$Country.Other=="St. Vincent Grenadines"),2] <- "Saint Vincent and the Grenadines"
table1[which(table1$Country.Other=="Wallis and Futuna"),2] <- "Wallis and Futuna Islands"

# Change the country names in OWID data to match with the country names in polygons table

owid_data2 <- owid_data

owid_data2[which(owid_data2$location=="Libya"),3] <- "Libyan Arab Jamahiriya"
owid_data2[which(owid_data2$location=="Congo"),3] <- "Democratic Republic of Congo"
owid_data2[which(owid_data2$location=="Tanzania"),3] <- "United Republic of Tanzania"
owid_data2[which(owid_data2$location=="Syria"),3] <- "Syrian Arab Republic"
owid_data2[which(owid_data2$location=="Czechia"),3] <- "Czech Republic"
owid_data2[which(owid_data2$location=="Moldova"),3] <- "Republic of Moldova"
owid_data2[which(owid_data2$location=="Myanmar"),3] <- "Burma"
owid_data2[which(owid_data2$location=="Laos"),3] <- "Lao People's Democratic Republic"
owid_data2[which(owid_data2$location=="Vietnam"),3] <- "Viet Nam"
owid_data2[which(owid_data2$location=="South Korea"),3] <- "Korea, Republic of"
owid_data2[which(owid_data2$location=="Falkland Islands"),3] <- "Falkland Islands (Malvinas)"
owid_data2[which(owid_data2$location=="Eswatini"),3] <- "Swaziland"
owid_data2[which(owid_data2$location=="Vatican"),3] <- "Holy See (Vatican City)"
owid_data2[which(owid_data2$location=="Brunei"),3] <- "Brunei Darussalam"
owid_data2[which(owid_data2$location=="Micronesia (country)"),3] <- "Micronesia, Federated States of"
owid_data2[which(owid_data2$location=="North Macedonia"),3] <- "The former Yugoslav Republic of Macedonia"

# Convert the date for OWID data

owid_data$date <- as.Date(owid_data$date, format="%Y-%m-%d")

#------------------------------------------------------------------------------
#--------------------DATA TRANSORMATION----------------------------------------
#------------------------------------------------------------------------------

# Get all unique country names
country <- unique(owid_data2$location)

# Get a list of all countries available and their latest available TOTAL_VACCINATIONS

latest_total_vaccinations <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$total_vaccinations)
  return (rows_filtered_by_country[idx, "total_vaccinations"])
}

result_total_vaccinations <- sapply(country, latest_total_vaccinations)
result_total_vaccinations[sapply(result_total_vaccinations, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# PEOPLE FULLY VACCINATED

latest_people_fully_vaccinated <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$people_fully_vaccinated)
  return (rows_filtered_by_country[idx, "people_fully_vaccinated"])
}

result_people_fully_vaccinated <- sapply(country, latest_people_fully_vaccinated)
result_people_fully_vaccinated[sapply(result_people_fully_vaccinated, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# PEOPLE FULLY VACCINATED PER HUNDRED

latest_people_fully_vaccinated_per_hundred <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$people_fully_vaccinated_per_hundred)
  return (rows_filtered_by_country[idx, "people_fully_vaccinated_per_hundred"])
}

result_people_fully_vaccinated_per_hundred <- sapply(country, latest_people_fully_vaccinated_per_hundred)
result_people_fully_vaccinated_per_hundred[sapply(result_people_fully_vaccinated_per_hundred, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# TOTAL DEATHS PER MILLION

latest_total_deaths_per_million <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$total_deaths_per_million)
  return (rows_filtered_by_country[idx, "total_deaths_per_million"])
}

result_total_deaths_per_million <- sapply(country, latest_total_deaths_per_million)
result_total_deaths_per_million[sapply(result_total_deaths_per_million, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# SHARE OF AGE 65 AND OLDER

max_aged_65_older <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$aged_65_older)
  return (rows_filtered_by_country[idx, "aged_65_older"])
}

result_aged_65_older <- sapply(country, max_aged_65_older)
result_aged_65_older[sapply(result_aged_65_older, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# HANDWASHING-FACILITIES

max_handwash_facility <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$handwashing_facilities)
  return (rows_filtered_by_country[idx, "handwashing_facilities"])
}

result_handwash_facility <- sapply(country, max_handwash_facility)
result_handwash_facility[sapply(result_handwash_facility, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# POPULATION DENSITY

max_population_density <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$population_density)
  return (rows_filtered_by_country[idx, "population_density"])
}

result_population_density <- sapply(country, max_population_density)
result_population_density[sapply(result_population_density, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# TOTAL TESTS PER THOUSAND

max_test_per_thousand <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$total_tests_per_thousand)
  return (rows_filtered_by_country[idx, "total_tests_per_thousand"])
}

result_test_per_thousand <- sapply(country, max_test_per_thousand)
result_test_per_thousand[sapply(result_test_per_thousand, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# TOTAL CASES PER MILLION

max_case_per_million <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$total_cases_per_million)
  return (rows_filtered_by_country[idx, "total_cases_per_million"])
}

result_case_per_million <- sapply(country, max_case_per_million)
result_case_per_million[sapply(result_case_per_million, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# GDP PER CAPITA

max_GDP <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$gdp_per_capita)
  return (rows_filtered_by_country[idx, "gdp_per_capita"])
}

result_GDP <- sapply(country, max_GDP)
result_GDP[sapply(result_GDP, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# TOTAL VACCINATIONS PER HUNDRED

max_vaccinations_per_hundred <- function(country) {
  rows_filtered_by_country <- owid_data2[owid_data2$location == country,]
  idx <- which.max(rows_filtered_by_country$total_vaccinations_per_hundred)
  return (rows_filtered_by_country[idx, "total_vaccinations_per_hundred"])
}

result_vaccinations_per_hundred <- sapply(country, max_vaccinations_per_hundred)
result_vaccinations_per_hundred[sapply(result_vaccinations_per_hundred, function(x) length(x)==0)] <- NA

#------------------------------------------------------------------------------

# Combine all of the above values in a dataframe

vaccines <- Map(c, result_total_vaccinations, result_people_fully_vaccinated,
                result_people_fully_vaccinated_per_hundred, result_aged_65_older,
                result_total_deaths_per_million, result_handwash_facility,
                result_population_density, result_test_per_thousand, result_case_per_million,
                result_GDP, result_vaccinations_per_hundred)

vaccines_df <- data.frame(matrix(unlist(vaccines), nrow=length(vaccines) , byrow=TRUE),stringsAsFactors=FALSE)
vaccines_df2 <- data.frame(Country=country, DosesAdministered=vaccines_df$X1,
                           PeopleFullyVaccinated=vaccines_df$X2, PercentagePeopleFullyVaccinated=vaccines_df$X3,
                           Shareof65YearsandOlder=vaccines_df$X4, TotalDeathsPerMillion=vaccines_df$X5,
                           HandWashingFacilities=vaccines_df$X6, PopulationDensity=vaccines_df$X7,
                           TotalTestsPerThousand=vaccines_df$X8, TotalCasesPerMillion=vaccines_df$X9,
                           GDPperCapita=vaccines_df$X10, TotalVaccinationsPerHundred=vaccines_df$X11)

# Get continent data into vaccines_df2

vaccines_df2 <- merge(vaccines_df2, continents, by.x = "Country", by.y = "Country")

# Exclude World's numbers

factors_df <- vaccines_df2[vaccines_df2$Country != "World",]

#------------------------------------------------------------------------------

owid_data3 <- owid_data

# Filter out the World's data and only keep the data for each country
owid_data3 <- owid_data3[owid_data3$continent!="",]

#------------------------------------------------------------------------------

# Values for Shiny app - absolute panel

total_case <- table1[which(table1$Country.Other=="World"),3]

total_new_case <- table1[which(table1$Country.Other=="World"),4]

total_death <- table1[which(table1$Country.Other=="World"),5]

total_ppl_fully_vaccinated <- vaccines_df2[which(vaccines_df2$Country=="World"),3]

pct_ppl_fully_vaccinated <- vaccines_df2[which(vaccines_df2$Country=="World"),4]

total_doses_administerd <- vaccines_df2[which(vaccines_df2$Country=="World"),2]

#------------------------------------------------------------------------------

# Create vaccine acceptance dataframe and merge it with factors_df for visualization used in factors chart

vaccine_acceptance <- read.csv("vaccine acceptance.csv")

vaccine_acceptance[which(vaccine_acceptance$Country=="US"),2] <- "United States"
vaccine_acceptance[which(vaccine_acceptance$Country=="UK"),2] <- "United Kingdom"

vaccine_acceptance <- vaccine_acceptance[vaccine_acceptance$Target.Population == "General population",]

factors_df <- merge(factors_df, vaccine_acceptance, by.x = "Country", by.y = "Country", all = TRUE)

#------------------------------------------------------------------------------
#--------------------SHINY APP-------------------------------------------------
#------------------------------------------------------------------------------

devtools::install_github("jcheng5/googleCharts", force = TRUE)

library(RColorBrewer)
library(dplyr)
library(googleCharts)
library(leaflet)
library(rgdal)
library(shiny)
library(shinythemes)
library(shinyWidgets)

# Load the polygons table

world_spdf <- readOGR( 
  dsn= "C:\\Users\\pun23\\Documents\\uni\\data 472\\project", 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Merge Worldometers data and OWID data with polygons table

world_covid <- merge(
  world_spdf, table1, by.x = "NAME", by.y = "Country.Other")

world_vaccines <- merge(
  world_spdf, vaccines_df2, by.x = "NAME", by.y = "Country")

# Check for miss-matched countries names when joining polygons table with other datasets

world_covid@data$NAME[which(is.na(world_covid@data$TotalCases))]
world_vaccines@data$NAME[which(is.na(world_vaccines@data$PeopleFullyVaccinated))]

# ------------------------------------------------------------------------------

# Map features

mybins <- c(0, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, Inf)

mypalette1 <- colorBin(palette="YlOrBr", domain=world_covid@data$TotalCases,
                       na.color="transparent", bins=mybins)
mypalette2 <- colorBin(palette="PuBu", domain=world_vaccines@data$PeopleFullyVaccinated,
                       na.color="transparent", bins=mybins)

mytooltip1 <- paste(
  "Country: ", "<strong>", world_covid@data$NAME, "<br/>", 
  "Active cases: ", format(world_covid@data$ActiveCases, big.mark=","), "<br/>",
  "Cumulative cases: ", format(world_covid@data$TotalCases, big.mark=","), "<br/>",
  "Total cases per 1M population: ", format(world_covid@data$Tot.Cases.1M.pop, big.mark=","),
  sep="") %>%
  lapply(htmltools::HTML)
mytooltip2 <- paste(
  "Country: ", "<strong>", world_vaccines@data$NAME, "<br/>", 
  "Doses administered: ", format(world_vaccines@data$DosesAdministered, big.mark=","), "<br/>",
  "People fully vaccinated: ", format(world_vaccines@data$PeopleFullyVaccinated, big.mark=","), "<br/>",
  "% people fully vaccinated: ", world_vaccines@data$PercentagePeopleFullyVaccinated, "%",
  sep="") %>%
  lapply(htmltools::HTML)

# MAP

covid_map <- (
  leaflet(world_covid)
  %>% addTiles()
  %>% setView(lat=10, lng=0, zoom=2)
  %>% addPolygons( 
    fillColor = ~mypalette1(world_covid@data$TotalCases),
    stroke = TRUE, 
    fillOpacity = 0.9, 
    color = "white", 
    weight = 0.3,
    label = mytooltip1,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto",
    )
  )
  %>% addLegend(
    pal = mypalette1,
    values = ~world_covid@data$TotalCases,
    opacity = 0.9,
    title = "Cumulative cases:",
    position = "bottomleft"
  )
)
vaccinations_map <- (
  leaflet(world_vaccines)
  %>% addTiles()
  %>% setView(lat=10, lng=0, zoom=2)
  %>% addPolygons( 
    fillColor = ~mypalette2(world_vaccines@data$PeopleFullyVaccinated),
    stroke = TRUE, 
    fillOpacity = 0.9, 
    color = "white", 
    weight = 0.3,
    label = mytooltip2,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto",
    )
  )
  %>% addLegend(
    pal = mypalette2,
    values = ~world_vaccines@data$PeopleFullyVaccinated,
    opacity = 0.9,
    title = "People Fully Vaccinated:",
    position = "bottomleft"
  )
)

# ------------------------------------------------------------------------------

# Pre-define x and y selection variables for the COVID-19 factor chart

factorChartChoices <- list(
  `Share of 65 Years and Older` = "Shareof65YearsandOlder",
  `Hand-washing Facilities` = "HandWashingFacilities",
  `Population Density` = "PopulationDensity",
  `Total Tests per Thousand` = "TotalTestsPerThousand",
  `COVID-19 Vaccination Acceptance Rate` = "Acceptance.Rate",
  `GDP per Capita` = "GDPperCapita",
  `Total Deaths per Million` = "TotalDeathsPerMillion",
  `Total Cases per Million` = "TotalCasesPerMillion",
  `Percentage People Fully Vaccinated` = "PercentagePeopleFullyVaccinated",
  `Total Vaccinations per Hundred` = "TotalVaccinationsPerHundred"
)

# ------------------------------------------------------------------------------

ui <- tagList(
  googleChartsInit(),
  tags$head(
    includeCSS("C:\\Users\\pun23\\Documents\\uni\\data 472\\project\\www\\styles.css")
  ),
  navbarPage(
    title = "COVID-19",
    collapsible = TRUE,
    tabPanel(
        "Maps",
        leafletOutput("map", width = "auto", height = "auto"),
        radioButtons(
            "choice",
            "Choice",
            c("COVID-19" = "covid_map", "Vaccinations" = "vaccinations_map")
        ),
        
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 75, left = 55, width = 250, fixed=TRUE,
                      draggable = TRUE, height = "auto",
                      
                      span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")),
                           style="color:#045a8d"),
                      span(tags$b(h3(textOutput("total_case_id"), align = "middle")), style="color:#777777"),
                      span(tags$b(h4(textOutput("total_new_case_id"), align = "middle")), style="color:#777777"),
                      span(tags$b(h5(textOutput("total_death_id"), align = "middle")), style="color:#777777"),
                      span(tags$b(h3(textOutput("total_ppl_fully_vaccinated_id"), align = "middle")), style="color:#777777"),
                      span(tags$b(h5(textOutput("pct_ppl_fully_vaccinated_id"), align = "middle")), style="color:#777777"),
                      span(tags$b(h5(textOutput("total_doses_administerd_id"), align = "middle")), style="color:#777777")
        ),
    ),
    tabPanel(
      "Animated Charts",
      googleBubbleChart(
        "COVID19chart",
        width = "100%",
        height = "700px",
        options = list(
          hAxis = list(
            title = "New Cases",
            viewWindowMode = "pretty",
            viewWindow = list(
              min = 0.0,
              max = max(owid_data3$new_cases) * 1.1
            ),
            logScale = TRUE
          ),
          vAxis = list(
            title = "New Tests",
            viewWindow = list(
              min = 0.0,
              max = max(owid_data3$new_tests) * 2.0
            ),
            logScale = TRUE
          ),
          sizeAxis = list(
            minValue = 0.0,
            maxValue = max(owid_data3$new_deaths)
          ),
          explorer = list(),
          bubble = list(
            opacity = 0.4,
            stroke = "none",
            textStyle = list(
              color = "none"
            )
          )
        )
      ),
      sliderInput(
        "date",
        "Date",
        width = "100%",
        min = min(owid_data3$date),
        max = max(owid_data3$date),
        value = max(owid_data3$date),
        animate = TRUE
      ),
      googleBubbleChart(
        "vaccinationsChart",
        width = "100%",
        height = "700px",
        options = list(
          hAxis = list(
            title = "New Vaccinations",
            viewWindowMode = "pretty",
            viewWindow = list(
              min = 0.0,
              max = max(owid_data3$new_vaccinations) * 1.1
            ),
            logScale = TRUE
          ),
          vAxis = list(
            title = "People Fully Vaccinated",
            viewWindow = list(
              min = 0.0,
              max = max(owid_data3$people_fully_vaccinated) * 2.0
            ),
            logScale = TRUE
          ),
          sizeAxis = list(
            minValue = 0.0,
            maxValue = max(owid_data3$population)
          ),
          explorer = list(),
          bubble = list(
            opacity = 0.4,
            stroke = "none",
            textStyle = list(
              color = "none"
            )
          )
        )
      ),
      checkboxGroupInput(
        "continents",
        "Continents",
        unique(owid_data3$continent),
        selected = unique(
          owid_data3$continent
        )
      ),
      pickerInput(
        "countries",
        "Countries",
        unique(owid_data3$location),
        selected = unique(owid_data3$location),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    ),
    tabPanel(
      "Factors Chart",
      tagList(
        selectInput(
          "factorChartX",
          "X axis",
          choices = factorChartChoices,
          selected = "Shareof65YearsandOlder"
        ),
        selectInput(
          "factorChartY",
          "Y axis",
          choices = factorChartChoices,
          selected = "TotalDeathsPerMillion"
        )
      ),
      googleBubbleChart(
        "factorChart",
        width = "100%",
        height = "700px",
        options = list(
          hAxis = list(
            logScale = TRUE
          ),
          vAxis = list(
            logScale = TRUE
          ),
          explorer = list(),
          bubble = list(
            opacity = 0.4,
            stroke = "none",
            textStyle = list(
              color = "none"
            )
          ),
          sizeAxis = list(
            maxSize = 5
          )
        )
      ),
      checkboxGroupInput(
        "Continents",
        "Continents",
        unique(owid_data3$continent),
        selected = unique(
          owid_data3$continent
        )
      ),
      pickerInput(
        "Countries",
        "Countries",
        factors_df$Country,
        selected = factors_df$Country,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    )
  )
)

server <- function(input, output, session)
{
  
  # tab panel "Maps"
  output$map <- renderLeaflet({
      get(input$choice)
  })
  
  output$total_case_id <- renderText({
    paste0(prettyNum(total_case, big.mark=","), " cases")
  })
  
  output$total_new_case_id <- renderText({
    paste0(prettyNum(total_new_case, big.mark=","), " new cases")
  })
  
  output$total_death_id <- renderText({
    paste0(prettyNum(total_death, big.mark=","), " deaths")
  })
  
  output$total_ppl_fully_vaccinated_id <- renderText({
    paste0(prettyNum(total_ppl_fully_vaccinated, big.mark=","), " people fully vaccinated")
  })
  
  output$pct_ppl_fully_vaccinated_id <- renderText({
    paste0(prettyNum(pct_ppl_fully_vaccinated, big.mark=","), "% people fully vaccinated")
  })
  
  output$total_doses_administerd_id <- renderText({
    paste0(prettyNum(total_doses_administerd, big.mark=","), " doses administerd")
  })
  
  # tab panel "Animated Charts"
  getDataCovid <- reactive({
    df <- (
      owid_data3
      %>% filter(date == input$date)
      %>% filter(continent %in% input$continents)
      %>% filter(location %in% input$countries)
      %>% select(location, new_cases, new_tests, continent, new_deaths)
      %>% arrange(continent)
    )
  })
  
  output$COVID19chart <- reactive({
    list(
      data = googleDataTable(getDataCovid())
    )
  })
  
  getDataVaccinations <- reactive({
    df <- (
      owid_data3
      %>% filter(date == input$date)
      %>% filter(continent %in% input$continents)
      %>% filter(location %in% input$countries)
      %>% select(location, new_vaccinations, people_fully_vaccinated, continent, population)
      %>% arrange(continent)
    )
  })
  
  output$vaccinationsChart <- reactive({
    list(
      data = googleDataTable(getDataVaccinations())
    )
  })
  
  # tab panel "Factors Chart"
  factorChartX <- reactive({ input$factorChartX })
  factorChartY <- reactive({ input$factorChartY })
    
  getFactorChartData <- reactive({
    df <- (
      factors_df
      %>% filter(Continent %in% input$Continents)
      %>% filter(Country %in% input$Countries)
      %>% select(Country, factorChartX(), factorChartY(), Continent)
      %>% arrange(Continent)
    )
  })
    
  output$factorChart <- reactive({
    list(
      data = googleDataTable(getFactorChartData())
    )
  })
  
}

shinyApp(ui = ui, server = server)



 
