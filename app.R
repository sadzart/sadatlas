###### load required packages ######
# to use the famous pipe operator allowing more intuitive data manipulation
library(magrittr)
# library to set colors
library(RColorBrewer)
# library to draw maps
library(leaflet)
# library to draw plots
library(plotly)
# library to build the app with plugins
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
# library to use the sf_read function to load worldcountry data
library(sf)
# library for cleaning data and manipulating it, has dplyr, ggplot2, tidyr, among other very useful libraries as sublibraries
library(tidyverse)
#########################


############# DATASET ##################

## Data source for world countries,  https://tapiquen-sig.jimdofree.com/english-version/free-downloads/world/
## Data source for country coordinates, https://gist.github.com/tadast/8827699
## Data source for population data, https://databank.worldbank.org/reports.aspx?source=2&series=SP.POP.TOTL&country=#

# Loading Country Data and geolocation for leaflet map
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = st_read("input_data/World_Countries/World_Countries.shp")

# Loading Population Data and Cleaning It
popData = read.csv('input_data/popData.csv')
popData$Series.Name = NULL
popData$Series.Code = NULL
colnames(popData) = c('country', 'CODE', 'population')
popData$population = as.numeric(popData$population)


# Loading confirmed, deaths, and recovered data from JHU dataset of cases which gets updated daily.
confirmed_df <- read_csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))
confirmed_df$country = confirmed_df$`Country/Region`
confirmed_df$country[confirmed_df$country == 'US'] = "United States"
confirmed_df$country[confirmed_df$country == 'Czechia']  = "Czech Republic"
confirmed_df$`Country/Region` = NULL

recovered_df <- read_csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                         col_types = cols(
                           .default = col_double(),
                           `Province/State` = col_character(),
                           `Country/Region` = col_character()
                         ))
recovered_df$country = recovered_df$`Country/Region`
recovered_df$country[recovered_df$country == 'US'] = "United States"
recovered_df$country[recovered_df$country == 'Czechia']  = "Czech Republic"
recovered_df$`Country/Region` = NULL

deaths_df <- read_csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                      col_types = cols(
                        .default = col_double(),
                        `Province/State` = col_character(),
                        `Country/Region` = col_character()
                      ))

deaths_df$country = deaths_df$`Country/Region`
deaths_df$country[deaths_df$country == 'US'] = "United States"
deaths_df$country[deaths_df$country == 'Czechia']  = "Czech Republic"
deaths_df$`Country/Region` = NULL

# Combining JHU data with population data
countryData = popData
recovered_df = recovered_df %>% left_join(countryData)
confirmed_df = confirmed_df %>% left_join(countryData)
deaths_df = deaths_df %>% left_join(countryData)


# Mutating the series to have dates in rows rather than columns
ts_confirmed <- confirmed_df %>%
  gather("Date", "Confirmed", -c("Province/State", "country", "Lat", "Long", 'CODE', 'population')) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_recovered <- recovered_df %>%
  gather("Date", "Recovered", -c("Province/State", "country", "Lat", "Long", 'CODE', 'population')) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))

ts_deaths <- deaths_df %>%
  gather("Date", "Deaths", -c("Province/State", "country", "Lat", "Long",  'CODE', 'population')) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))


###################### main functions #######################
# set mapping colour for each outbreak
colorSelect = function(marker_type) {
  if (marker_type == 'cases') return('steelblue')
  if (marker_type == 'deaths') return('red')
  if (marker_type == 'recovered') return('forestgreen')
  return('yellow')
}

# to calculate daily changes
daily_increase = function(data){
  return(data - c(0,head(data,-1)))
}
# to calculate moving averages as daily changes are very eratic.
moving_average = function(data, duration){
  n = length(data)
  moving_average = rep(0,n)
  for (i in 1:n) {
    if (i + duration < n) {
      moving_average[i] = mean(data[i:(i+duration)])
    }
    else {
      moving_average[i] = mean(data[i:n])
    }
  }
  return(moving_average)
}

# setting the time period to take a moving average from.
durat = 7
###################### main functions #######################

# getting populations, this is done again as this has 188 entries unlike original population data (222 entries)
## Only populations for countries that are part of the JHU dataset
countryPop = ts_confirmed %>% group_by(country) %>% summarise(population = max(population))

ts_confirmed = ts_confirmed %>% group_by(country, Date) %>% summarise(Confirmed = sum(Confirmed), 
                                                                      Lat = first(Lat), Long = first(Long),
                                                                      population = first(population))
ts_recovered = ts_recovered %>% group_by(country, Date) %>% summarise(Recovered = sum(Recovered),
                                                                      Lat = first(Lat), Long = first(Long))
ts_deaths    = ts_deaths    %>% group_by(country, Date) %>% summarise(Deaths = sum(Deaths),
                                                                      Lat = first(Lat), Long = first(Long))


# Date filtered data. This is what we use to build the other datasets.
dateSelected = max(ts_confirmed$Date)
byDate = function(data, date) {
  data %>% filter(Date <= date)
}
ts_confirmed_date_filtered = byDate(ts_confirmed, dateSelected)
ts_deaths_date_filtered = byDate(ts_deaths, dateSelected)
ts_recovered_date_filtered = byDate(ts_recovered, dateSelected)

# Generating Maxes per country (if date filtered is max then the entire data is selected)
maxConfirmed = ts_confirmed_date_filtered %>% group_by(country) %>% summarise(confirmed = max(Confirmed))
maxRecovered = ts_recovered_date_filtered %>% group_by(country) %>% summarise(recovered = max(Recovered))
maxDeaths    = ts_deaths_date_filtered    %>% group_by(country) %>% summarise(deaths = max(Deaths))
active = maxConfirmed$confirmed - maxRecovered$recovered - maxDeaths$deaths
mortalityRate = maxDeaths$deaths / maxConfirmed$confirmed
recoveryRate = maxRecovered$recovered / maxConfirmed$confirmed
per100K = (maxConfirmed$confirmed * 100000) / countryPop$population
activeper100k = ((maxConfirmed$confirmed - maxRecovered$recovered - maxDeaths$deaths)* 100000) / countryPop$population
deathsper100k = (maxDeaths$deaths * 100000) / countryPop$population
maxDf = data.frame(country = maxConfirmed$country,
                   confirmed = maxConfirmed$confirmed, 
                   recovered = maxRecovered$recovered, 
                   deaths = maxDeaths$deaths,
                   active = maxConfirmed$confirmed
                   - maxRecovered$recovered
                   - maxDeaths$deaths,
                   mortalityRate = maxDeaths$deaths / maxConfirmed$confirmed,
                   recoveryRate = maxRecovered$recovered / maxConfirmed$confirmed,
                   per100K = (maxConfirmed$confirmed * 100000) / countryPop$population,
                   activeper100k = ((maxConfirmed$confirmed - maxRecovered$recovered - maxDeaths$deaths)
                                    * 100000) / countryPop$population,
                   deathsper100k = (maxDeaths$deaths * 100000) / countryPop$population
)


# Generating Totals per country
totalConfirmed = ts_confirmed_date_filtered %>% group_by(Date) %>% summarise(Confirmed = sum(Confirmed))
totalRecovered = ts_recovered_date_filtered %>% group_by(Date) %>% summarise(Recovered = sum(Recovered))
totalDeaths    = ts_deaths_date_filtered    %>% group_by(Date) %>% summarise(Deaths = sum(Deaths))
active = totalConfirmed$Confirmed - totalRecovered$Recovered - totalDeaths$Deaths
mortalityRate = totalDeaths$Deaths / totalConfirmed$Confirmed
recoveryRate = totalRecovered$Recovered / totalConfirmed$Confirmed
totalDf = data.frame(date = totalConfirmed$Date,
                     confirmed = totalConfirmed$Confirmed, 
                     recovered = totalRecovered$Recovered, 
                     deaths = totalDeaths$Deaths,
                     active_cases = totalConfirmed$Confirmed
                     - totalRecovered$Recovered
                     - totalDeaths$Deaths,
                     mortalityRate = totalDeaths$Deaths / totalConfirmed$Confirmed,
                     recoveryRate = totalRecovered$Recovered / totalConfirmed$Confirmed)


##
# Global Cases
c = tail(totalConfirmed$Confirmed,1)
cat("Global Cases:", sep="\n", prettyNum(c,big.mark=","))

# Cases by Country
countryMaxes = maxConfirmed %>% arrange(desc(confirmed))
countryMaxes$prettyConfirmed = prettyNum(countryMaxes$confirmed,big.mark=",")


cv_aggregated = data.frame(date = totalDf$date, cases = totalDf$confirmed, 
                           deaths = totalDf$deaths, recovered = totalDf$recovered, 
                           active_cases = totalDf$active_cases,
                           new_cases = daily_increase(totalDf$confirmed))
cv_aggregated$region = "Global"


# global dataset
cv_cases_global = totalDf
cv_cases_global$date            = totalDf$date
cv_cases_global$cases           = totalDf$confirmed
cv_cases_global$new_cases       = daily_increase(totalDf$confirmed)
cv_cases_global$deaths          = totalDf$deaths
cv_cases_global$new_deaths      = daily_increase(totalDf$deaths)
cv_cases_global$pop             = sum(countryPop$population)
cv_cases_global$permil          = as.numeric(format(round(cv_cases_global$cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$newpermil       = as.numeric(format(round(cv_cases_global$new_cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$deathspermil    = as.numeric(format(round(cv_cases_global$deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$newdeathspermil = as.numeric(format(round(cv_cases_global$new_deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$global_level = "Global"

# "current date"
current_date = max(totalDf$date)

# Merged dataset of ts_...
merged = ts_confirmed %>% left_join(ts_recovered, by = c('country','Date')) %>% left_join(ts_deaths, by = c('country', 'Date'))
merged$date = merged$Date
merged$Date = NULL
merged$cases = merged$Confirmed
merged$deaths = merged$Deaths
merged$recovered = merged$Recovered
merged$permil = (merged$cases * 1000000) / merged$population 
merged$deathspermil = (merged$deaths * 1000000) / merged$population 

merged = merged %>% group_by('country') %>% mutate(new_cases = daily_increase(Confirmed))
merged = merged %>% group_by(country)
merged = merged %>% mutate(new_cases = daily_increase(Confirmed))
merged = merged %>% mutate(new_av_cases = moving_average(new_cases, durat))
merged = merged %>% mutate(new_recovered = daily_increase(Recovered))
merged = merged %>% mutate(new_av_rec = moving_average(new_recovered, durat))
merged = merged %>% mutate(new_deaths = daily_increase(Deaths))
merged = merged %>% mutate(new_av_deaths = moving_average(new_deaths, durat))
merged$newpermil = (merged$new_cases * 1000000) / merged$population 
merged$newdeathspermil = (merged$new_deaths * 1000000) / merged$population 

merged$active_cases = merged$Confirmed - merged$Deaths - merged$Recovered
merged$region = merged$country
merged = merged %>% ungroup()


# From answer to https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
cls_names = c(as.character(maxDf$country),"Global")
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
m = length(totalConfirmed$Confirmed)
set.seed(2)
country_cols = sample(color, m)
names(country_cols) = cls_names
################    DATASET   ########################



######## MAP FUNCTIONS #############

# creat base map 
basemap = leaflet(worldcountry) %>% 
 addTiles() %>% 
 addProviderTiles("CartoDB.DarkMatter") %>%
 fitBounds(~-100,-50,~80,80)

# Moving Average Plot with color coded countries'
Moving_Average_Plot <- function(cv_cases, plot_start_date, plotvar) {
  plotvar1 = switch(plotvar,
                    "Cases (total)" = "new_cases",
                    "Deaths (total)" = "new_deaths",
                    "Recovered (total)" = "new_recovered")
  plotvar2 = switch(plotvar,
                    "Cases (total)" = "new_av_cases",
                    "Deaths (total)" = "new_av_deaths",
                    "Recovered (total)" = "new_av_rec")
  
  fig <- plot_ly(cv_cases, x = ~date, color = ~country, colors = country_cols)
  fig <- fig %>% add_trace(y = ~(eval(parse(text = plotvar1))), type = "scatter", mode = 'lines', fill = 'tozeroy', showlegend = FALSE)
  fig <- fig %>% add_trace(y = ~(eval(parse(text = plotvar2))), line = list(width = 4, dash = 'dot'))
  fig <- fig %>% layout(xaxis = list(title = "Time", range = plot_start_date), yaxis = list(title = "Daily Changes"))
  fig
  }



# function to plot cumulative COVID cases by date
total_plot = function(cv_aggregated, plot_start_date, plotvar) {
  
  marker_type = switch(plotvar,
                    "Cases (total)" = "cases",
                    "Deaths (total)" = "deaths",
                    "Recovered (total)" = "recovered")
  
  plot_df = cv_aggregated %>% filter(date %in% seq(plot_start_date[1],plot_start_date[2], by='day'))
  
  
  fig <- plot_ly(plot_df, x = ~date, color = ~region, colors = colorSelect(marker_type))
  fig <- fig %>% add_trace(y = ~(eval(parse(text = marker_type))), type = "scatter", mode = 'lines', fill = 'tozeroy', showlegend = FALSE)
  fig <- fig %>% layout(xaxis = list(title = "Time"), yaxis = list(title = "Total"))
  fig
}

 # function to plot cumulative cases by region
 country_cases_cumulative = function(cv_cases, start_point = "Date", plot_date) {
   
   fig <- plot_ly(cv_cases, x = ~date, color = ~region, colors = country_cols)
   fig <- fig %>% add_trace(y = ~outcome, type = "scatter", mode = 'lines', showlegend = TRUE)
   fig <- fig %>% layout(xaxis = list(range = plot_date, title = "Time"), yaxis = list(title = "Total"))
   fig
 }
 

######## MAP FUNCTIONS #############


######### SHINY UI ##########
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("cyborg"), collapsible = TRUE,
             "COVID-19 Visualization", id="nav",
             
             tabPanel("COVID-19 Map",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, right = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        h5(textOutput("reactive_count"), align = "center", style="color:gold"),
                                        radioButtons(
                                          "marker_type",
                                          label = h5("Select Marker"),
                                          choices = c("Total Cases" = "cases",
                                                      "Total Deaths" = "deaths",
                                                      "Total Recovered" = "recovered",
                                                      "New Cases" = "new_cases",
                                                      "Active Cases" = "active_cases")),
                                        
                                        sliderInput("plot_date",
                                                    label = h5("Select Maximum Date"),
                                                    min = min(merged$date),
                                                    max = max(merged$date),
                                                    value = max(merged$date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = 500, loop = FALSE))
                          )
                          
                          
                          
                          
                      )
             ),
             
             tabPanel("Plots",
                      
                      fluidRow(
                        
                        column(6,
                               tabsetPanel(
                                 #tabPanel("Table", dataTableOutput('mainTable')),
                                 tabPanel("Country Total", plotlyOutput("country_plot_cumulative")),
                                 #tabPanel("New", plotlyOutput("country_plot")),
                                 tabPanel("Country Daily", plotlyOutput("moving_average_plot")),
                                 tabPanel("Global Total", plotlyOutput("cumulative_plot"))
                               )
                        ),
                        
                        
                        column(6,
                               pickerInput("country_select", "Country:",   
                                           choices = maxDf[order(-maxDf$confirmed),"country"], 
                                           options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                           selected = maxDf[order(-maxDf$confirmed),"country"][1:2],
                                           multiple = TRUE), 
                               
                               pickerInput("outcome_select", "Outcome:",   
                                           choices = c("Cases (total)", "Deaths (total)", "Recovered (total)"), 
                                           selected = c("Cases (total)"),
                                           multiple = FALSE),
                               
                               sliderInput("minimum_date",
                                           "Minimum date:",
                                           min = min(merged$date),
                                           max = max(merged$date),
                                           value= c(min(merged$date), max(merged$date)),
                                           timeFormat="%d %b"),
                               
                        )
                        
                      )
             )
             
  )          
)


######### SHINY UI ##########




######## SHINY SERVER #########

server = function(input, output, session) {
  
 
  toListen <- reactive({
    list(input$plot_date,input$marker_type)
  })
  

  total_db = reactive({
    cv_aggregated
  })
  
  reactive_db = reactive({
    merged %>% filter(date == input$plot_date)
  })

  output$reactive_count <- renderText({
    paste0(prettyNum(sum(reactive_db()[input$marker_type]), big.mark=",")," ", input$marker_type)
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(toListen(), {
    
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      
      
      addCircleMarkers(data = reactive_db(), lat = ~ Lat, lng = ~ Long, weight = 2, radius = ~(eval(parse(text=input$marker_type)))^(1/5), 
                       fillOpacity = 0.5, color = colorSelect(input$marker_type), group = "2019-COVID (cases)",
                       label = 
                         sprintf("<strong>%s (Total)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %g<br/>Cases per Million: %g<br/>Deaths per Million: %g", 
                                       reactive_db()$country, reactive_db()$cases, 
                                       reactive_db()$deaths, reactive_db()$recovered, 
                                       reactive_db()$permil, reactive_db()$deathspermil) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = colorSelect(input$marker_type)),
                         textsize = "15px", direction = "auto"))
  })
  


   # create dataframe with selected countries
   country_reactive_db = reactive({
       db = merged
       db$region = db$country
   
     if (input$outcome_select=="Cases (total)") { 
       db$outcome = db$cases
       db$new_outcome = db$new_cases
     }
     
     if (input$outcome_select=="Deaths (total)") { 
       db$outcome = db$deaths 
       db$new_outcome = db$new_deaths 
     }
     
     if (input$outcome_select=="Recovered (total)") { 
       db$outcome = db$recovered
       db$new_outcome = db$new_recovered
     }
     db %>% filter(region %in% input$country_select)
   })
   
   # global plot
   output$cumulative_plot <- renderPlotly({
     total_plot(total_db(), input$minimum_date, input$outcome_select)
   })
   
  # country-specific plots
   output$country_plot_cumulative <- renderPlotly({
     country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
   })
   
   # moving average plot
   output$moving_average_plot <- renderPlotly({
     Moving_Average_Plot(country_reactive_db(), input$minimum_date,input$outcome_select)
   })

  }

############ SHINY SERVER ################### 
shinyApp(ui, server)