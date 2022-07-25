# the web map should show the total no. of voters in each constituency.

library(shiny)
library(htmltools)
library(dplyr)
library(tools)
library(leaflet)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(DT)

wards <- read.csv('wards.csv', header = T)
attach(wards)
wards_shapefile <- readOGR(dsn='wards_shapefile.shp')

# create ui that show the name of presiding officer
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = 'presiding_officer',
        label = 'Name of presiding officer',
        placeholder = 'ex. Tom Omondi'
      ),
      
      # option to select the ward
      selectizeInput(
        inputId = 'selected_wards',
        label = 'Choose the ward you are presiding over',
        choices = c(wards$ward)
      ),
      
      # show the total number of voters who came to vote
      numericInput(
        inputId = 'total_voters',
        label = 'Total voters who came to vote',
        value = 0,
        min = 0,
        max = NA
      ),
      
      # show the results of the five different presidential candidates
      toTitleCase('David Waihiga Mwaure'),
      numericInput(
        inputId = 'votes_david',
        label = 'Votes for David Waihiga Mwaure',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('George Wajackoyah'),
      numericInput(
        inputId = 'votes_george',
        label = 'Votes for George Wajackoyah',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('Raila Odinga'),
      numericInput(
        inputId = 'votes_raila',
        label = 'Votes for Raila Odinga',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('Reuben Kigame'),
      numericInput(
        inputId = 'votes_reuben',
        label = 'Votes for Reuben Kigame',
        value = 0,
        min = 0,
        max = NA
      ),
      
      toTitleCase('William Ruto'),
      numericInput(
        inputId = 'votes_william',
        label = 'Votes for William Ruto',
        value = 0,
        min = 0,
        max = NA
      ),
      
      # put the submit button
      actionButton(inputId = "submit",
                   label = "Submit")
    ),
    # mainpanel
    mainPanel(
      leafletOutput(outputId = 'wards_map', width = '100%', height = 400), # webmap showing no. of voters who came, % of votes for each of the five presidential candidates (clickable)
      DTOutput(outputId = 'table_react'), # is reactive to the total votes cast (mandatory) and is reactive to the names of the presidential candidates to show the votes cast to each
      uiOutput(outputId = 'officer_name')
    )
  )
)

# define the server function 

server <- function(input, output, session){
  
  table_results <- eventReactive(input$submit, {
    req(input$selected_wards)
    input$total_voters
    input$votes_david
    input$votes_george
    input$votes_raila
    input$votes_reuben
    input$votes_william
    
    data.frame(ward = input$selected_wards, voters = input$total_voters, david_waih = input$votes_david, 
               george_waj = input$votes_george, raila_odin = input$votes_raila, reuben_kig = input$votes_reuben,
               william_ru = input$votes_william)
  })
  
  
  presiding_officer_name <- eventReactive(input$submit, {
    input$presiding_officer
    
  })
  
  factpal <- colorFactor(palette = rainbow(47), unique(wards_shapefile@data$county))
  
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = wards_shapefile, stroke = T, weight = 0.5,
                fillColor = ~factpal(wards_shapefile@data$county), 
                fillOpacity = 0.2, popup = paste0("County: ", wards_shapefile$county, "<br>",
                                                  "Sub_county: ", wards_shapefile$subcounty, "<br>",
                                                  "Wards: ", wards_shapefile$ward))
  
  output$wards_map <- renderLeaflet(map)
  
  br()
  br()
  
  # create a reactive table showing total no. of voters for each ward, and results for each of the five 
  # presidential candidates
  output$table_react <- renderDT(table_results())
  
  br()
  
  output$officer_name <- renderUI({HTML(paste0("Signed by Presiding officer name: ", "<br>",
                                               presiding_officer_name()))})
}


# create the shiny app object
shinyApp(ui, server)























