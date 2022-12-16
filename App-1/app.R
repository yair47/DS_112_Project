library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(flexdashboard)
library(rnaturalearth)


library(readr)
data2020 <- read_csv("data2020.csv")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

chosen <- c("Overall Health Vulnerabilty" = "vulhealth2020", "Projected Percent Increase of Climate-Change Induced Deaths"  = "projecteddeathperc2020",
          "Sanitation Accessibility" = "sanitation2020", "Medical Staff in Developing Countries" = "medstaff2020", "Change in Medical Staff from 1995-2020" = "med_percchange")

ui <- fluidPage(
  tags$head(HTML("<title>Health Indicator Data</title>")),
  useShinyjs(),
  br(),
  titlePanel("The State of Global Health & Vulnerability as a Result of Climate Change"),
  
 sidebarLayout(position = "right",
               sidebarPanel(
                 selectInput(
                   inputId = "variableselected",
                   label = "Select variable",
                   choices = chosen
                   )
                 ),
              
                 mainPanel(
                   
                   #leaflet Output!!
                   h3("Global Indicator Data"),
                   leafletOutput("mymap", height = 600),
                           
                           br(),
                           br(),
               
               #Table output
               h3("Search:"),
               DTOutput(outputId = "table"),
               )
 )
               
)

server <- function(input, output, session){

  #renders the table of data
  output$table <- renderDT(data2020)

  
  #renders the leaflet
  output$mymap <- renderLeaflet({
    
    map <- ne_countries()
    names(map)[names(map) == "iso_a3"] <- "ISO3"
    names(map)[names(map) == "name"] <- "NAME"
    
    
    ordercounties <- match(map@data$ISO3, data2020$ISO3)
    map@data <- data2020[ordercounties, ]
    
    map$variableplot <- as.numeric(unlist((map@data[, input$variableselected])))
    
    pal <- colorNumeric(
      palette = "inferno", domain = map$variableplot,
    )
    
    
    map$labels <- paste0(
      "<strong> Country: </strong> ",
      map$NAME, "<br/> ",
      "<strong> Vulnerability score: </strong> ",
      map$variableplot, "<br/> "
    ) %>%
      lapply(htmltools::HTML)
    
    leafletdata2020vul <- leaflet(map) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        weight = 2,
        fillOpacity = 1,
        label = ~labels,
        highlight = highlightOptions(
          color = "black",
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = "Vulnerability Indicator </br> in 2020"
      )
  
  })
}

shinyApp(ui = ui, server = server)


