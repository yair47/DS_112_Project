
library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#vars <- c("vulnerabilty" = "vulhealth2020", "Projected Percent Increase of Climate-Change Induced Deaths"  = "projecteddeathperc2020",
#          "Sanitation Accessibility" = "sanitation2020")

ui <- fluidPage(
  tags$head(HTML("<title>Health Indicator Data</title>")),
  useShinyjs(),
  br(),
  titlePanel("Global Vulnerability Health Indicator Data"),
  
  
  br(),
  
 sidebarLayout(position = 'right',
               sidebarPanel(
                 selectInput("color","Color", vars, selected ="vulhealth2020"),
                 width = 2),
                 
                 #leaflet Output!!
                 mainPanel(leafletOutput("mymap", height = 1000)))
               
)

server <- function(input,output, session){
  
  library(readr)
  data2020 <- read_csv("~/GitHub/DS_112_Project/data2020.csv")
  
 
  library(rnaturalearth)
  map <- ne_countries()
  names(map)[names(map) == "iso_a3"] <- "ISO3"
  names(map)[names(map) == "name"] <- "NAME"
  
  map$vulhealth2020 <- data2020$vulhealth2020[match(map$ISO3,data2020$ISO3)]

  
  #renders the leaflet
  output$mymap <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = "magma", domain = map$vulhealth2020,
    )
    
    map$labels <- paste0(
      "<strong> Country: </strong> ",
      map$NAME, "<br/> ",
      "<strong> Overall Health Vulnerability 2020: </strong> ",
      map$vulhealth2020, "<br/> "
    ) %>%
      lapply(htmltools::HTML)
    
    leafletdata2020vul <- leaflet(map) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addPolygons(
        fillColor = ~ pal(vulhealth2020),
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
        pal = pal, values = ~vulhealth2020,
        opacity = 0.7, title = "Health Vulnerability </br> Score in 2020"
      )
    
    
    
    map$labels <- paste0(
      "<strong> Country: </strong> ",
      map$NAME, "<br/> ",
      "<strong> medstaff2020: </strong> ",
      map$projecteddeathperc2020, "<br/> "
    ) %>%
      lapply(htmltools::HTML)
    
    leafletdata2020_med <- leaflet(map) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addPolygons(
        fillColor = ~ pal(projecteddeathperc2020),
        color = "black",
        weight = 2,
        fillOpacity = 1,
        label = ~labels,
        highlight = highlightOptions(
          color = "white",
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~projecteddeathperc2020,
        opacity = 0.7, title = "projecteddeathperc2020"
      )
    
    leafletdata2020
  })
}


shinyApp(ui = ui, server = server)

runApp("App-1")

