
library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(flex_dashboard)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- fluidPage(
  tags$head(HTML("<title>Health Indicator Data</title>")),
  useShinyjs(),
  br(),
  span(style = "font-weight: 600; font-size: 30px; width: 100%;
         color: #022DB7;", "Health Indicators in 2020 Data"),
  br(),br(),
  
  
  #leaflet Output!!
  
  leafletOutput("mymap", height = 1000),
  p(),
  actionButton("recalc", "New points"),
  print("Hello World!")
)

server <- function(input,output, session){
  
  library(readr)
  data2020 <- read_csv("~/GitHub/DS_112_Project/data2020.csv")
  
  library(rnaturalearth)
  map <- ne_countries()
  names(map)[names(map) == "iso_a3"] <- "ISO3"
  names(map)[names(map) == "name"] <- "NAME"
  plot(map)
  map$projecteddeathperc2020 <- data2020$projecteddeathperc2020[match(map$ISO3,data2020$ISO3)]
  library(DT)
  DT::datatable(map@data[, c("ISO3", "NAME", "projecteddeathperc2020")],
                rownames = FALSE, options = list(pageLength = 10)
  )

  
  #renders the leaflet
  output$mymap <- renderLeaflet({
    
      pal <- colorBin(
        palette = "magma", domain = map$projecteddeathperc2020,
        bins = seq(1, max(map$projecteddeathperc2020, na.rm = TRUE) + 0, by = .01)
      )
      
      #map labels
      map$labels <- paste0(
        "<strong> Country: </strong> ",
        map$NAME, "<br/> ",
        "<strong> projecteddeathperc2020: </strong> ",
        map$projecteddeathperc2020, "<br/> "
      ) %>%
        lapply(htmltools::HTML)
      
      leafletdata2020 <- leaflet(map) %>%
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

