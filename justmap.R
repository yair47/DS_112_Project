
library(readr)
id_health_medstaff <- read_csv("~/GitHub/DS_112_Project/resources/indicators/id_heal_04/input.csv", 
                               col_types = cols(`1995` = col_double(), 
                                                `2015` = col_double()))
gdp <- read_csv("~/GitHub/DS_112_Project/resources/indicators/gdp/score.csv", 
                col_types = cols(`1995` = col_skip(), 
                                 `1996` = col_skip(), `1997` = col_skip(), 
                                 `1998` = col_skip(), `1999` = col_skip(), 
                                 `2000` = col_skip(), `2001` = col_skip(), 
                                 `2002` = col_skip(), `2003` = col_skip(), 
                                 `2004` = col_skip(), `2005` = col_skip(), 
                                 `2006` = col_skip(), `2007` = col_skip(), 
                                 `2008` = col_skip(), `2009` = col_skip(), 
                                 `2010` = col_skip(), `2011` = col_skip(), 
                                 `2012` = col_skip(), `2013` = col_skip(), 
                                 `2014` = col_skip(), `2015` = col_skip(), 
                                 `2016` = col_skip(), `2017` = col_skip(), 
                                 `2018` = col_skip(), `2019` = col_skip()))
pop <- read_csv("~/GitHub/DS_112_Project/resources/indicators/pop/input.csv", 
                col_types = cols(`1995` = col_skip(), 
                                 `1996` = col_skip(), `1997` = col_skip(), 
                                 `1998` = col_skip(), `1999` = col_skip(), 
                                 `2000` = col_skip(), `2001` = col_skip(), 
                                 `2002` = col_skip(), `2003` = col_skip(), 
                                 `2004` = col_skip(), `2005` = col_skip(), 
                                 `2006` = col_skip(), `2007` = col_skip(), 
                                 `2008` = col_skip(), `2009` = col_skip(), 
                                 `2010` = col_skip(), `2011` = col_skip(), 
                                 `2012` = col_skip(), `2013` = col_skip(), 
                                 `2014` = col_skip(), `2015` = col_skip(), 
                                 `2016` = col_skip(), `2017` = col_skip(), 
                                 `2018` = col_skip(), `2019` = col_skip()))
#projected percent increase in climate-change induced deaths
projecteddeath_perc <- read_csv("~/GitHub/DS_112_Project/resources/indicators/id_heal_01/input.csv",
                                col_types = cols(`1995` = col_skip(), 
                                                 `1996` = col_skip(), `1997` = col_skip(), 
                                                 `1998` = col_skip(), `1999` = col_skip(), 
                                                 `2000` = col_skip(), `2001` = col_skip(), 
                                                 `2002` = col_skip(), `2003` = col_skip(), 
                                                 `2004` = col_skip(), `2005` = col_skip(), 
                                                 `2006` = col_skip(), `2007` = col_skip(), 
                                                 `2008` = col_skip(), `2009` = col_skip(), 
                                                 `2010` = col_skip(), `2011` = col_skip(), 
                                                 `2012` = col_skip(), `2013` = col_skip(), 
                                                 `2014` = col_skip(), `2015` = col_skip(), 
                                                 `2016` = col_skip(), `2017` = col_skip(), 
                                                 `2018` = col_skip(), `2019` = col_skip()))
pop2020 <- pop %>% 
  rename("pop2020" = "2020")

gdp2020 <- gdp %>% 
  rename("gdp2020" = "2020")

medstaff2020 <- id_health_medstaff %>% 
  rename("medstaff2020" = "2020") %>% 
  select(ISO3,Name,medstaff2020)

projecteddeathperc2020 <- projecteddeath_perc %>% 
  rename("projecteddeathperc2020" = "2020")

data2020 <- pop2020 %>% 
  left_join(gdp2020) %>% 
  left_join(medstaff2020) %>% 
  left_join(projecteddeathperc2020)


library(rnaturalearth)
map <- ne_countries()
names(map)[names(map) == "iso_a3"] <- "ISO3"
names(map)[names(map) == "name"] <- "NAME"

plot(map)

map$projecteddeathperc2020 <- data2020$projecteddeathperc2020[match(map$ISO3,data2020$ISO3)]

library(leaflet)

pal <- colorBin(
  palette = "magma", domain = map$projecteddeathperc2020,
  bins = seq(1, max(map$projecteddeathperc2020, na.rm = TRUE) + 0, by = .01)
)


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
    pal = pal, values = ~projecteddeathperc2020,
    opacity = 0.7, title = "projecteddeathperc2020"
  )

leafletdata2020
