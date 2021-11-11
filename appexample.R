library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shiny)
library(shinydashboard)

sgmap55 <- readOGR("https://raw.githubusercontent.com/aeiyuni/regioncount/master/55_MP14_PLNG_AREA_WEB_PL.kml")
wfmap <- read.csv("https://raw.githubusercontent.com/aeiyuni/regioncount/master/wfmap.csv")


bins <-c(1,50,100,150,200,250,300,350,400,450,500)
pal <- colorBin("YlGnBu", domain = wfmap$count, bins = bins, na.color = "#808080")

labels <- sprintf(
  "<strong>%s</strong><br/>%g respondents </sup>",
  wfmap$planarea, wfmap$count
) %>% lapply(htmltools::HTML)


ui<- fluidPage(
  sidebarPanel(
    selectInput("region", "Planning Area:", 
                choices = wfmap$planarea)
  ),
  mainPanel(
    leafletOutput("sgmap2", height= "1000px"))
  
)


server <- function(input, output, session){
  
  output$sgmap2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSearchOSM()%>%
      addResetMapButton()%>%
      clearMarkers()%>%
      addProviderTiles("OpenStreetMap") %>%
      setView(103.8198,1.3521,12) %>%
      addPolygons(data = sgmap55,
                  weight = 1,
                  color = "white",
                  smoothFactor = 0.5,
                  fillOpacity = 0.8,
                  fillColor = pal(wfmap$count),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = "By region",
                  layerId = ~Name
      ) %>%
      addLegend(pal = pal,
                values = wfmap$count,
                opacity = 0.7,
                position = "topright")
    
  })
  
  observe({
    
    ## the sgmap2 needs to match the name of the map you're outputting above
    event <- input$sgmap2_shape_click
    print( event )
    updateSelectInput(session, inputId = "region", selected = event$id
    )
    
  }) 
}

shinyApp(ui, server)