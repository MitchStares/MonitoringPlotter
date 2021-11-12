
library(shiny)
library(leaflet)
#Custom Leaflet.extras library with extensions for this project
#remotes::install.github("https://github.com/MitchStares/leaflet.extras.updated") 
library(leaflet.extras)
library(utils)
library(xfun)
library(sf)
library(jsonlite)
source("functions.R") #read in custom functions

sh <- data.frame()

ui <- fluidPage(titlePanel("Monitoring Plotter"),
                
                sidebarLayout(
                    sidebarPanel(
                        fileInput(
                            "drawingfile",
                            h4(strong("Input Drawing File")),
                            accept = c(
                                '.shp',
                                '.dbf',
                                '.sbn',
                                '.sbx',
                                '.shx',
                                '.prj',
                                '.cpg',
                                '.geojson',
                                '.json'
                            ),
                            multiple = TRUE
                        ),
                        selectInput(
                            'fileSave',
                            "Select file type",
                            choices = c("GeoJSON", "ESRI Shapefile")
                        ),
                        actionButton("printShapes", h5(strong(
                            "Generate Drawing File"
                        ))),
                        actionButton("generateGrid", h5(strong(
                            "Generate grid on selected polygon"))),
                        textOutput("printText")
                    ),
                    mainPanel(leafletOutput("mymap", height = "1000px"),)
                ))

server <- function(input, output, session) {
    clickData <- reactiveValues(clickedPolygon=NULL) #to store click position
    
    observeEvent(input$mymap_shape_click,{
        clickData$clickedPolygon <- input$mymap_shape_click
        print(clickData$clickedPolygon$id)
        output$printText <- renderText({
            print(paste0("latitude: ", clickData$clickedPolygon$lat, ", longitude: ", clickData$clickedPolygon$lng))
            #TODO: Add highlighting for selected polygon (and remove previous highlighting)
            #https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
        })
    })
    
    
    observeEvent(input$generateGrid, {
        drawnFeatures <- data.frame()
        reactive(drawnFeatures)
        drawnFeatures <-
            input$mymap_draw_all_features
        if (is.null(drawnFeatures)) {
            print("nope")
        } else {
            feature <-
                sf::read_sf(jsonlite::toJSON(
                    drawnFeatures,
                    force = TRUE,
                    auto_unbox = TRUE,
                    digits = NA
                ))
            if(is.null(clickData$clickedPolygon$id)){
                output$printText <- renderText({
                    print(paste0("Selected Polygon does not contain a unique ID"))})
            } else{
            feature <-
                feature[which(feature$'X_leaflet_id' == clickData$clickedPolygon$id), ]
            grid <- makeGrid(feature)
            selected <- selectPlots(grid[[1]])
            proxy <-
                leafletProxy("mymap") %>% addPolygons(data = selected,
                                                      group = "grids",
                                                      color = "red")
            }
        }
    })
    #Currently breaks on Circle plots. Need to deal with makeGrids ability to do circles. See notion issue
    
    ## TODO: On click (above), store SOMETHING in a reactiveValue to reference against
    ## Test referencing and calling the reference against paste(clickData$lat, clickData$lng)
    
    ## Using reference in reactiveValue, assign Generate Grid to work on that reactiveValue
    ## Output makeGrid and/or selected to leaflet map on new group layer with addPolygon()

    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "Default", attribution = 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors') %>%
            setView(lng = 151,
                    lat = -33,
                    zoom = 7) %>%
            addDrawToolbar(
                targetGroup = "draw",
                position = "topleft",
                editOptions = editToolbarOptions(edit = TRUE)
            )
    })
    # Generate Shape List Action Button
    observeEvent(input$printShapes, {
        shapedf <- data.frame()
        reactive(shapedf)
        shapedf <- input$mymap_draw_all_features
        sh <-
            sf::read_sf(jsonlite::toJSON(
                shapedf,
                force = TRUE,
                auto_unbox = TRUE,
                digits = NA
            ))
        
        if (input$fileSave == "GeoJSON") {
            if (file.exists(paste0(getwd(), "/Drawings.geojson"))) {
                unlink(paste0(getwd(), "/Drawings.geojson"))
            }
            shpwrite <-
                sf::st_write(
                    sh,
                    dsn = paste0(getwd(), "/Drawings.geojson"),
                    driver = input$fileSave
                )
        }
        else if (input$fileSave == "ESRI Shapefile") {
            shpwrite <-
                sf::write_sf(
                    sh,
                    dsn = getwd(),
                    layer =  "Drawings",
                    driver = input$fileSave,
                    delete_layer = TRUE,
                    append = FALSE
                )
        }
        else if (input$fileSave == "CSV") {
            shpwrite <-
                sf::write_sf(
                    sh,
                    dsn = paste0(getwd(), "/Drawings.csv"),
                    driver = input$fileSave,
                    delete_layer = TRUE,
                    append = FALSE
                )
        }
    })
    
    # Intake Spatial File
    
    observeEvent(input$drawingfile, {
        drawFile = input$drawingfile
        req(drawFile)
        #Geojson
        if (file_ext(drawFile) == "geojson" ||
            file_ext(drawFile) == "json") {
            uploaded <- st_read(dsn = drawFile$datapath)
            uploaded <-
                st_transform(uploaded, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        }
        # CSV
        else if (file_ext(drawFile) == "csv") {
            uploaded <- st_read(dsn = drawFile$datapath)
            uploaded <-
                st_transform(uploaded, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        }
        # Shapefile
        else{
            uploadDirectory <- dirname(drawFile$datapath[1])
            previouswd <- getwd()
            setwd(uploadDirectory)
            for (i in 1:nrow(drawFile)) {
                file.rename(drawFile$datapath[i], drawFile$name[i])
            }
            setwd(previouswd)
            layerName <- sub(".dbf$", "", basename(drawFile$name[1]))
            uploaded <-
                st_read(dsn = uploadDirectory, layer = layerName)
            uploaded <-
                st_transform(uploaded, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        }
        proxy <- leafletProxy("mymap")
        for (i in 1:nrow(uploaded)) {
            featureType <- sf::st_geometry_type(uploaded[i,])
            if(featureType == "POLYGON" || featureType == "MULTIPOLYGON"){
                proxy %>% addPolygons(data = uploaded[i,],group = "draw")
            }
            if(featureType == "POINT" || featureType == "MULTIPOINT"){
                proxy %>% addMarkers(data = uploaded[i,],group = "draw")
            }
            if(featureType == "LINESTRING" || featureType == "MULTILINESTRING") {
                proxy %>% addPolylines(data = uploaded[i,], group = "draw")
            }
        }
    })
    leaflet() %>% addTiles() %>% leaflet.extras::addDrawToolbar()    
}

shinyApp(ui = ui, server = server)
