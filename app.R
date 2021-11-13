
library(shiny)
library(leaflet)
#Custom Leaflet.extras library with extensions for this project
#remotes::install_github("https://github.com/MitchStares/leaflet.extras.updated") 
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
    #Reactive value for clicking on map
    clickData <- reactiveValues(clickedPolygon=NULL) #to store click position
    uploadedFile <- reactiveValues(shapefile = NULL) #Figure out a fix so that multiple upload events dont overwrite reactiveValue
    polygonTracker <- data.frame("id" = c(1), "source" = c("dummy"))
    
    proxy <- leafletProxy("mymap") #Define Proxy for later use
    
    ## Render Leaflet Map
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
    
    ## Click Event ##
        # Extract data on click, store in reactiveValue
        # Highlight clicked polygon, remove previous highlights
    
    observeEvent(input$mymap_shape_click,{
        clickData$clickedPolygon <- input$mymap_shape_click
        print(clickData$clickedPolygon$id)
        #Check clicked polygon exists (Drawn)
        # drawnPolys <-
        #     sf::read_sf(jsonlite::toJSON(
        #         input$mymap_draw_all_features,
        #         force = TRUE,
        #         auto_unbox = TRUE,
        #         digits = NA
        #     ))
        # clickPoly <-
        #     drawnPolys[which(drawnPolys$'X_leaflet_id' == clickData$clickedPolygon$id), ]
        # #remove previously highlighted polygon
        # proxy %>% clearGroup("highlighted_polygon")
        # 
        # #include data source (clickPoly)
        # proxy %>% addPolygons(stroke = TRUE, weight = 2, color = "blue",data = clickPoly, group = "highlighted_polygon")
        
        output$printText <- renderText({
            print(paste0("latitude: ", clickData$clickedPolygon$lat, ", longitude: ", clickData$clickedPolygon$lng))
            
            #TODO: Add highlighting for selected polygon (and remove previous highlighting)
            #https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
            #TODO: Highlighting could be drastically improved for performance. Reading in the polygon each time is awfully slow. 
        })
    })
    
    ## Generate Grid Button event ##
    observeEvent(input$generateGrid, {
        drawnFeatures <- data.frame()
        reactive(drawnFeatures)
        if (is.null(drawnFeatures) &&
            is.null(clickData$clickedPolygon) &&
            is.null(uploadedFile$shapefile)) {
            #we have clicked button and drawn nothing or uploaded nothing
            print(
                "Error: Nothing on map. Just because the button is there, doesn't mean we need to click it"
            )
        } else if (is.null(clickData$clickedPolygon$id)) {
            output$printText <- renderText({
                print(paste0("Selected Polygon does not contain a unique ID"))
            })
        } else if (!is.null(clickData$clickedPolygon)) {
            #clicked button and have previously clicked something on map
            
            ## TODO: Fix this section to filter and extract of polygonTracker for more intelligent filtering
            
            ## TODO: polygonTracker subsetting needs to be able to filter either clickedPolygon$id or clickedPolygon$id$iter[[1]]
            print(polygonTracker)
            clickSource <-
                polygonTracker[which(
                    polygonTracker[,"id"] == clickData$clickedPolygon$id #||
                        #polygonTracker$id == clickData$clickedPolygon$id$iter[[1]]
                ), ]
            print(clickSource)
            if (is.null(clickSource) ||
                is.na(clickSource$source) ||
                clickSource$source != "uploadFile$shapefile" ||
                clickSource$source != "input$mymap_draw_all_features") {
                print("Error: No compatible source detected")
            } else if (clickSource$source == "input$mymap_draw_all_features") {
                drawnFeatures <-
                    input$mymap_draw_all_features #potentially change this to be responsive to dataframe cell
                print(input$mymap_draw_all_features)
                feature <-
                    sf::read_sf(
                        jsonlite::toJSON(
                            drawnFeatures,
                            force = TRUE,
                            auto_unbox = TRUE,
                            digits = NA
                        )
                    )
                feature <-
                    feature[which(feature$'X_leaflet_id' == clickData$clickedPolygon$id),]
            } else if (clickSource$source == "uploadFile$shapefile") {
                #TODO: This logic needs to be fixed.
                feature <-
                    uploadedFile$shapefile[which(uploadedFile$shapefile$iter == clickData$clickedPolygon$id$iter[[1]]), ]
            }
        }
        #Check for empty/poor filtering and exit out
        if (nrow(feature) == 0) {
            print("detected feature is empty")
        } else {
            grid <- makeGrid(feature)
            selected <- selectPlots(grid[[1]])
            proxy %>% addPolygons(data = selected,
                                  group = "grids",
                                  color = "red")
        }
    })
    #Currently breaks on Circle plots. Need to deal with makeGrids ability to do circles. See notion issue
    
    ## TODO: On click (above), store SOMETHING in a reactiveValue to reference against
    ## Test referencing and calling the reference against paste(clickData$lat, clickData$lng)
    
    ## Using reference in reactiveValue, assign Generate Grid to work on that reactiveValue
    ## Output makeGrid and/or selected to leaflet map on new group layer with addPolygon()

    #Track new drawings, add to static table
    #TODO: add mymap_draw_deleted_features tracking to remove from this table on delete incase new polys get drawn with same ID
    observeEvent(input$mymap_draw_new_feature, {
        id <- input$mymap_draw_new_feature$properties$`_leaflet_id`
        row <- data.frame("id" = c(id), "source" = c("input$mymap_draw_all_features"))
        polygonTracker <- rbind(polygonTracker, row)
    })
    
 
    ## Generate Shape List Action Button ##
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
    #TODO: Fix uniqueID so multiple upload events will maintain uniqueIDs 
    observeEvent(input$drawingfile, {
        drawFile = input$drawingfile
        req(drawFile)
        #Geojson
        if (file_ext(drawFile) == "geojson" ||
            file_ext(drawFile) == "json") {
            uploaded <- st_read(dsn = drawFile$datapath)
            uploaded <-
                st_transform(uploaded, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#            uploadedFile$shapefile <- uploaded
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
        for (n in 1:nrow(uploaded)) {
            featureType <- sf::st_geometry_type(uploaded[n,])
            uploaded[n,"iter"] <- 50+n
            uploadId <- 50+n #add uniqueID and source to polygonTracker
            uploadRow <- c(uploadId, "uploadFile$shapefile")
            polygonTracker <- rbind(polygonTracker,uploadRow )
            if(featureType == "POLYGON" || featureType == "MULTIPOLYGON"){
                proxy %>% addPolygons(data = uploaded[n,],group = "draw", layerId = uploaded[n,"iter"])
            }
            if(featureType == "POINT" || featureType == "MULTIPOINT"){
                proxy %>% addMarkers(data = uploaded[n,],group = "draw")
            }
            if(featureType == "LINESTRING" || featureType == "MULTILINESTRING") {
                proxy %>% addPolylines(data = uploaded[n,], group = "draw")
            }
        }
        uploadedFile$shapefile <- uploaded
        print(uploadedFile$shapefile)
    })
}

shinyApp(ui = ui, server = server)
