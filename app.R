
# From:
# https://stackoverflow.com/questions/65347690/how-do-i-save-adddrawtoolbar-shapes-drawn-in-an-r-leaflet-shiny-map-so-i-can-re


library(shiny)
library(leaflet)
library(leaflet.extras)
library(utils)
library(xfun)
library(sf)
library(jsonlite)

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
                        )))
                    ),
                    mainPanel(leafletOutput("mymap", height = "1000px"),)
                ))

server <- function(input, output, session) {
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
    
    # observeEvent(input$drawingFile, {
    #     drawFile <- input$drawingFile
    #     ext <- file_ext(drawFile$datapath)
    #     req(drawFile)
    #     validate(need(ext == "csv", "Please upload a csv file."))
    #     
    #     ddf <- read.csv(drawFile$datapath, header = TRUE) # The drawing dataframe
    #     ind <- which(ddf == "Feature") # Index for drawing df to break up the df to redraw the shapes.
    #     ind <- as.array(ind)
    #     
    #     for (i in 1:nrow(ind)) {
    #         if(i != nrow(ind)) thisShape <- ddf[ind[i]:ind[i+1]]
    #         else thisShape <- ddf[ind[i]:ncol(ddf)]
    #         
    #         #####
    #         if(thisShape[3] == "polyline") {
    #             tf <- array(startsWith(names(thisShape),"features.geometry.coordinates"))
    #             w <- 1
    #             pnts <- array()
    #             for (i in 1:nrow(tf)) {
    #                 if(tf[i] == TRUE) {
    #                     pnts[w] <- thisShape[i]
    #                     w <- w+1
    #                 }
    #             }
    #             n <- 1
    #             m <- 1
    #             plng <- array()
    #             plat <- array()
    #             pnts <- as.array(pnts)
    #             for (j in 1:nrow(pnts)) {
    #                 if(j %% 2 == 1) {
    #                     plng[n] <- pnts[j]
    #                     n <- n+1
    #                 }
    #                 else if(j %% 2 == 0) {
    #                     plat[m] <- pnts[j]
    #                     m <- m+1
    #                 }
    #             }
    #             as.vector(plng, mode = "any")
    #             as.vector(plat, mode = "any")
    #             PL <- data.frame(matrix(unlist(plng)))
    #             PLsub <- data.frame(matrix(unlist(plat)))
    #             PL <- cbind(PL, PLsub)
    #             colnames(PL) <- c("lng","lat")
    #             PL1 <- reactiveVal(PL)
    #             
    #             proxy <- leafletProxy("mymap", data = PL1())
    #             proxy %>% addPolylines(lng = ~lng, lat = ~lat, group = "draw")
    #         }
    #         #####
    #         else if(thisShape[3] == "polygon") {
    #             tf <- array(startsWith(names(thisShape),"features.geometry.coordinates"))
    #             w <- 1
    #             pnts <- array()
    #             for (i in 1:nrow(tf)) {
    #                 if(tf[i] == TRUE) {
    #                     pnts[w] <- thisShape[i]
    #                     w <- w+1
    #                 }
    #             }
    #             n <- 1
    #             m <- 1
    #             plng <- array()
    #             plat <- array()
    #             pnts <- as.array(pnts)
    #             for (j in 1:nrow(pnts)) {
    #                 if(j %% 2 == 1) {
    #                     plng[n] <- pnts[j]
    #                     n <- n+1
    #                 }
    #                 else if(j %% 2 == 0) {
    #                     plat[m] <- pnts[j]
    #                     m <- m+1
    #                 }
    #             }
    #             as.vector(plng, mode = "any")
    #             as.vector(plat, mode = "any")
    #             PG <- data.frame(matrix(unlist(plng)))
    #             PGsub <- data.frame(matrix(unlist(plat)))
    #             PG <- cbind(PG, PGsub)
    #             colnames(PG) <- c("lng","lat")
    #             PG1 <- reactiveVal(PG)
    #             
    #             proxy <- leafletProxy("mymap", data = PG1())
    #             proxy %>% addPolygons(lng = ~lng, lat = ~lat, group = "draw")
    #         }
    #         #####
    #         else if(thisShape[3] == "rectangle"){
    #             rlng1 <- as.numeric(thisShape[5])
    #             rlat1 <- as.numeric(thisShape[6])
    #             rlng2 <- as.numeric(thisShape[9])
    #             rlat2 <- as.numeric(thisShape[10])
    #             
    #             proxy <- leafletProxy("mymap")
    #             proxy %>% addRectangles(lng1 = rlng1, lat1 = rlat1, lng2 = rlng2, lat2 = rlat2,
    #                                     group = "draw")
    #         }
    #         #####
    #         else if(thisShape[3] == "circle"){
    #             crad <- as.numeric(thisShape[4])
    #             clng <- as.numeric(thisShape[6])
    #             clat <- as.numeric(thisShape[7])
    #             
    #             proxy <- leafletProxy("mymap")
    #             proxy %>% addCircles(lng = clng, lat = clat, radius = crad, group = "draw")
    #         }
    #         #####
    #         else if(thisShape[3] == "marker") {
    #             mlng <- as.numeric(thisShape[5])
    #             mlat <- as.numeric(thisShape[6])
    #             
    #             proxy <- leafletProxy("mymap")
    #             proxy %>% addMarkers(lng = mlng, lat = mlat, group = "draw")
    #         }
    #         #####
    #         else if(thisShape[3] == "circlemarker") {
    #             cmlng <- as.numeric(thisShape[6])
    #             cmlat <- as.numeric(thisShape[7])
    #             
    #             proxy <- leafletProxy("mymap")
    #             proxy %>% addCircleMarkers(lng = cmlng, lat = cmlat, group = "draw")
    #         }
    #     }
    # })
}

shinyApp(ui = ui, server = server)
