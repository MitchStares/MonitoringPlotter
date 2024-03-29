##App Testing##
library(sf)
library(ggplot2)
poly <- read_sf("Drawings.geojson")

makeGrid <- function(data, gridsize = NA, within = FALSE){
  gridList <- list()
  for (i in 1:nrow(data)) {
  if (st_geometry_type(data[i, ]) == "MULTIPOLYGON" || 
      st_geometry_type(data[i, ]) == "POLYGON") {
    single <- data[i, ]
    bbox <- st_bbox(single)
    if(is.na(gridsize)){
      singleGrid <- st_make_grid(bbox)
    } else{
      singleGrid <- st_make_grid(bbox, cellsize = gridsize)
    }
    if(within ==  TRUE){
      singleGrid <- singleGrid[st_within(singleGrid, single) %>% lengths > 0]
    } else{
      singleGrid <- singleGrid[single]
    }
    singleGrid <- st_transform(singleGrid, crs = st_crs(data))
    gridList[[i]] <- singleGrid
  } 
  else{
    next
  }
}
  return(gridList)
}

grids <- makeGrid(poly)



selectPlots <- function(data, n = 10) {
  if (n > length(data)) {
    stop("Number of plots exceeds maximum possible plots in polygon")
  }
  plots <- list()
  for (i in 1:n) {
    plots[[i]] <- data[[sample(1:length(data), 1)]]
   }
    plots <- st_sfc(plots, crs = st_crs(data))
    return(plots)
}

selected <- selectPlots(grids[[4]], n=20)

ggplot()+
  geom_sf(data = poly, colour = "black", alpha = 0.2)+
  geom_sf(data = grids[[4]], colour = "red", alpha = 0.1)+
  geom_sf(data = selected, colour = "blue", alpha = 0.1)




