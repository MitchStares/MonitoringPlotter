##App Testing##
library(sf)
library(ggplot2)
poly <- read_sf("Drawings.geojson")

makeGrid <- function(data, gridsize = NA){
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
    singleGrid <- singleGrid[single]
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
    # if (!st_within(plots[[i]], data)) {
    #   
   }
    plots <- st_sfc(plots, crs = st_crs(data))
    return(plots)
  }
selected <- selectPlots(grids[[4]], n=20)

plots <- list()
plots[[1]] <- grids[[4]][[sample(1:length(grids[[4]]),1)]]
plots <- st_sfc(plots, crs = st_crs(poly))
st_within(plots[[1]], poly[4,])


ggplot()+
  geom_sf(data = poly, colour = "black", alpha = 0.2)+
  geom_sf(data = grids[[4]], colour = "red", alpha = 0.1)+
  geom_sf(data = selected, colour = "blue", alpha = 0.1)+
  geom_sf(data = plots, fill = "red")

