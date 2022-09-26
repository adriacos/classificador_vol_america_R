export_map <- function(m, parcel_id, municipality, province){
  print("export_map")
  
  library(htmlwidgets)
  library(webshot)
  library(png)
  library(raster)
  
  saveWidget(m, "leaflet_map.html", selfcontained = FALSE)
  webshot("leaflet_map.html", file = "leaflet_map.png",
          cliprect = "viewport")
  
  img <- readPNG("leaflet_map.png")
  
  img[img==1]=NA
  ar2mat <- matrix(img, nrow = nrow(img), ncol = ncol(img))
  ## Define the extent
  bbx <- getBox(m)
  rast = raster(ar2mat, xmn=bbx[1], xmx=bbx[2], ymn=bbx[3], ymx=bbx[4])
  ## Define the spatial reference system
  proj4string(rast) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  plot(rast)
  extent(rast)
  writeRaster(rast, paste("./classificador_vol_america/rasters/", parcel_id, "-", province, "-", municipality, ".tif", sep=""), format="GTiff", overwrite=TRUE)
}


getBox <- function(m){
  view <- m$x$options
  lat <- view[[1]][1]
  lng <- view[[1]][2]
  zoom <- view[[2]]
  zoom_width <- 360 / 2^zoom
  lng_width <- m$width / 256 * zoom_width
  lat_height <- 360 * m$height * cos(lat/180 * pi) / 2^(zoom + 8)
  #lat_height <- m$height / 256 * zoom_width
  return(c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2))
}