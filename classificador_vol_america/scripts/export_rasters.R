export_map <- function(m, plot_id, municipality, province){
  print("export_map")
  
  library(htmlwidgets)
  library(webshot)
  library(png)
  library(raster)
  dir <- paste("./classificador_vol_america/leaflet_temp/leaflet_",plot_id,"_temp", sep="")
  dir.create(dir)
  saveWidget(m, paste(dir,"/leaflet_map.html", sep=""), selfcontained = FALSE)
  webshot(paste(dir,"/leaflet_map.html", sep=""), file = paste(dir,"/leaflet_map.png", sep=""),
          cliprect = "viewport")
  img <- readPNG(paste(dir,"/leaflet_map.png", sep=""))
  
  img[img==1]=NA
  ar2mat <- matrix(img, nrow = nrow(img), ncol = ncol(img))
  ## Define the extent
  bbx <- getBox(m)
  rast = raster(ar2mat, xmn=bbx[1], xmx=bbx[2], ymn=bbx[3], ymx=bbx[4])
  ## Define the spatial reference system
  proj4string(rast) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  plot(rast)
  extent(rast)
  writeRaster(rast, paste("./classificador_vol_america/rasters/", plot_id, "-", province, "-", municipality, ".tif", sep=""), format="GTiff", overwrite=TRUE)
  unlink(dir, recursive = T)
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