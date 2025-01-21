
create_1956_class_diba_leaflet <- function(lat, long){
  print("classificador_vol_america/scripts/create_1956_class_diba_leaflet.R")
  
  lat <- as.numeric(lat)
  long <- as.numeric(long)
  
  library(leaflet)
  library(leaflet.opacity)
  
  #epsg4258 <- leafletCRS(crsClass="L.Proj.CRS", code="EPSG:4258", proj4def="+proj=longlat +ellps=GRS80 +no_defs +type=crs", resolutions=1.5^(25:15))	
  epsg25831 <- leafletCRS(crsClass="L.Proj.CRS", code="EPSG:25831", proj4def="+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", resolutions=1.5^(25:15))	
  epgs4326 <- leafletCRS(crsClass="L.CRS.EPSG4326", code="EPSG:4326", proj4def="+proj=longlat +datum=WGS84 +no_defs +type=crs", resolutions=1.5^(25:15))	
    
  driver = "ESRI Shapefile"
  df <- data.frame(lat=lat, long=long)
  m <- leaflet(
    width = 992, height = 744,
    options=list(center = c(lat, long), zoom = 16, zoomControl=FALSE, attributionControl=FALSE)
  )
  m <- addWMSTiles(m, 
                   "http://sitmun.diba.cat/wms/servlet/CSA56",
                   layers = "CSA56",
                   options = WMSTileOptions(crs=epsg25831, format="jpeg", transparent=TRUE),
                   attribution = "Diputació de Barcelonal",
                   layerId = "1956", 
                   group = "1956",
  )
  
  return(m)
}