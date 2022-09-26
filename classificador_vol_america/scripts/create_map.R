create_map <- function(lat, long){
	print("create_map")

	library(leaflet)
	library(leaflet.opacity)

	epsg4258 <- leafletCRS(crsClass="L.Proj.CRS", code="EPSG:4258", proj4def="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs", resolutions=1.5^(25:15))	

	driver = "ESRI Shapefile"
	df <- data.frame(lat=lat, long=long)
	m <- leaflet(
	 width = 992, height = 744,
	  options=list(center = c(lat, long), zoom = 16, zoomControl=FALSE, attributionControl=FALSE)
	)
	#m <- addTiles(m, layerId="base", group="base")

	m <- addWMSTiles(m, 
    		"https://www.ign.es/wms/pnoa-historico",
    		layers = "AMS_1956-1957",
		options = WMSTileOptions(crs=epsg4258, format="jpeg", transparent=TRUE),
	    	attribution = "Instituto Geogr?fico Nacional",
		layerId = "1956", 
		group = "1956",
	)
  
	return(m)
}

#, maxBounds = list(list(1, -180), list(78, 180))