create_map <- function(lat, long){
	
#	if (!require("leaflet")){
#		install.packages("leaflet")
#	}
#	if (!require("leaflet.opacity")){
#		install.packages("leaflet.opacity")	
#	}
	library(leaflet)
	library(leaflet.opacity)

	epsg4258 <- leafletCRS(crsClass="L.Proj.CRS", code="EPSG:4258", proj4def="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs", resolutions=1.5^(25:15))	

	#lat = 41.8817 writeOGR(map, ".", "filename", 
	driver = "ESRI Shapefile")
	#long = 2.90968
	df <- data.frame(lat=lat, long=long)
	m <- leaflet()
	m <- addTiles(m, layerId="base", group="base")
	m <- addWMSTiles(m, 
    		"https://www.ign.es/wms-inspire/pnoa-ma",
    		layers = "OI.OrthoimageCoverage",
		options = WMSTileOptions(format = "image/png", transparent = TRUE),
	    	attribution = "Instituto Geogr?fico Nacional",
		layerId = "ortofoto", 
		group = "ortofoto",
	)

	m <- addWMSTiles(m, 
    		"https://www.ign.es/wms/pnoa-historico",
    		layers = "AMS_1956-1957",
		options = WMSTileOptions(crs=epsg4258, format="jpeg", transparent=TRUE),
	    	attribution = "Instituto Geogr?fico Nacional",
		layerId = "1956", 
		group = "1956",
	)
	
	m <- addCircles(m, data=df, radius = 25, fillColor = "transparent", color = "orange", weight=1.5)
	m <- addCircles(m, data=df, radius = 1000, fillColor = "transparent", color = "red", weight=1.5)
	#m <- addCircles(m, data=df, radius = 3000, fillColor = "transparent", color = "red", weight=1.5)
	#m <- addOpacitySlider(m, layerId="1956")

	export_map(m)
	
	m <- addLayersControl(m, 
    		baseGroups = c("1956", "ortofoto", "base"),
    		#overlayGroups = c("Quakes", "Outline"),
    		options = layersControlOptions(collapsed = FALSE)
  )
	return(m)
}

export_map <- function(m){
  
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
  rast = raster(ar2mat, xmn=0, xmx=1, ymn=0, ymx=1)
  ## Define the spatial reference system
  proj4string(rast) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  plot(rast); extent(rast)
  writeRaster(rast, "raster.tif", format="GTiff", overwrite=TRUE)
  
  
  #rc <- clump(rast)
  
}