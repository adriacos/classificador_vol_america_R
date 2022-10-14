
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/create_ortofoto_leaflet.R")
source("./classificador_vol_america/scripts/export_rasters.R")


create_export_ortofoto_raster <- function(plot_id, lat, long, municipality, province){
  print("create_export_map")
  print(plot_id)
  map <- create_ortofoto_leaflet(lat, long)
  export_map(map, plot_id, municipality, province)
}