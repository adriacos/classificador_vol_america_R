print("clump_vector_big_files_last")
source("./classificador_vol_america/scripts/clump_vector.R")
vects <- readRDS("./classificador_vol_america/vect/clumped/global_clmp.rds")
rast <- raster("./classificador_vol_america/rasters/original/original.tif")
clump_vector(vects, rast,"global")
unlink("./classificador_vol_america/vect/clumped/temp/")
vects <- do.call(rbind,lapply(paste("./classificador_vol_america/vect/clumped/temp/",
                                    list.files("./classificador_vol_america/vect/clumped/temp/"),
                                    sep=""),readRDS))
saveRDS(vects,"./classificador_vol_america/vect/clumped/global_clmp.rds")
unlink("./classificador_vol_america/vect/clumped/temp/")

clump_vector_current <- "clumped_big_files_last"
save(clump_vector_current,file="./classificador_vol_america/temp/clump_vector_current.RData")

restartSession(command=source("./classificador_vol_america/scripts/clump_vector_finalise.R"))