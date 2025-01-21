print("clump_vector_big_files_last")
source("./scripts/clump_vector.R")
vects <- readRDS("./vect/clumped/global_clmp.rds")
rast <- raster("./rasters/original/original.tif")
clump_vector(vects, rast,"global")
unlink("./vect/clumped/temp/")
vects <- do.call(rbind,lapply(paste("./vect/clumped/temp/",
                                    list.files("./vect/clumped/temp/"),
                                    sep=""),readRDS))
saveRDS(vects,"./vect/clumped/global_clmp.rds")
unlink("./vect/clumped/temp/")

clump_vector_current <- "clumped_big_files_last"
save(clump_vector_current,file="./temp/clump_vector_current.RData")

restartSession(command=source("./scripts/clump_vector_finalise.R"))